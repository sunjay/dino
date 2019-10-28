use crate::ir;
use crate::resolve::{TyId, DeclMap};
use crate::codegen::*;

use super::Error;
use super::mangler::NameMangler;

/// Choices for what to do with the result of a block
#[derive(Debug, Clone)]
enum BlockBehaviour {
    /// Return the result of the block using a `return` statement
    Return,
    /// Ignore the result, doing nothing with it
    Ignore,
    /// Store the result in the given variable
    StoreVar {
        mangled_name: String,
    },
}

pub struct FunctionCodeGenerator<'a> {
    mod_scope: &'a DeclMap<'a>,
    mangler: NameMangler,
}

impl<'a> FunctionCodeGenerator<'a> {
    pub fn generate(func: &ir::Function, mod_scope: &'a DeclMap<'a>) -> Result<CFunction, Error> {
        // Each function body should have a single name mangler
        let mangler = NameMangler::new();
        let mut generator = Self {mod_scope, mangler};
        generator.gen_function(func)
    }

    fn gen_function(&mut self, func: &ir::Function) -> Result<CFunction, Error> {
        let ir::Function {name, sig, body} = func;

        let ir::FuncSig {return_type, params} = sig;
        // Add each parameter to the mangler so it can be used from within the function body
        let cparams = params.iter().map(|ir::FuncParam {name, ty}| CFunctionParam {
            mangled_name: self.mangler.mangle_name(name).to_string(),
            ty: self.lookup_type_name(ty),
        }).collect();

        let sig = CFunctionSignature {
            //TODO: Mangle function names
            mangled_name: name.to_string(),
            return_type: self.lookup_type_name(return_type),
            params: cparams,
        };

        let body = self.gen_block(body, BlockBehaviour::Return)?;

        Ok(CFunction {sig, body})
    }

    fn gen_block(
        &mut self,
        block: &ir::Block,
        behaviour: BlockBehaviour,
    ) -> Result<CStmts, Error> {
        let ir::Block {stmts, ret, ret_ty} = block;

        let mut cstmts = Vec::new();

        // Statements must be traversed in order for our name mangling mechanism to work
        for stmt in stmts {
            let gen_stmt = match stmt {
                ir::Stmt::Cond(cond) => {
                    // Conditionals in statement position do not return a value from the block. They
                    // also do not cause a `return` unless there is an explicit return statement in
                    // one of the conditional branch bodies. Thus, the only remaining alternative is
                    // to ignore the result of each block. These conditionals type check to unit
                    // anyway, so we are just ignoring the unit value.

                    CStmt::Cond(self.gen_cond_stmt(cond, &mut cstmts, BlockBehaviour::Ignore)?)
                },
                ir::Stmt::WhileLoop(wloop) => {
                    CStmt::WhileLoop(self.gen_while_loop(wloop, &mut cstmts)?)
                },
                ir::Stmt::VarDecl(var_decl) => {
                    CStmt::VarDecl(self.gen_var_decl(var_decl, &mut cstmts)?)
                },
                ir::Stmt::Expr(expr) => {
                    CStmt::Expr(self.gen_expr(expr, &mut cstmts)?)
                },
            };
            cstmts.push(gen_stmt);
        }

        let ret_expr = ret.as_ref()
            .map(|ret| self.gen_expr(ret, &mut cstmts).map(|val| (ret.ty_id(), val)))
            .transpose()?;

        let last_stmt_expr = match ret_expr {
            Some((ret_expr_ty, ret_expr)) => {
                debug_assert_eq!(*ret_ty, ret_expr_ty,
                    "bug: return expression of block had different type than the block itself");

                ret_expr
            },
            // Produce unit if no return expression
            None => self.gen_unit_literal(*ret_ty)?,
        };

        cstmts.push(match behaviour {
            BlockBehaviour::Return => CStmt::Return(last_stmt_expr),
            BlockBehaviour::Ignore => CStmt::Expr(last_stmt_expr),
            BlockBehaviour::StoreVar {mangled_name} => CStmt::VarAssign(CVarAssign {
                    mangled_name,
                    init_expr: CInitializerExpr::Expr(last_stmt_expr),
            }),
        });

        Ok(CStmts(cstmts))
    }

    fn gen_cond_stmt(
        &mut self,
        cond: &ir::Cond,
        prev_stmts: &mut Vec<CStmt>,
        block_behaviour: BlockBehaviour,
    ) -> Result<CCond, Error> {
        let ir::Cond {conds, else_body} = cond;

        // Note that there is no need to generate a new mangler for each block because C does not
        // support variable shadowing. A single scope is sufficient.

        // This loop takes any else-if expressions and nests them in else clauses:
        //
        // So this code:
        //
        //     if (...) {
        //         body1
        //     } else if (...) {
        //         body2
        //     } else if (...) {
        //         body3
        //     } else {
        //         body4
        //     }
        //
        // Becomes this code:
        //
        //     if (...) {
        //         body1
        //     } else {
        //         if (...) {
        //             body2
        //         } else {
        //             if (...) {
        //                 body3
        //             } else {
        //                 body4
        //             }
        //         }
        //     }
        //
        // Notice that any temporary variables needed for an else-if condition can now go just above
        // the corresponding if statement.

        // It guaranteed that there is at least one condition
        let (cond, body) = conds.first().expect("bug: codegen received empty conditional");
        let cond_expr = self.gen_expr(cond, prev_stmts)?;
        let if_body = self.gen_block(body, block_behaviour.clone())?;

        // Put any else-ifs into the else body
        let mut else_body = else_body.as_ref()
            .map(|else_body| self.gen_block(else_body, block_behaviour.clone()))
            .transpose()?;

        // Special-case: if we have been asked to assign the result to a variable, we need to do so in
        // every branch of the conditional, otherwise that variable is left uninitialized.
        if else_body.is_none() {
            if let BlockBehaviour::StoreVar {..} = &block_behaviour {
                // If we have no else and we're storing to a variable, the type checker ensures that
                // we must be assigning to a variable of type unit. That means that it is sufficient
                // to generate an empty else block and generate code for that.
                let empty_block = ir::Block {
                    stmts: Vec::new(),
                    ret: None,
                    // Take the return type from the if-block
                    ret_ty: body.ret_ty,
                };
                else_body = Some(self.gen_block(&empty_block, block_behaviour.clone())
                    .expect("bug: failed to generate empty else block"));
            }
        }

        // Traverses in reverse order so it collects the else clause, then uses the last else if as the
        // else clause, and so on until it gets back to the end of the if.
        for (cond, body) in conds.iter().skip(1).rev() {
            // The statements containing the entire else-if expression
            // This becomes the body of the else for either the previous else-if or the top-level if.
            let mut else_if_stmts = Vec::new();

            let cond_expr = self.gen_expr(cond, &mut else_if_stmts)?;
            let if_body = self.gen_block(body, block_behaviour.clone())?;

            else_if_stmts.push(CStmt::Cond(CCond {cond_expr, if_body, else_body}));
            else_body = Some(CStmts(else_if_stmts));
        }

        Ok(CCond {cond_expr, if_body, else_body})
    }

    fn gen_while_loop(
        &mut self,
        wloop: &ir::WhileLoop,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CWhileLoop, Error> {
        let ir::WhileLoop {cond, body} = wloop;

        let cond = self.gen_expr(cond, prev_stmts)?;
        // Ignore the result of the body because it is currently guaranteed to be unit. We don't
        // support returning values from loops yet.
        let body = self.gen_block(body, BlockBehaviour::Ignore)?;

        Ok(CWhileLoop {cond, body})
    }

    fn gen_var_decl(
        &mut self,
        var_decl: &ir::VarDecl,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CVarDecl, Error> {
        let ir::VarDecl {ident, ty, expr} = var_decl;

        Ok(CVarDecl {
            mangled_name: self.mangler.mangle_name(ident).to_string(),
            ty: self.lookup_type_name(ty),
            init_expr: CInitializerExpr::Expr(self.gen_expr(expr, prev_stmts)?)
        })
    }

    /// Generates code for an expression. In the process of doing this, we may need to append
    /// additional statements that are necessary for this expression to be evaluated.
    fn gen_expr(
        &mut self,
        expr: &ir::Expr,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CExpr, Error> {
        Ok(match expr {
            ir::Expr::VarAssign(assign, ty) => self.gen_var_assign(assign, *ty, prev_stmts)?,
            ir::Expr::FieldAccess(access, ty) => self.gen_field_access(access, *ty, prev_stmts)?,
            ir::Expr::Cond(cond, ty) => self.gen_cond_expr(cond, ty, prev_stmts)?,
            ir::Expr::Call(call, _) => CExpr::Call(self.gen_call_expr(call, prev_stmts)?),
            ir::Expr::Return(ret_expr, ty) => self.gen_return(ret_expr.as_ref().map(|x| x.as_ref()), *ty, prev_stmts)?,
            ir::Expr::StructLiteral(struct_lit, ty) => self.gen_struct_literal(struct_lit, *ty, prev_stmts)?,
            &ir::Expr::BStrLiteral(value, ty) => self.gen_bstr_literal(value, ty)?,
            &ir::Expr::IntegerLiteral(value, ty) => self.gen_int_literal(value, ty)?,
            &ir::Expr::RealLiteral(value, ty) => self.gen_real_literal(value, ty)?,
            &ir::Expr::ComplexLiteral(value, ty) => self.gen_complex_literal(value, ty)?,
            &ir::Expr::BoolLiteral(value, ty) => self.gen_bool_literal(value, ty)?,
            &ir::Expr::UnitLiteral(ty) => self.gen_unit_literal(ty)?,
            &ir::Expr::Var(name, _) => CExpr::Var(self.mangler.get(name).to_string()),
        })
    }

    /// Generates an expression (and series of statements) that evaluate a conditional that was used
    /// in expression position. C doesn't support this, so we need to generate a temporary value and
    /// evaluate the expression beforehand.
    fn gen_cond_expr(
        &mut self,
        cond: &ir::Cond,
        ret_ty: &TyId,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CExpr, Error> {
        // Generate a temporary uninitialized variable to store the result of the conditional
        let result_var_mangled_name = self.mangler.fresh_mangled_name();
        prev_stmts.push(CStmt::TempVarDecl(CTempVarDecl {
            mangled_name: result_var_mangled_name.clone(),
            ty: self.lookup_type_name(ret_ty),
            init_expr: None,
        }));

        // Generate the conditional, putting the result of all the blocks into the temporary variable
        let block_behaviour = BlockBehaviour::StoreVar {mangled_name: result_var_mangled_name.clone()};
        let cond = self.gen_cond_stmt(cond, prev_stmts, block_behaviour)?;
        prev_stmts.push(CStmt::Cond(cond));

        // Now that the variable has been assigned as part of the if, produce the variable that
        // contains the result
        Ok(CExpr::Var(result_var_mangled_name))
    }

    fn gen_call_expr(
        &mut self,
        expr: &ir::CallExpr,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CCallExpr, Error> {
        let ir::CallExpr {func_name, args} = expr;

        //TODO: In order to preserve execution order, calls should be lifted into a temporary variable
        // and the expression returned from here should be a CExpr::Var(temp_var)
        Ok(CCallExpr {
            //TODO: Mangle function names
            mangled_func_name: func_name.to_string(),
            args: args.iter()
                .map(|expr| self.gen_expr(expr, prev_stmts))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn gen_var_assign(
        &mut self,
        assign: &ir::VarAssign,
        ty: TyId,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CExpr, Error> {
        let ir::VarAssign {lhs, expr} = assign;

        // C doesn't support assignment in expression position, so the assignment must be lifted
        // into a statement
        match lhs {
            ir::LValueExpr::FieldAccess(access, _) => unimplemented!(),

            ir::LValueExpr::Var(ident, _) => {
                let assign = CStmt::VarAssign(CVarAssign {
                    mangled_name: self.mangler.get(ident).to_string(),
                    init_expr: CInitializerExpr::Expr(self.gen_expr(expr, prev_stmts)?),
                });
                prev_stmts.push(assign);
            },
        }

        // We can then produce a unit value since that is always the result of an assignment
        self.gen_unit_literal(ty)
    }

    fn gen_field_access(
        &mut self,
        access: &ir::FieldAccess,
        ty: TyId,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CExpr, Error> {
        let ir::FieldAccess {lhs, field} = access;

        unimplemented!()
    }

    fn gen_return(
        &mut self,
        ret_expr: Option<&ir::Expr>,
        ty: TyId,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CExpr, Error> {
        // C doesn't support return in expression position, so the return must be lifted into
        // a statement
        let assign = CStmt::Return(match ret_expr {
            Some(ret_expr) => self.gen_expr(ret_expr, prev_stmts)?,
            // If no return expression is provided, we must be returning unit
            None => self.gen_unit_literal(ty)?,
        });
        prev_stmts.push(assign);

        // We can then produce a unit value since that is always the result of a return expression
        self.gen_unit_literal(ty)
    }

    fn gen_struct_literal(
        &mut self,
        struct_lit: &ir::StructLiteral,
        ty: TyId,
        prev_stmts: &mut Vec<CStmt>,
    ) -> Result<CExpr, Error> {
        let ir::StructLiteral {ty_id, field_values} = struct_lit;

        // The struct literal must be generated in a separate temporary variable because we need to
        // initialize each field

        unimplemented!()
    }

    fn gen_bstr_literal(
        &self,
        value: &[u8],
        ty: TyId,
    ) -> Result<CExpr, Error> {
        let lit_constructors = self.mod_scope.type_lit_constructors(ty);
        Ok(CExpr::Call(CCallExpr {
            //TODO: Mangle function names
            mangled_func_name: lit_constructors.bstr_literal_constructor
                .as_ref()
                .expect("bug: no byte string literal constructor defined for type that type checked to bstr")
                .to_string(),
            args: vec![
                CExpr::NTStrLiteral(value.to_vec()),
                CExpr::IntegerLiteral(value.len() as i64),
            ],
        }))
    }

    fn gen_int_literal(
        &self,
        value: i64,
        ty: TyId,
    ) -> Result<CExpr, Error> {
        let lit_constructors = self.mod_scope.type_lit_constructors(ty);
        Ok(CExpr::Call(CCallExpr {
            //TODO: Mangle function names
            mangled_func_name: lit_constructors.int_literal_constructor
                .as_ref()
                .expect("bug: no integer literal constructor defined for type that type checked to int")
                .to_string(),
            args: vec![CExpr::IntegerLiteral(value)],
        }))
    }

    fn gen_real_literal(
        &self,
        value: f64,
        ty: TyId,
    ) -> Result<CExpr, Error> {
        let lit_constructors = self.mod_scope.type_lit_constructors(ty);
        Ok(CExpr::Call(CCallExpr {
            //TODO: Mangle function names
            mangled_func_name: lit_constructors.real_literal_constructor
                .as_ref()
                .expect("bug: no real literal constructor defined for type that type checked to real")
                .to_string(),
            args: vec![CExpr::DoubleLiteral(value)],
        }))
    }

    fn gen_complex_literal(
        &self,
        value: f64,
        ty: TyId,
    ) -> Result<CExpr, Error> {
        let lit_constructors = self.mod_scope.type_lit_constructors(ty);
        Ok(CExpr::Call(CCallExpr {
            //TODO: Mangle function names
            mangled_func_name: lit_constructors.complex_literal_constructor
                .as_ref()
                .expect("bug: no complex literal constructor defined for type that type checked to complex")
                .to_string(),
            args: vec![CExpr::DoubleLiteral(value)],
        }))
    }

    fn gen_bool_literal(
        &self,
        value: bool,
        ty: TyId,
    ) -> Result<CExpr, Error> {
        let lit_constructors = self.mod_scope.type_lit_constructors(ty);
        Ok(CExpr::Call(CCallExpr {
            //TODO: Mangle function names
            mangled_func_name: lit_constructors.bool_literal_constructor
                .as_ref()
                .expect("bug: no bool literal constructor defined for type that type checked to bool")
                .to_string(),
            args: vec![CExpr::BoolLiteral(value)],
        }))
    }

    /// Unit literals are zero-sized and thus can be spawned arbitrarily and out of nowhere
    fn gen_unit_literal(
        &self,
        ty: TyId,
    ) -> Result<CExpr, Error> {
        let lit_constructors = self.mod_scope.type_lit_constructors(ty);
        Ok(CExpr::Call(CCallExpr {
            //TODO: Mangle function names
            mangled_func_name: lit_constructors.unit_literal_constructor
                .as_ref()
                .expect("bug: no unit literal constructor defined for type that type checked to ()")
                .to_string(),
            args: Vec::new(),
        }))
    }

    fn lookup_type_name(
        &self,
        &ty: &TyId,
    ) -> String {
        let name = *self.mod_scope.type_name(ty);
        //TODO: Mangle generated struct names
        name.to_string()
    }
}
