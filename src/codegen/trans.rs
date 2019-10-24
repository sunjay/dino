//! Translates IR into generated code

use super::*;

use std::collections::HashMap;

use snafu::Snafu;
use rand::{Rng, SeedableRng, rngs::SmallRng};

use crate::ir;
use crate::resolve::{TyId, ProgramDecls, DeclMap};

/// Represents a single level of local scope and maps the names of variables to their mangled
/// equivalent
struct NameMangler {
    rng: SmallRng,
    mangled_names: HashMap<String, String>,
    next_fresh_name: u64,
}

impl NameMangler {
    pub fn new() -> Self {
        Self {
            // Want names to be deterministic across builds
            rng: SmallRng::seed_from_u64(2194920),
            mangled_names: HashMap::new(),
            next_fresh_name: 0,
        }
    }

    /// Mangles the given name, overwriting any mangled name previously stored for the same name
    pub fn mangle_name(&mut self, name: &str) -> &str {
        // Append some random bytes to the end of the name to differentiate this name from any
        // other shadowed variables with the same name
        //TODO: Base the random characters off of the name so they aren't the same in every function
        //TODO: Make it impossible for there to be collisions (currently collisions are unlikely,
        // but still entirely possible)
        let mut mangled_name = name.to_string();
        mangled_name.reserve_exact(9);
        mangled_name.push('_');
        for _ in 0..8 {
            mangled_name.push(self.rng.gen_range(b'a', b'z') as char);
        }

        self.mangled_names.insert(name.to_string(), mangled_name);
        self.get(name)
    }

    /// Generates a fresh mangled name that is not associated with any program variable name
    pub fn fresh_mangled_name(&mut self) -> String {
        //TODO: Actually ensure that this name is unique
        let mut mangled_name = format!("var{}_", self.next_fresh_name);
        self.next_fresh_name += 1;
        mangled_name.reserve_exact(8);
        for _ in 0..8 {
            mangled_name.push(self.rng.gen_range(b'a', b'z') as char);
        }

        mangled_name
    }

    /// Returns the mangled name of the given name or panics
    pub fn get(&self, name: &str) -> &str {
        self.mangled_names.get(name).expect("bug: unresolved name was allowed to get to codegen")
    }
}

/// Code generation errors
#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("`main` function not found"))]
    NoEntryPoint,
    #[snafu(display("`main` function has wrong type"))]
    InvalidEntryPointType,
}

/// Generates an executable program from the given IR
pub fn executable(prog: &ir::Program, program_scope: &ProgramDecls) -> Result<CExecutableProgram, Error> {
    let ir::Program {top_level_module} = prog;
    let ir::Module {decls} = top_level_module;

    let ProgramDecls {top_level_decls: mod_scope, prims} = program_scope;

    let mut entry_point = None;
    let mut functions = Vec::new();

    for decl in decls {
        match decl {
            // A "main" function in the top level declarations of a program must be the entry point
            ir::Decl::Function(func) if func.name == "main" => {
                let ir::Function {sig, ..} = func;
                // The main function must have no return type and no arguments
                if sig.return_type != prims.unit() || !sig.params.is_empty() {
                    return Err(Error::InvalidEntryPointType);
                }

                // Note that it is guaranteed that `entry_point` will only be assigned once since
                // the IR assumes that all declaration names have been checked to be unique within
                // a given module.
                debug_assert!(entry_point.is_none(), "bug: allowed multiple entry points");

                // Take the generated body and put it in the right struct
                let CFunction {sig: _, body} = gen_function(func, mod_scope)?;
                entry_point = Some(CEntryPoint {body});
            },

            ir::Decl::Function(func) => functions.push(gen_function(func, mod_scope)?),
        }
    }

    let entry_point = match entry_point {
        Some(entry_point) => entry_point,
        None => return Err(Error::NoEntryPoint),
    };

    Ok(CExecutableProgram {functions, entry_point})
}

fn gen_function(
    func: &ir::Function,
    mod_scope: &DeclMap,
) -> Result<CFunction, Error> {
    let ir::Function {name, sig, body} = func;

    // Each function body should have a single name mangler
    let mut mangler = NameMangler::new();

    let ir::FuncSig {return_type, params} = sig;
    // Add each parameter to the mangler so it can be used from within the function body
    let cparams = params.iter().map(|ir::FuncParam {name, ty}| CFunctionParam {
        mangled_name: mangler.mangle_name(name).to_string(),
        ty: lookup_type_name(ty, &mangler, mod_scope),
    }).collect();

    let sig = CFunctionSignature {
        //TODO: Mangle function names
        mangled_name: name.to_string(),
        return_type: lookup_type_name(return_type, &mangler, mod_scope),
        params: cparams,
    };

    let body = gen_block(body, BlockBehaviour::Return, &mut mangler, mod_scope)?;

    Ok(CFunction {sig, body})
}

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

fn gen_block(
    block: &ir::Block,
    behaviour: BlockBehaviour,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
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

                CStmt::Cond(gen_cond_stmt(cond, &mut cstmts, BlockBehaviour::Ignore, mangler, mod_scope)?)
            },
            ir::Stmt::WhileLoop(wloop) => {
                CStmt::WhileLoop(gen_while_loop(wloop, &mut cstmts, mangler, mod_scope)?)
            },
            ir::Stmt::VarDecl(var_decl) => {
                CStmt::VarDecl(gen_var_decl(var_decl, &mut cstmts, mangler, mod_scope)?)
            },
            ir::Stmt::Expr(expr) => {
                CStmt::Expr(gen_expr(expr, &mut cstmts, mangler, mod_scope)?)
            },
        };
        cstmts.push(gen_stmt);
    }

    let ret_expr = ret.as_ref()
        .map(|ret| gen_expr(ret, &mut cstmts, mangler, mod_scope).map(|val| (ret.ty_id(), val)))
        .transpose()?;

    let last_stmt_expr = match ret_expr {
        Some((ret_expr_ty, ret_expr)) => {
            debug_assert_eq!(*ret_ty, ret_expr_ty,
                "bug: return expression of block had different type than the block itself");

            ret_expr
        },
        // Produce unit if no return expression
        None => gen_unit_literal(*ret_ty, mangler, mod_scope)?,
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
    cond: &ir::Cond,
    prev_stmts: &mut Vec<CStmt>,
    block_behaviour: BlockBehaviour,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
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
    let cond_expr = gen_expr(cond, prev_stmts, mangler, mod_scope)?;
    let if_body = gen_block(body, block_behaviour.clone(), mangler, mod_scope)?;

    // Put any else-ifs into the else body
    let mut else_body = else_body.as_ref()
        .map(|else_body| gen_block(else_body, block_behaviour.clone(), mangler, mod_scope))
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
            else_body = Some(gen_block(&empty_block, block_behaviour.clone(), mangler, mod_scope)
                .expect("bug: failed to generate empty else block"));
        }
    }

    // Traverses in reverse order so it collects the else clause, then uses the last else if as the
    // else clause, and so on until it gets back to the end of the if.
    for (cond, body) in conds.iter().skip(1).rev() {
        // The statements containing the entire else-if expression
        // This becomes the body of the else for either the previous else-if or the top-level if.
        let mut else_if_stmts = Vec::new();

        let cond_expr = gen_expr(cond, &mut else_if_stmts, mangler, mod_scope)?;
        let if_body = gen_block(body, block_behaviour.clone(), mangler, mod_scope)?;

        else_if_stmts.push(CStmt::Cond(CCond {cond_expr, if_body, else_body}));
        else_body = Some(CStmts(else_if_stmts));
    }

    Ok(CCond {cond_expr, if_body, else_body})
}

fn gen_while_loop(
    wloop: &ir::WhileLoop,
    prev_stmts: &mut Vec<CStmt>,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CWhileLoop, Error> {
    let ir::WhileLoop {cond, body} = wloop;

    let cond = gen_expr(cond, prev_stmts, mangler, mod_scope)?;
    // Ignore the result of the body because it is currently guaranteed to be unit. We don't
    // support returning values from loops yet.
    let body = gen_block(body, BlockBehaviour::Ignore, mangler, mod_scope)?;

    Ok(CWhileLoop {cond, body})
}

fn gen_var_decl(
    var_decl: &ir::VarDecl,
    prev_stmts: &mut Vec<CStmt>,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CVarDecl, Error> {
    let ir::VarDecl {ident, ty, expr} = var_decl;

    Ok(CVarDecl {
        mangled_name: mangler.mangle_name(ident).to_string(),
        ty: lookup_type_name(ty, mangler, mod_scope),
        init_expr: CInitializerExpr::Expr(gen_expr(expr, prev_stmts, mangler, mod_scope)?)
    })
}

/// Generates code for an expression. In the process of doing this, we may need to append
/// additional statements that are necessary for this expression to be evaluated.
fn gen_expr(
    expr: &ir::Expr,
    prev_stmts: &mut Vec<CStmt>,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    Ok(match expr {
        ir::Expr::VarAssign(assign, ty) => gen_var_assign(assign, *ty, prev_stmts, mangler, mod_scope)?,
        ir::Expr::FieldAccess(access, ty) => gen_field_access(access, *ty, prev_stmts, mangler, mod_scope)?,
        ir::Expr::Cond(cond, ty) => gen_cond_expr(cond, ty, prev_stmts, mangler, mod_scope)?,
        ir::Expr::Call(call, _) => CExpr::Call(gen_call_expr(call, prev_stmts, mangler, mod_scope)?),
        ir::Expr::Return(ret_expr, ty) => gen_return(ret_expr.as_ref().map(|x| x.as_ref()), *ty, prev_stmts, mangler, mod_scope)?,
        &ir::Expr::BStrLiteral(value, ty) => gen_bstr_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::IntegerLiteral(value, ty) => gen_int_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::RealLiteral(value, ty) => gen_real_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::ComplexLiteral(value, ty) => gen_complex_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::BoolLiteral(value, ty) => gen_bool_literal(value, ty, mangler, mod_scope)?,
        &ir::Expr::UnitLiteral(ty) => gen_unit_literal(ty, mangler, mod_scope)?,
        &ir::Expr::Var(name, _) => CExpr::Var(mangler.get(name).to_string()),
    })
}

/// Generates an expression (and series of statements) that evaluate a conditional that was used
/// in expression position. C doesn't support this, so we need to generate a temporary value and
/// evaluate the expression beforehand.
fn gen_cond_expr(
    cond: &ir::Cond,
    ret_ty: &TyId,
    prev_stmts: &mut Vec<CStmt>,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    // Generate a temporary uninitialized variable to store the result of the conditional
    let result_var_mangled_name = mangler.fresh_mangled_name();
    prev_stmts.push(CStmt::TempVarDecl(CTempVarDecl {
        mangled_name: result_var_mangled_name.clone(),
        ty: lookup_type_name(ret_ty, mangler, mod_scope),
        init_expr: None,
    }));

    // Generate the conditional, putting the result of all the blocks into the temporary variable
    let block_behaviour = BlockBehaviour::StoreVar {mangled_name: result_var_mangled_name.clone()};
    let cond = gen_cond_stmt(cond, prev_stmts, block_behaviour, mangler, mod_scope)?;
    prev_stmts.push(CStmt::Cond(cond));

    // Now that the variable has been assigned as part of the if, produce the variable that
    // contains the result
    Ok(CExpr::Var(result_var_mangled_name))
}

fn gen_call_expr(
    expr: &ir::CallExpr,
    prev_stmts: &mut Vec<CStmt>,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CCallExpr, Error> {
    let ir::CallExpr {func_name, args} = expr;

    //TODO: In order to preserve execution order, calls should be lifted into a temporary variable
    // and the expression returned from here should be a CExpr::Var(temp_var)
    Ok(CCallExpr {
        //TODO: Mangle function names
        mangled_func_name: func_name.to_string(),
        args: args.iter()
            .map(|expr| gen_expr(expr, prev_stmts, mangler, mod_scope))
            .collect::<Result<Vec<_>, _>>()?,
    })
}

fn gen_var_assign(
    assign: &ir::VarAssign,
    ty: TyId,
    prev_stmts: &mut Vec<CStmt>,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let ir::VarAssign {ident, expr} = assign;

    // C doesn't support assignment in expression position, so the assignment must be lifted into
    // a statement
    let assign = CStmt::VarAssign(CVarAssign {
        mangled_name: mangler.get(ident).to_string(),
        init_expr: CInitializerExpr::Expr(gen_expr(expr, prev_stmts, mangler, mod_scope)?),
    });
    prev_stmts.push(assign);

    // We can then produce a unit value since that is always the result of an assignment
    gen_unit_literal(ty, mangler, mod_scope)
}

fn gen_field_access(
    access: &ir::FieldAccess,
    ty: TyId,
    prev_stmts: &mut Vec<CStmt>,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let ir::FieldAccess {lhs, field} = access;

    unimplemented!()
}

fn gen_return(
    ret_expr: Option<&ir::Expr>,
    ty: TyId,
    prev_stmts: &mut Vec<CStmt>,
    mangler: &mut NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    // C doesn't support return in expression position, so the return must be lifted into
    // a statement
    let assign = CStmt::Return(match ret_expr {
        Some(ret_expr) => gen_expr(ret_expr, prev_stmts, mangler, mod_scope)?,
        // If no return expression is provided, we must be returning unit
        None => gen_unit_literal(ty, mangler, mod_scope)?,
    });
    prev_stmts.push(assign);

    // We can then produce a unit value since that is always the result of a return expression
    gen_unit_literal(ty, mangler, mod_scope)
}

fn gen_bstr_literal(
    value: &[u8],
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let lit_constructors = mod_scope.type_lit_constructors(ty);
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
    value: i64,
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let lit_constructors = mod_scope.type_lit_constructors(ty);
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
    value: f64,
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let lit_constructors = mod_scope.type_lit_constructors(ty);
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
    value: f64,
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let lit_constructors = mod_scope.type_lit_constructors(ty);
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
    value: bool,
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let lit_constructors = mod_scope.type_lit_constructors(ty);
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
    ty: TyId,
    _mangler: &NameMangler,
    mod_scope: &DeclMap,
) -> Result<CExpr, Error> {
    let lit_constructors = mod_scope.type_lit_constructors(ty);
    Ok(CExpr::Call(CCallExpr {
        //TODO: Mangle function names
        mangled_func_name: lit_constructors.unit_literal_constructor
            .as_ref()
            .expect("bug: no unit literal constructor defined for type that type checked to ()")
            .to_string(),
        args: Vec::new(),
    }))
}

fn lookup_type_name<'a>(
    &ty: &TyId,
    mangler: &NameMangler,
    mod_scope: &'a DeclMap,
) -> String {
    let name = *mod_scope.type_name(ty);
    if mod_scope.type_is_extern(ty) {
        // Use name verbatim
        name
    } else {
        // Use mangled name
        mangler.get(name)
    }.to_string()
}
