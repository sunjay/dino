use std::collections::{HashSet, HashMap};

use snafu::OptionExt;
use maplit::hashset;

use crate::resolve::{DeclMap, TyId};
use crate::primitives::Primitives;
use crate::{ast, ir};

use super::{
    Error,
    UnresolvedName,
    UnresolvedType,
    UnresolvedFunction,
    tyir,
    solve::{TypeSubst, collect_valid_types, apply_eq_constraints, build_substitution},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(usize);

/// A map of variable names to their type variables
type LocalScope<'a> = HashMap<ast::Ident<'a>, TyVar>;

/// A linked list of scopes, used to create an ad-hoc stack where only the "top" is mutable
///
/// `'a` is the lifetime of the things stored in the scope. `'s` is the lifetime of the parent scope.
#[derive(Debug, Default)]
struct Scope<'a, 's> {
    /// The current scope, it can be modified freely
    pub current: LocalScope<'a>,
    /// The next upper level of scope (if any)
    pub parent: Option<&'s Scope<'a, 's>>,
}

impl<'a, 's> Scope<'a, 's> {
    /// Create a new mutable child scope
    pub fn child_scope(&'s self) -> Self {
        Self {
            current: LocalScope::default(),
            parent: Some(self),
        }
    }

    /// Returns true if the given variable name is in scope at the current level, or in any parent
    /// scope.
    pub fn contains(&self, name: &ast::Ident<'a>) -> bool {
        self.get(name).is_some()
    }

    /// Returns the type variable associated with a given variable name
    pub fn get(&self, name: ast::Ident<'a>) -> Option<TyVar> {
        self.current.get(name).copied().or_else(|| match self.parent {
            Some(parent) => parent.get(name),
            None => None,
        })
    }

    /// Adds a new variable *only* to the current scope and associates it with the given type
    /// variable
    pub fn add_variable(&mut self, name: ast::Ident<'a>, ty_var: TyVar) {
        self.current.insert(name, ty_var);
    }
}

/// Asserts that a given type variable must be one of the given types
#[derive(Debug)]
pub struct TyVarValidTypes {
    /// The type variable being constrained
    pub ty_var: TyVar,
    /// The set of types valid for this variable
    pub valid_tys: HashSet<TyId>,
}

/// Asserts that the given type variables must correspond to the same set of types
#[derive(Debug)]
pub struct TyVarEquals(pub TyVar, pub TyVar);

#[derive(Debug, Default)]
pub struct ConstraintSet {
    ty_var_valid_types: Vec<TyVarValidTypes>,
    ty_var_equals: Vec<TyVarEquals>,
    next_var: usize,
}

impl ConstraintSet {
    /// Generates a constraint set for the given function declaration. Any fresh type variables
    /// created are annotated inline into the returned `tyir::Function`
    pub fn generate<'a>(
        func: &'a ast::Function<'a>,
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<(Self, tyir::Function<'a>), Error> {
        let mut constraints = Self::default();
        let func = constraints.append_func(func, decls, prims)?;
        Ok((constraints, func))
    }

    /// Attempts to solve the constraint set and return the solution as a substitution map
    pub fn solve(
        self,
        resolve_ambiguity: impl Fn(&HashSet<TyId>) -> Option<TyId> + Send + Sync,
    ) -> Result<TypeSubst, Error> {
        let Self {ty_var_valid_types, ty_var_equals, next_var} = self;

        let mut valid_types = collect_valid_types(ty_var_valid_types);
        apply_eq_constraints(&ty_var_equals, &mut valid_types);

        build_substitution(valid_types, (0..next_var).map(TyVar), resolve_ambiguity)
    }

    /// Generates a fresh type variable and returns it
    fn fresh_type_var(&mut self) -> TyVar {
        let var = TyVar(self.next_var);
        self.next_var += 1;
        var
    }

    /// Appends constrains for the given function
    fn append_func<'a>(
        &mut self,
        func: &'a ast::Function<'a>,
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<tyir::Function<'a>, Error> {
        let mut scope = Scope::default();

        let ast::Function {name, sig, body, is_extern} = func;
        assert!(!is_extern, "bug: attempt to type check an extern function");

        let sig = resolve_sig(decls, prims, sig)?;
        let ir::FuncSig {return_type: func_return_type, ref params} = sig;

        // Assert that the function body block returns the expected type
        let return_type = self.fresh_type_var();
        self.ty_var_valid_types.push(TyVarValidTypes {
            ty_var: return_type,
            valid_tys: hashset!{func_return_type},
        });

        // Add each parameter as a local variable in the function scope
        for &ir::FuncParam {name, ty} in params {
            // Each parameter (and all its uses) must type check to the declared type
            let param_ty_var = self.fresh_type_var();
            self.ty_var_valid_types.push(TyVarValidTypes {
                ty_var: param_ty_var,
                valid_tys: hashset!{ty},
            });
            scope.add_variable(name, param_ty_var);
        }

        let body = self.append_block(body, return_type, &mut scope, decls, prims)?;
        Ok(tyir::Function {name, sig, body})
    }


    /// Appends constraints for the given block
    ///
    /// IMPORTANT: The scope passed to this function should pretty much *always* be a new scope or
    /// a new child scope (created with `child_scope`)
    fn append_block<'a, 's>(
        &mut self,
        block: &'a ast::Block<'a>,
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Block<'a>, Error> {
        let ast::Block {stmts, ret} = block;

        Ok(tyir::Block {
            stmts: stmts.iter()
                .map(|stmt| self.append_stmt(stmt, scope, decls, prims))
                .collect::<Result<Vec<_>, _>>()?,
            ret: match ret {
                // The returned expression must have the same type as the block
                Some(ret) => Some(self.append_expr(ret, return_type, scope, decls, prims)?),

                None => {
                    // No return expression, so the return type of this block should be unit
                    self.ty_var_valid_types.push(TyVarValidTypes {
                        ty_var: return_type,
                        valid_tys: hashset!{prims.unit()},
                    });

                    None
                },
            },
        })
    }

    /// Appends constraints for the given statement
    fn append_stmt<'a, 's>(
        &mut self,
        stmt: &'a ast::Stmt<'a>,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Stmt<'a>, Error> {
        match stmt {
            ast::Stmt::Cond(cond) => self.append_cond(cond, None, scope, decls, prims)
                .map(tyir::Stmt::Cond),
            ast::Stmt::VarDecl(decl) => self.append_var_decl(decl, scope, decls, prims)
                .map(tyir::Stmt::VarDecl),
            ast::Stmt::Expr(expr) => {
                // Generate a fresh variable that is never used after this point. By not using the
                // type variable, we indicate that the type of this expression does not matter.
                // Statement types do not matter because statements end with semicolons.
                let ty_var = self.fresh_type_var();
                self.append_expr(expr, ty_var, scope, decls, prims).map(tyir::Stmt::Expr)
            },
        }
    }

    /// Appends constraints for the given variable declaration
    fn append_var_decl<'a, 's>(
        &mut self,
        var_decl: &'a ast::VarDecl<'a>,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::VarDecl<'a>, Error> {
        let ast::VarDecl {ident, ty, expr} = var_decl;

        if scope.contains(ident) {
            //TODO: To support variable shadowing in this algorithm we just need to make sure that
            // statements are walked in order and that new variable declarations overwrite the
            // recorded types of previous declarations with a *fresh* variable.
            // Need to be a bit careful to make sure this works with nested scopes.
            panic!("TODO: Variable shadowing is not supported yet.");
        }

        // Generate a fresh variable for the var decl
        let var_decl_ty_var = self.fresh_type_var();

        // The type variable should match the annotated type (if any)
        if let Some(ty) = ty {
            let var_decl_ty = lookup_type(decls, prims, ty)?;
            self.ty_var_valid_types.push(TyVarValidTypes {
                ty_var: var_decl_ty_var,
                valid_tys: hashset!{var_decl_ty},
            });
        }

        // Must append expr BEFORE updating local scope with the new type variable or else variable
        // shadowing will not work. Semantically, this variable does not come into scope until
        // *after* the variable expression has been evaluated.
        let expr = self.append_expr(expr, var_decl_ty_var, scope, decls, prims)?;

        // Associate the variable name with its type variable
        scope.add_variable(ident, var_decl_ty_var);

        Ok(tyir::VarDecl {
            ident,
            ty_var: var_decl_ty_var,
            expr,
        })
    }

    /// Appends constraints for the given expression
    fn append_expr<'a, 's>(
        &mut self,
        expr: &'a ast::Expr<'a>,
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Expr<'a>, Error> {
        match expr {
            ast::Expr::Cond(cond) => {
                self.append_cond(cond, Some(return_type), scope, decls, prims)
                    .map(|cond| tyir::Expr::Cond(Box::new(cond), return_type))
            },

            ast::Expr::Call(call) => {
                self.append_call_expr(call, return_type, scope, decls, prims)
                    .map(|call| tyir::Expr::Call(call, return_type))
            },

            &ast::Expr::IntegerLiteral(ast::IntegerLiteral {value, type_hint}) => {
                // Assert that the literal is one of the expected types for this kind of literal
                let valid_tys = match type_hint {
                    Some(ty_name) => hashset!{decls.type_id(&ty_name)
                        .expect("bug: parser allowed an invalid integer type hint")},
                    // Integer literals can be used to create instances of all these types
                    None => hashset!{prims.int(), prims.real(), prims.complex()}
                };
                self.ty_var_valid_types.push(TyVarValidTypes {ty_var: return_type, valid_tys});

                Ok(tyir::Expr::IntegerLiteral(value, return_type))
            },

            &ast::Expr::RealLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.ty_var_valid_types.push(TyVarValidTypes {
                    ty_var: return_type,
                    valid_tys: hashset!{prims.real(), prims.complex()},
                });

                Ok(tyir::Expr::RealLiteral(value, return_type))
            },

            &ast::Expr::ComplexLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.ty_var_valid_types.push(TyVarValidTypes {
                    ty_var: return_type,
                    valid_tys: hashset!{prims.complex()},
                });

                Ok(tyir::Expr::ComplexLiteral(value, return_type))
            },

            &ast::Expr::BoolLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.ty_var_valid_types.push(TyVarValidTypes {
                    ty_var: return_type,
                    valid_tys: hashset!{prims.bool()},
                });

                Ok(tyir::Expr::BoolLiteral(value, return_type))
            },

            &ast::Expr::Var(name) => {
                // Assert that the type variable is equal to the return type variable
                let var_ty_var = scope.get(name).context(UnresolvedName {name})?;
                self.ty_var_equals.push(
                    TyVarEquals(var_ty_var, return_type)
                );

                Ok(tyir::Expr::Var(name, var_ty_var))
            },
        }
    }

    /// Appends constraints for the given conditional
    ///
    /// If return_type is None, the conditional must result in a unit type
    fn append_cond<'a, 's>(
        &mut self,
        cond: &'a ast::Cond<'a>,
        return_type: Option<TyVar>,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Cond<'a>, Error> {
        let ast::Cond {conds, else_body} = cond;

        let no_return_type = return_type.is_none();
        // Create a fresh type variable if none was provided
        let return_type = return_type.unwrap_or_else(|| self.fresh_type_var());

        // If the condition is used as a statement (no return type) or if there is no else clause,
        // the if condition must return unit
        if no_return_type || cond.else_body.is_none() {
            self.ty_var_valid_types.push(TyVarValidTypes {
                ty_var: return_type,
                valid_tys: hashset!{prims.unit()},
            });
        }

        let conds = conds.iter().map(|(cond, body)| {
            // Every condition must evaluate to a value of type bool
            let cond_var = self.fresh_type_var();
            self.ty_var_valid_types.push(TyVarValidTypes {
                ty_var: cond_var,
                valid_tys: hashset!{prims.bool()},
            });
            let cond = self.append_expr(cond, cond_var, scope, decls, prims)?;

            // The body of every condition must evaluate to the same type
            let mut child_scope = scope.child_scope();
            let body = self.append_block(body, return_type, &mut child_scope, decls, prims)?;

            Ok((cond, body))
        }).collect::<Result<Vec<_>, _>>()?;

        // The body of the else clause must evaluate to the same type as the other condition bodies
        let else_body = else_body.as_ref().map(|else_body| {
            let mut child_scope = scope.child_scope();
            self.append_block(else_body, return_type, &mut child_scope, decls, prims)
        }).transpose()?;

        Ok(tyir::Cond {conds, else_body})
    }

    /// Appends constraints for the given function call
    fn append_call_expr<'a, 's>(
        &mut self,
        call: &'a ast::CallExpr<'a>,
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::CallExpr<'a>, Error> {
        let ast::CallExpr {func_name, args} = call;

        let sig = decls.func_sig(func_name)
            .context(UnresolvedFunction {name: *func_name})?;
        // Return early if the number of arguments is wrong
        if args.len() != sig.params.len() {
            return Err(Error::ArityMismatch {
                func_name: func_name.to_string(),
                expected: sig.params.len(),
                actual: args.len(),
            });
        }

        // Assert that the return type of this expression is the same as the function return type
        let ir::FuncSig {return_type: func_return_type, params} = resolve_sig(decls, prims, sig)?;
        self.ty_var_valid_types.push(TyVarValidTypes {
            ty_var: return_type,
            valid_tys: hashset!{func_return_type},
        });

        let args = args.iter().zip(params).map(|(arg, param)| {
            let arg_ty_var = self.fresh_type_var();

            // Assert that each argument matches the corresponding parameter type
            let ir::FuncParam {name: _, ty: param_ty} = param;
            self.ty_var_valid_types.push(TyVarValidTypes {
                ty_var: arg_ty_var,
                valid_tys: hashset!{param_ty},
            });

            self.append_expr(arg, arg_ty_var, scope, decls, prims)
        }).collect::<Result<Vec<_>, _>>()?;

        Ok(tyir::CallExpr {
            func_name,
            args,
        })
    }
}

/// Resolves all of the types in a function signature
fn resolve_sig<'a>(
    decls: &'a DeclMap<'a>,
    prims: &Primitives,
    sig: &'a ast::FuncSig<'a>,
) -> Result<ir::FuncSig<'a>, Error> {
    let ast::FuncSig {return_type, params} = sig;
    let return_type = lookup_type(decls, prims, return_type)?;
    let params = params.iter().map(|param| {
        let ast::FuncParam {name, ty} = param;
        Ok(ir::FuncParam {name, ty: lookup_type(decls, prims, ty)?})
    }).collect::<Result<Vec<_>, _>>()?;

    Ok(ir::FuncSig {return_type, params})
}

/// Resolves a single type to either a declared type or a primitive
fn lookup_type(decls: &DeclMap, prims: &Primitives, ty: &ast::Ty) -> Result<TyId, Error> {
    match ty {
        ast::Ty::Unit => Ok(prims.unit()),
        ast::Ty::Named(ty) => decls.type_id(ty).context(UnresolvedType {name: *ty}),
    }
}
