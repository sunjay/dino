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
    solve::{verify_valid_tys_or_default, apply_eq_constraints, verify_substitution},
};
use super::subst::TypeSubst;

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

/// Asserts that the given type variables must correspond to the same types
#[derive(Debug)]
pub struct TyVarEquals(pub TyVar, pub TyVar);

#[derive(Debug, Default)]
pub struct ConstraintSet {
    /// The concrete types collected along the way
    /// Each type variable can only be assigned to a single type
    subst: TypeSubst,
    /// A list of variable equality constraints
    ty_var_equals: Vec<TyVarEquals>,
    /// A list of variables associated with integer literals (int, real, complex)
    int_vars: HashSet<TyVar>,
    /// A list of variables associated with real literals (real, complex)
    real_vars: HashSet<TyVar>,
    /// The ID of the next type variable
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
    pub fn solve(self, prims: &Primitives) -> Result<TypeSubst, Error> {
        let Self {mut subst, ty_var_equals, int_vars, real_vars, next_var} = self;

        apply_eq_constraints(&ty_var_equals, &mut subst)?;

        // Assert that the literals are one of the expected types for that kind of literal
        verify_valid_tys_or_default(
            &int_vars,
            &hashset!{prims.int(), prims.real(), prims.complex()},
            prims.int(),
            &mut subst,
        ).map_err(|actual| Error::InvalidIntLitType {actual})?;
        verify_valid_tys_or_default(
            &real_vars,
            &hashset!{prims.real(), prims.complex()},
            prims.real(),
            &mut subst,
        ).map_err(|actual| Error::InvalidRealLitType {actual})?;

        // The resulting substitution must contain all variables
        verify_substitution(&subst, (0..next_var).map(TyVar))?;

        Ok(subst)
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
        self.subst.insert(return_type, func_return_type)
            .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

        // Add each parameter as a local variable in the function scope
        for &ir::FuncParam {name, ty} in params {
            // Each parameter (and all its uses) must type check to the declared type
            let param_ty_var = self.fresh_type_var();
            self.subst.insert(param_ty_var, ty)
                .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;
            scope.add_variable(name, param_ty_var);
        }

        // Type expected from block is the same as the type expected from the function
        let body = self.append_block(body, return_type, return_type, &mut scope, decls, prims)?;
        Ok(tyir::Function {name, sig, body})
    }


    /// Appends constraints for the given block
    ///
    /// IMPORTANT: The scope passed to this function should pretty much *always* be a new scope or
    /// a new child scope (created with `child_scope`)
    fn append_block<'a, 's>(
        &mut self,
        block: &'a ast::Block<'a>,
        // The type expected from the block
        return_type: TyVar,
        // The return type of the function surrounding this block
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Block<'a>, Error> {
        let ast::Block {stmts, ret} = block;

        Ok(tyir::Block {
            stmts: stmts.iter()
                .map(|stmt| self.append_stmt(stmt, func_return_type, scope, decls, prims))
                .collect::<Result<Vec<_>, _>>()?,
            ret: match ret {
                // The returned expression must have the same type as the block
                Some(ret) => Some(self.append_expr(ret, return_type, func_return_type, scope, decls, prims)?),

                None => {
                    // No return expression, so the return type of this block should be unit
                    self.subst.insert(return_type, prims.unit())
                        .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

                    None
                },
            },
            ret_ty_var: return_type,
        })
    }

    /// Appends constraints for the given statement
    fn append_stmt<'a, 's>(
        &mut self,
        stmt: &'a ast::Stmt<'a>,
        // The return type of the function surrounding this statement
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Stmt<'a>, Error> {
        match stmt {
            ast::Stmt::Cond(cond) => self.append_cond(cond, None, func_return_type, scope, decls, prims)
                .map(tyir::Stmt::Cond),
            ast::Stmt::WhileLoop(wloop) => self.append_while_loop(wloop, func_return_type, scope, decls, prims)
                .map(tyir::Stmt::WhileLoop),
            ast::Stmt::VarDecl(decl) => self.append_var_decl(decl, func_return_type, scope, decls, prims)
                .map(tyir::Stmt::VarDecl),
            ast::Stmt::Expr(expr) => {
                // Generate a fresh variable that is never used after this point. By not using the
                // type variable, we indicate that the type of this expression does not matter.
                // Statement types do not matter because statements end with semicolons.
                let ty_var = self.fresh_type_var();
                self.append_expr(expr, ty_var, func_return_type, scope, decls, prims).map(tyir::Stmt::Expr)
            },
        }
    }

    /// Appends constraints for the given variable declaration
    fn append_while_loop<'a, 's>(
        &mut self,
        wloop: &'a ast::WhileLoop<'a>,
        // The return type of the function surrounding this while loop
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::WhileLoop<'a>, Error> {
        let ast::WhileLoop {cond, body} = wloop;

        // Every condition must evaluate to a value of type bool
        let cond_var = self.fresh_type_var();
        self.subst.insert(cond_var, prims.bool())
            .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;
        // Loop condition must use the parent scope, not the child scope for the loop body
        let cond = self.append_expr(cond, cond_var, func_return_type, scope, decls, prims)?;

        // Loops are not currently allowed in expression position, so the body must result in ()
        let loop_body_var = self.fresh_type_var();
        self.subst.insert(loop_body_var, prims.unit())
            .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;
        // The body of the loop gets a new inner scope so that variables declared within it aren't
        // accessible after the loop has finished running
        let mut child_scope = scope.child_scope();
        let body = self.append_block(body, loop_body_var, func_return_type, &mut child_scope, decls, prims)?;

        Ok(tyir::WhileLoop {cond, body})
    }

    /// Appends constraints for the given variable declaration
    fn append_var_decl<'a, 's>(
        &mut self,
        var_decl: &'a ast::VarDecl<'a>,
        // The return type of the function surrounding this variable declaration
        func_return_type: TyVar,
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
            self.subst.insert(var_decl_ty_var, var_decl_ty)
                .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;
        }

        // Must append expr BEFORE updating local scope with the new type variable or else variable
        // shadowing will not work. Semantically, this variable does not come into scope until
        // *after* the variable expression has been evaluated.
        let expr = self.append_expr(expr, var_decl_ty_var, func_return_type, scope, decls, prims)?;

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
        // The type expected from the expression
        return_type: TyVar,
        // The return type of the function surrounding this expression
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Expr<'a>, Error> {
        match expr {
            ast::Expr::Cond(cond) => {
                self.append_cond(cond, Some(return_type), func_return_type, scope, decls, prims)
                    .map(|cond| tyir::Expr::Cond(Box::new(cond), return_type))
            },

            ast::Expr::Call(call) => {
                self.append_call_expr(call, return_type, func_return_type, scope, decls, prims)
                    .map(|call| tyir::Expr::Call(call, return_type))
            },

            ast::Expr::VarAssign(assign) => {
                self.append_var_assign(assign, return_type, func_return_type, scope, decls, prims)
                    .map(|assign| tyir::Expr::VarAssign(Box::new(assign), return_type))
            },

            ast::Expr::Return(ret_expr) => {
                self.append_return(ret_expr.as_ref().map(|x| x.as_ref()), return_type, func_return_type, scope, decls, prims)
                    .map(|ret_expr| tyir::Expr::Return(ret_expr.map(Box::new), return_type))
            },

            ast::Expr::BStrLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.subst.insert(return_type, prims.bstr())
                    .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

                Ok(tyir::Expr::BStrLiteral(value, return_type))
            },

            &ast::Expr::IntegerLiteral(ast::IntegerLiteral {value, type_hint}) => {
                // Check if the user specified a specific type for the integer literal
                if let Some(ty_name) = type_hint {
                    let expected_type = decls.type_id(&ty_name)
                        .expect("bug: parser allowed an invalid integer type hint");
                    self.subst.insert(return_type, expected_type)
                        .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;
                }

                self.int_vars.insert(return_type);
                Ok(tyir::Expr::IntegerLiteral(value, return_type))
            },

            &ast::Expr::RealLiteral(value) => {
                self.real_vars.insert(return_type);

                Ok(tyir::Expr::RealLiteral(value, return_type))
            },

            &ast::Expr::ComplexLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.subst.insert(return_type, prims.complex())
                    .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

                Ok(tyir::Expr::ComplexLiteral(value, return_type))
            },

            &ast::Expr::BoolLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.subst.insert(return_type, prims.bool())
                    .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

                Ok(tyir::Expr::BoolLiteral(value, return_type))
            },

            &ast::Expr::UnitLiteral => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.subst.insert(return_type, prims.unit())
                    .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

                Ok(tyir::Expr::UnitLiteral(return_type))
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
        // The type expected from the conditional blocks
        return_type: Option<TyVar>,
        // The return type of the function surrounding this conditional
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Cond<'a>, Error> {
        let ast::Cond {conds, else_body} = cond;
        debug_assert!(!conds.is_empty(), "bug: conditional had no initial if block");

        let no_return_type = return_type.is_none();
        // Create a fresh type variable if none was provided
        let return_type = return_type.unwrap_or_else(|| self.fresh_type_var());

        // If the condition is used as a statement (no return type) or if there is no else clause,
        // the if condition must return unit
        if no_return_type || cond.else_body.is_none() {
            self.subst.insert(return_type, prims.unit())
                .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;
        }

        let conds = conds.iter().map(|(cond, body)| {
            // Every condition must evaluate to a value of type bool
            let cond_var = self.fresh_type_var();
            self.subst.insert(cond_var, prims.bool())
                .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;
            let cond = self.append_expr(cond, cond_var, func_return_type, scope, decls, prims)?;

            // The body of every condition must evaluate to the same type
            let mut child_scope = scope.child_scope();
            let body = self.append_block(body, return_type, func_return_type, &mut child_scope, decls, prims)?;

            Ok((cond, body))
        }).collect::<Result<Vec<_>, _>>()?;

        // The body of the else clause must evaluate to the same type as the other condition bodies
        let else_body = else_body.as_ref().map(|else_body| {
            let mut child_scope = scope.child_scope();
            self.append_block(else_body, return_type, func_return_type, &mut child_scope, decls, prims)
        }).transpose()?;

        Ok(tyir::Cond {conds, else_body})
    }

    /// Appends constraints for the given function call
    fn append_call_expr<'a, 's>(
        &mut self,
        call: &'a ast::CallExpr<'a>,
        // The type expected from the call expression
        return_type: TyVar,
        // The return type of the function surrounding this call
        func_return_type: TyVar,
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
        let ir::FuncSig {return_type: call_return_type, params} = resolve_sig(decls, prims, sig)?;
        self.subst.insert(return_type, call_return_type)
            .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

        let args = args.iter().zip(params).map(|(arg, param)| {
            let arg_ty_var = self.fresh_type_var();

            // Assert that each argument matches the corresponding parameter type
            let ir::FuncParam {name: _, ty: param_ty} = param;
            self.subst.insert(arg_ty_var, param_ty)
                .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

            self.append_expr(arg, arg_ty_var, func_return_type, scope, decls, prims)
        }).collect::<Result<Vec<_>, _>>()?;

        Ok(tyir::CallExpr {
            func_name,
            args,
        })
    }

    /// Appends constraints for the given assignment expression
    fn append_var_assign<'a, 's>(
        &mut self,
        assign: &'a ast::VarAssign<'a>,
        // The type expected from the assignment expression
        return_type: TyVar,
        // The return type of the function surrounding this assignment
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::VarAssign<'a>, Error> {
        let ast::VarAssign {ident, expr} = assign;

        // The return type of a variable assignment is ()
        self.subst.insert(return_type, prims.unit())
            .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

        // The type of the right-hand expression of the assignment must match the type of the
        // variable being assigned to
        let var_ty_var = scope.get(ident).context(UnresolvedName {name: *ident})?;
        let expr = self.append_expr(expr, var_ty_var, func_return_type, scope, decls, prims)?;

        Ok(tyir::VarAssign {ident, expr})
    }

    /// Appends constraints for the given return expression
    fn append_return<'a, 's>(
        &mut self,
        ret_expr: Option<&'a ast::Expr<'a>>,
        // The type expected from the return expression
        return_type: TyVar,
        // The return type of the function surrounding this return expression
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<Option<tyir::Expr<'a>>, Error> {
        // A return expression always type checks to ()
        self.subst.insert(return_type, prims.unit())
            .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

        Ok(match ret_expr {
            // The return expression must match the type returned from the function
            Some(ret_expr) => {
                Some(self.append_expr(ret_expr, func_return_type, func_return_type, scope, decls, prims)?)
            },
            // No return expression, thus the function must be returning unit
            None => {
                self.subst.insert(func_return_type, prims.unit())
                    .map_err(|mismatch| Error::MismatchedTypes {expected: mismatch.expected, actual: mismatch.actual})?;

                None
            },
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
