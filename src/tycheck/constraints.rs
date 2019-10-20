use std::collections::HashSet;

use snafu::OptionExt;
use maplit::hashset;
use ena::unify::{InPlaceUnificationTable, UnifyKey, EqUnifyValue};

use crate::resolve::{DeclMap, TyId};
use crate::primitives::Primitives;
use crate::{ast, ir};

use super::{
    Error,
    UnresolvedName,
    UnresolvedType,
    UnresolvedFunction,
    UnresolvedMethod,
    AmbiguousMethodCall,
    tyir,
    solve::{build_substitution, verify_valid_tys_or_default},
};
use super::scope::Scope;
use super::subst::TypeSubst;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(u32);

impl UnifyKey for TyVar {
    type Value = Option<TyId>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(id: u32) -> Self {
        TyVar(id)
    }

    fn tag() -> &'static str {
        "TyVar"
    }
}

impl EqUnifyValue for TyId {}

#[derive(Debug, Default)]
pub struct ConstraintSet {
    /// The concrete types for each variable, collected along the way.
    /// A union-find implementation keeps track of which type variables are equal to each other
    /// and maintains that equivalence as values are updated.
    ty_var_table: InPlaceUnificationTable<TyVar>,
    /// A list of variables associated with integer literals (int, real, complex)
    int_vars: HashSet<TyVar>,
    /// A list of variables associated with real literals (real, complex)
    real_vars: HashSet<TyVar>,
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
        let Self {mut ty_var_table, int_vars, real_vars} = self;

        // Assert that the literals are one of the expected types for that kind of literal
        verify_valid_tys_or_default(
            &int_vars,
            &hashset!{prims.int(), prims.real(), prims.complex()},
            prims.int(),
            &mut ty_var_table,
        ).map_err(|actual| Error::InvalidIntLitType {actual})?;
        verify_valid_tys_or_default(
            &real_vars,
            &hashset!{prims.real(), prims.complex()},
            prims.real(),
            &mut ty_var_table,
        ).map_err(|actual| Error::InvalidRealLitType {actual})?;

        // The resulting substitution must contain all variables
        let ty_vars = (0..ty_var_table.len()).map(|id| TyVar(id as u32));
        let ty_vars = ty_vars.map(|ty_var| (ty_var, ty_var_table.probe_value(ty_var)));
        build_substitution(ty_vars)
    }

    /// Generates a fresh type variable and returns it
    fn fresh_type_var(&mut self) -> TyVar {
        self.ty_var_table.new_key(None)
    }

    /// Asserts that a given type variable is the given type
    fn ty_var_is_ty(&mut self, ty_var: TyVar, ty: TyId) -> Result<(), Error> {
        self.ty_var_table.unify_var_value(ty_var, Some(ty))
            .map_err(|(expected, actual)| Error::MismatchedTypes {expected, actual})
    }

    /// Asserts that the given type variables must correspond to the same types
    fn ty_var_equals(&mut self, ty_var1: TyVar, ty_var2: TyVar) -> Result<(), Error> {
        self.ty_var_table.unify_var_var(ty_var1, ty_var2)
            .map_err(|(expected, actual)| Error::MismatchedTypes {expected, actual})
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
        self.ty_var_is_ty(return_type, func_return_type)?;

        // Add each parameter as a local variable in the function scope
        for &ir::FuncParam {name, ty} in params {
            // Each parameter (and all its uses) must type check to the declared type
            let param_ty_var = self.fresh_type_var();
            self.ty_var_is_ty(param_ty_var, ty)?;
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
        decls: &'a DeclMap<'a>,
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
                    self.ty_var_is_ty(return_type, prims.unit())?;

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
        decls: &'a DeclMap<'a>,
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
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<tyir::WhileLoop<'a>, Error> {
        let ast::WhileLoop {cond, body} = wloop;

        // Every condition must evaluate to a value of type bool
        let cond_var = self.fresh_type_var();
        self.ty_var_is_ty(cond_var, prims.bool())?;
        // Loop condition must use the parent scope, not the child scope for the loop body
        let cond = self.append_expr(cond, cond_var, func_return_type, scope, decls, prims)?;

        // Loops are not currently allowed in expression position, so the body must result in ()
        let loop_body_var = self.fresh_type_var();
        self.ty_var_is_ty(loop_body_var, prims.unit())?;
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
        decls: &'a DeclMap<'a>,
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
            self.ty_var_is_ty(var_decl_ty_var, var_decl_ty)?;
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
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<tyir::Expr<'a>, Error> {
        match expr {
            ast::Expr::VarAssign(assign) => {
                self.append_var_assign(assign, return_type, func_return_type, scope, decls, prims)
                    .map(|assign| tyir::Expr::VarAssign(Box::new(assign), return_type))
            },

            ast::Expr::MethodCall(call) => {
                self.append_method_call(call, return_type, func_return_type, scope, decls, prims)
                    .map(|call| tyir::Expr::Call(call, return_type))
            },

            ast::Expr::Cond(cond) => {
                self.append_cond(cond, Some(return_type), func_return_type, scope, decls, prims)
                    .map(|cond| tyir::Expr::Cond(Box::new(cond), return_type))
            },

            ast::Expr::Call(call) => {
                self.append_func_call(call, return_type, func_return_type, scope, decls, prims)
                    .map(|call| tyir::Expr::Call(call, return_type))
            },

            ast::Expr::Return(ret_expr) => {
                self.append_return(ret_expr.as_ref().map(|x| x.as_ref()), return_type, func_return_type, scope, decls, prims)
                    .map(|ret_expr| tyir::Expr::Return(ret_expr.map(Box::new), return_type))
            },

            ast::Expr::BStrLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.ty_var_is_ty(return_type, prims.bstr())?;

                Ok(tyir::Expr::BStrLiteral(value, return_type))
            },

            &ast::Expr::IntegerLiteral(ast::IntegerLiteral {value, type_hint}) => {
                // Check if the user specified a specific type for the integer literal
                if let Some(ty_name) = type_hint {
                    let expected_type = decls.type_id(&ty_name)
                        .expect("bug: parser allowed an invalid integer type hint");
                    self.ty_var_is_ty(return_type, expected_type)?;
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
                self.ty_var_is_ty(return_type, prims.complex())?;

                Ok(tyir::Expr::ComplexLiteral(value, return_type))
            },

            &ast::Expr::BoolLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.ty_var_is_ty(return_type, prims.bool())?;

                Ok(tyir::Expr::BoolLiteral(value, return_type))
            },

            &ast::Expr::UnitLiteral => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.ty_var_is_ty(return_type, prims.unit())?;

                Ok(tyir::Expr::UnitLiteral(return_type))
            },

            &ast::Expr::Var(name) => {
                let var_ty_var = scope.get(name).context(UnresolvedName {name})?;
                // Assert that the type of the variable must be equal to the type expected from the
                // expression
                self.ty_var_equals(var_ty_var, return_type)?;

                Ok(tyir::Expr::Var(name, var_ty_var))
            },
        }
    }

    /// Appends constraints for the given method call
    fn append_method_call<'a, 's>(
        &mut self,
        call: &'a ast::MethodCall<'a>,
        // The type expected from the call expression
        return_type: TyVar,
        // The return type of the function surrounding this call
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<tyir::CallExpr<'a>, Error> {
        let ast::MethodCall {lhs, call} = call;

        // Generate the constraints for the left-hand side expression, with the hope that this
        // type variable gets assigned a type
        //TODO: Rather than hoping for a type, it would be neat to have a way to solve the current
        // set of constraints to see if we can get a type based on what we have.
        let lhs_ty_var = self.fresh_type_var();
        let lhs = self.append_expr(lhs, lhs_ty_var, func_return_type, scope, decls, prims)?;

        // In order to call the method, we must know the type of lhs at this point. Hopefully the
        // constraint generation for that expression gave us something.
        let lhs_ty = self.ty_var_table.probe_value(lhs_ty_var)
            .with_context(|| AmbiguousMethodCall {})?;

        let ast::CallExpr {func_name: method_name, args} = call;
        let func = decls.method(lhs_ty, method_name)
            .context(UnresolvedMethod {name: *method_name, ty: lhs_ty})?;

        let has_self = func.sig.params.get(0).map(|param| param.name == "self").unwrap_or(false);
        if !has_self {
            return Err(Error::UnexpectedAssociatedFunction {});
        }

        // Using the func.name like this works for extern methods. Not sure what user-defined
        // methods will look like just yet. Maybe this is a hack?
        assert!(func.is_extern,
            "TODO: Only methods with extern declarations are currently supported");
        let func_name = &func.name;

        // Append the `self` argument as the lhs expression
        self.append_func_call_sig(&func.sig, func_name, args, Some(lhs), return_type, func_return_type, scope, decls, prims)
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
        decls: &'a DeclMap<'a>,
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
            self.ty_var_is_ty(return_type, prims.unit())?;
        }

        let conds = conds.iter().map(|(cond, body)| {
            // Every condition must evaluate to a value of type bool
            let cond_var = self.fresh_type_var();
            self.ty_var_is_ty(cond_var, prims.bool())?;
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
    fn append_func_call<'a, 's>(
        &mut self,
        call: &'a ast::CallExpr<'a>,
        // The type expected from the call expression
        return_type: TyVar,
        // The return type of the function surrounding this call
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<tyir::CallExpr<'a>, Error> {
        let ast::CallExpr {func_name, args} = call;

        let sig = decls.func_sig(func_name)
            .context(UnresolvedFunction {name: *func_name})?;

        self.append_func_call_sig(sig, func_name, args, None, return_type, func_return_type, scope, decls, prims)
    }

    /// Appends constraints for the given function call given the signature
    fn append_func_call_sig<'a, 's>(
        &mut self,
        sig: &ast::FuncSig,
        // The function name to call, not necessarily the original function/method name
        func_name: &'a ast::Ident<'a>,
        args: &'a [ast::Expr<'a>],
        // An extra argument to prepend on to the list of arguments passed to the call
        // Used to implement methods with a `self` parameter
        extra_first_arg: Option<tyir::Expr<'a>>,
        // The type expected from the call expression
        return_type: TyVar,
        // The return type of the function surrounding this call
        func_return_type: TyVar,
        scope: &mut Scope<'a, 's>,
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<tyir::CallExpr<'a>, Error> {
        // Return early if the number of arguments is wrong
        let num_extra_args = match extra_first_arg {
            Some(_) => 1,
            None => 0,
        };
        let num_actual_args = args.len() + num_extra_args;
        if num_actual_args != sig.params.len() {
            return Err(Error::ArityMismatch {
                func_name: func_name.to_string(),
                expected: sig.params.len(),
                actual: num_actual_args,
            });
        }

        // Assert that the return type of this expression is the same as the function return type
        let ir::FuncSig {return_type: call_return_type, params} = resolve_sig(decls, prims, sig)?;
        self.ty_var_is_ty(return_type, call_return_type)?;

        let args = extra_first_arg.map(Ok).into_iter().chain(args.iter().zip(params).map(|(arg, param)| {
            let arg_ty_var = self.fresh_type_var();

            // Assert that each argument matches the corresponding parameter type
            let ir::FuncParam {name: _, ty: param_ty} = param;
            self.ty_var_is_ty(arg_ty_var, param_ty)?;

            self.append_expr(arg, arg_ty_var, func_return_type, scope, decls, prims)
        })).collect::<Result<Vec<_>, _>>()?;

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
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<tyir::VarAssign<'a>, Error> {
        let ast::VarAssign {ident, expr} = assign;

        // The return type of a variable assignment is ()
        self.ty_var_is_ty(return_type, prims.unit())?;

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
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<Option<tyir::Expr<'a>>, Error> {
        // A return expression always type checks to ()
        self.ty_var_is_ty(return_type, prims.unit())?;

        Ok(match ret_expr {
            // The return expression must match the type returned from the function
            Some(ret_expr) => {
                Some(self.append_expr(ret_expr, func_return_type, func_return_type, scope, decls, prims)?)
            },
            // No return expression, thus the function must be returning unit
            None => {
                self.ty_var_is_ty(func_return_type, prims.unit())?;

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
