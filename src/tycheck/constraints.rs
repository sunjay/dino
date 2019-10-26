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
    UnresolvedField,
    AmbiguousFieldAccess,
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
    pub fn function<'a>(
        func: &'a ast::Function<'a>,
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<(Self, tyir::Function<'a>), Error> {
        let mut constraints = Self::default();
        let func = FunctionConstraintGenerator::generate(None, func, decls, prims, &mut constraints)?;
        Ok((constraints, func))
    }

    /// Generates a constraint set for the given method declaration. Any fresh type variables
    /// created are annotated inline into the returned `tyir::Function`
    pub fn method<'a>(
        self_ty: TyId,
        func: &'a ast::Function<'a>,
        decls: &'a DeclMap<'a>,
        prims: &Primitives,
    ) -> Result<(Self, tyir::Function<'a>), Error> {
        let mut constraints = Self::default();
        let method = FunctionConstraintGenerator::generate(Some(self_ty), func, decls, prims, &mut constraints)?;
        Ok((constraints, method))
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

    /// Attempts to get a concrete type for the given variable from the current solution. Returns
    /// None if no concrete type has been determined for this variable yet.
    pub fn ty_so_far(&mut self, ty_var: TyVar) -> Option<TyId> {
        self.ty_var_table.probe_value(ty_var)
    }

    /// Generates a fresh type variable and returns it
    pub fn fresh_type_var(&mut self) -> TyVar {
        self.ty_var_table.new_key(None)
    }

    /// Asserts that a given type variable is the given type
    pub fn ty_var_is_ty(&mut self, ty_var: TyVar, ty: TyId) -> Result<(), Error> {
        self.ty_var_table.unify_var_value(ty_var, Some(ty))
            .map_err(|(expected, actual)| Error::MismatchedTypes {expected, actual})
    }

    /// Asserts that the given type variables must correspond to the same types
    pub fn ty_var_equals(&mut self, ty_var1: TyVar, ty_var2: TyVar) -> Result<(), Error> {
        self.ty_var_table.unify_var_var(ty_var1, ty_var2)
            .map_err(|(expected, actual)| Error::MismatchedTypes {expected, actual})
    }

    /// Records this type variable as an int var so it can be special-cased in the later stages of
    /// type checking. No variable should be both an int var and a real var.
    pub fn ty_var_is_int(&mut self, ty_var: TyVar) {
        self.int_vars.insert(ty_var);
    }

    /// Records this type variable as an real var so it can be special-cased in the later stages of
    /// type checking. No variable should be both an int var and a real var.
    pub fn ty_var_is_real(&mut self, ty_var: TyVar) {
        self.real_vars.insert(ty_var);
    }
}

#[derive(Debug)]
struct FunctionConstraintGenerator<'a, 'b, 'c> {
    self_ty: Option<TyId>,
    decls: &'a DeclMap<'a>,
    prims: &'b Primitives,
    constraints: &'c mut ConstraintSet,
    /// The return type of the function being type checked
    func_return_type: TyVar,
}

impl<'a, 'b, 'c> FunctionConstraintGenerator<'a, 'b, 'c> {
    pub fn generate(
        self_ty: Option<TyId>,
        func: &'a ast::Function<'a>,
        decls: &'a DeclMap<'a>,
        prims: &'b Primitives,
        constraints: &'c mut ConstraintSet,
    ) -> Result<tyir::Function<'a>, Error> {
        let func_return_type = constraints.fresh_type_var();
        let mut generator = Self {
            self_ty,
            decls,
            prims,
            constraints,
            func_return_type,
        };

        generator.append_func(func)
    }

    /// Appends constrains for the given function
    fn append_func(&mut self, func: &'a ast::Function<'a>) -> Result<tyir::Function<'a>, Error> {
        let ast::Function {name, sig, body, is_extern} = func;
        assert!(!is_extern, "bug: attempt to type check an extern function");

        let sig = self.resolve_sig(sig)?;
        let ir::FuncSig {return_type: func_return_type, ref params} = sig;

        // Assert that the function body block returns the expected type
        let return_type = self.func_return_type;
        self.constraints.ty_var_is_ty(return_type, func_return_type)?;

        // Add each parameter as a local variable in the function scope
        let mut scope = Scope::default();
        for &ir::FuncParam {name, ty} in params {
            // Each parameter (and all its uses) must type check to the declared type
            let param_ty_var = self.constraints.fresh_type_var();
            self.constraints.ty_var_is_ty(param_ty_var, ty)?;
            scope.add_variable(name, param_ty_var);
        }

        // Type expected from block is the same as the type expected from the function
        let body = self.append_block(body, return_type, &mut scope)?;
        Ok(tyir::Function {name, sig, body})
    }


    /// Appends constraints for the given block
    ///
    /// IMPORTANT: The scope passed to this function should pretty much *always* be a new scope or
    /// a new child scope (created with `child_scope`)
    fn append_block<'s>(
        &mut self,
        block: &'a ast::Block<'a>,
        // The type expected from the block
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::Block<'a>, Error> {
        let ast::Block {stmts, ret} = block;

        Ok(tyir::Block {
            stmts: stmts.iter()
                .map(|stmt| self.append_stmt(stmt, scope))
                .collect::<Result<Vec<_>, _>>()?,
            ret: match ret {
                // The returned expression must have the same type as the block
                Some(ret) => Some(self.append_expr(ret, return_type, scope)?),

                None => {
                    // No return expression, so the return type of this block should be unit
                    self.constraints.ty_var_is_ty(return_type, self.prims.unit())?;

                    None
                },
            },
            ret_ty_var: return_type,
        })
    }

    /// Appends constraints for the given statement
    fn append_stmt<'s>(
        &mut self,
        stmt: &'a ast::Stmt<'a>,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::Stmt<'a>, Error> {
        match stmt {
            ast::Stmt::Cond(cond) => self.append_cond(cond, None, scope)
                .map(tyir::Stmt::Cond),
            ast::Stmt::WhileLoop(wloop) => self.append_while_loop(wloop, scope)
                .map(tyir::Stmt::WhileLoop),
            ast::Stmt::VarDecl(decl) => self.append_var_decl(decl, scope)
                .map(tyir::Stmt::VarDecl),
            ast::Stmt::Expr(expr) => {
                // Generate a fresh variable that is never used after this point. By not using the
                // type variable, we indicate that the type of this expression does not matter.
                // Statement types do not matter because statements end with semicolons.
                let ty_var = self.constraints.fresh_type_var();
                self.append_expr(expr, ty_var, scope).map(tyir::Stmt::Expr)
            },
        }
    }

    /// Appends constraints for the given variable declaration
    fn append_while_loop<'s>(
        &mut self,
        wloop: &'a ast::WhileLoop<'a>,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::WhileLoop<'a>, Error> {
        let ast::WhileLoop {cond, body} = wloop;

        // Every condition must evaluate to a value of type bool
        let cond_var = self.constraints.fresh_type_var();
        self.constraints.ty_var_is_ty(cond_var, self.prims.bool())?;
        // Loop condition must use the parent scope, not the child scope for the loop body
        let cond = self.append_expr(cond, cond_var, scope)?;

        // Loops are not currently allowed in expression position, so the body must result in ()
        let loop_body_var = self.constraints.fresh_type_var();
        self.constraints.ty_var_is_ty(loop_body_var, self.prims.unit())?;
        // The body of the loop gets a new inner scope so that variables declared within it aren't
        // accessible after the loop has finished running
        let mut child_scope = scope.child_scope();
        let body = self.append_block(body, loop_body_var, &mut child_scope)?;

        Ok(tyir::WhileLoop {cond, body})
    }

    /// Appends constraints for the given variable declaration
    fn append_var_decl<'s>(
        &mut self,
        var_decl: &'a ast::VarDecl<'a>,
        scope: &mut Scope<'a, 's>,
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
        let var_decl_ty_var = self.constraints.fresh_type_var();

        // The type variable should match the annotated type (if any)
        if let Some(ty) = ty {
            let var_decl_ty = self.lookup_type(ty)?;
            self.constraints.ty_var_is_ty(var_decl_ty_var, var_decl_ty)?;
        }

        // Must append expr BEFORE updating local scope with the new type variable or else variable
        // shadowing will not work. Semantically, this variable does not come into scope until
        // *after* the variable expression has been evaluated.
        let expr = self.append_expr(expr, var_decl_ty_var, scope)?;

        // Associate the variable name with its type variable
        scope.add_variable(ident, var_decl_ty_var);

        Ok(tyir::VarDecl {
            ident,
            ty_var: var_decl_ty_var,
            expr,
        })
    }

    /// Appends constraints for the given expression
    fn append_expr<'s>(
        &mut self,
        expr: &'a ast::Expr<'a>,
        // The type expected from the expression
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::Expr<'a>, Error> {
        match expr {
            ast::Expr::VarAssign(assign) => {
                self.append_var_assign(assign, return_type, scope)
                    .map(|assign| tyir::Expr::VarAssign(Box::new(assign), return_type))
            },

            ast::Expr::MethodCall(call) => {
                self.append_method_call(call, return_type, scope)
                    .map(|call| tyir::Expr::Call(call, return_type))
            },

            ast::Expr::FieldAccess(access) => {
                self.append_field_access(access, return_type, scope)
                    .map(|access| tyir::Expr::FieldAccess(Box::new(access), return_type))
            },

            ast::Expr::Cond(cond) => {
                self.append_cond(cond, Some(return_type), scope)
                    .map(|cond| tyir::Expr::Cond(Box::new(cond), return_type))
            },

            ast::Expr::Call(call) => {
                self.append_func_call(call, return_type, scope)
                    .map(|call| tyir::Expr::Call(call, return_type))
            },

            ast::Expr::Return(ret_expr) => {
                self.append_return(ret_expr.as_ref().map(|x| x.as_ref()), return_type, scope)
                    .map(|ret_expr| tyir::Expr::Return(ret_expr.map(Box::new), return_type))
            },

            ast::Expr::StructLiteral(struct_lit) => {
                unimplemented!()
            },

            ast::Expr::BStrLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.constraints.ty_var_is_ty(return_type, self.prims.bstr())?;

                Ok(tyir::Expr::BStrLiteral(value, return_type))
            },

            &ast::Expr::IntegerLiteral(ast::IntegerLiteral {value, type_hint}) => {
                // Check if the user specified a specific type for the integer literal
                if let Some(ty_name) = type_hint {
                    let expected_type = self.decls.type_id(&ty_name)
                        .expect("bug: parser allowed an invalid integer type hint");
                    self.constraints.ty_var_is_ty(return_type, expected_type)?;
                }

                self.constraints.ty_var_is_int(return_type);
                Ok(tyir::Expr::IntegerLiteral(value, return_type))
            },

            &ast::Expr::RealLiteral(value) => {
                self.constraints.ty_var_is_real(return_type);

                Ok(tyir::Expr::RealLiteral(value, return_type))
            },

            &ast::Expr::ComplexLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.constraints.ty_var_is_ty(return_type, self.prims.complex())?;

                Ok(tyir::Expr::ComplexLiteral(value, return_type))
            },

            &ast::Expr::BoolLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.constraints.ty_var_is_ty(return_type, self.prims.bool())?;

                Ok(tyir::Expr::BoolLiteral(value, return_type))
            },

            &ast::Expr::UnitLiteral => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.constraints.ty_var_is_ty(return_type, self.prims.unit())?;

                Ok(tyir::Expr::UnitLiteral(return_type))
            },

            &ast::Expr::SelfLiteral => {
                let name = "self";
                let var_ty_var = scope.get(name).context(UnresolvedName {name})?;
                // Assert that the type of the variable must be equal to the type expected from the
                // expression
                self.constraints.ty_var_equals(var_ty_var, return_type)?;

                Ok(tyir::Expr::Var(name, var_ty_var))
            },

            &ast::Expr::Var(name) => {
                let var_ty_var = scope.get(name).context(UnresolvedName {name})?;
                // Assert that the type of the variable must be equal to the type expected from the
                // expression
                self.constraints.ty_var_equals(var_ty_var, return_type)?;

                Ok(tyir::Expr::Var(name, var_ty_var))
            },
        }
    }

    /// Appends constraints for the given method call
    fn append_method_call<'s>(
        &mut self,
        call: &'a ast::MethodCall<'a>,
        // The type expected from the call expression
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::CallExpr<'a>, Error> {
        let ast::MethodCall {lhs, method_name, args} = call;

        // Generate the constraints for the left-hand side expression, with the hope that this
        // type variable gets assigned a type
        //TODO: Rather than hoping for a type, it would be neat to have a way to solve the current
        // set of constraints to see if we can get a type based on what we have.
        let lhs_ty_var = self.constraints.fresh_type_var();
        let lhs = self.append_expr(lhs, lhs_ty_var, scope)?;

        // In order to call the method, we must know the type of lhs at this point. Hopefully the
        // constraint generation for that expression gave us something.
        let lhs_ty = self.constraints.ty_so_far(lhs_ty_var)
            .with_context(|| AmbiguousMethodCall {})?;

        let func = self.decls.method(lhs_ty, method_name)
            .context(UnresolvedMethod {method_name: *method_name, ty: lhs_ty})?;

        let has_self = func.sig.params.get(0).map(|param| param.name == "self").unwrap_or(false);
        if !has_self {
            return Err(Error::UnexpectedAssociatedFunction {});
        }

        let func_name = if func.is_extern {
            // Using the func.name like this works for extern methods but not user-defined methods
            ast::IdentPath::from(func.name)

        } else {
            // If there is a self type, use that as `Type::method`
            match self.self_ty {
                Some(self_ty) => {
                    let &ty_name = self.decls.type_name(self_ty);
                    ast::IdentPath::from(vec![ty_name, func.name])
                },

                None => ast::IdentPath::from(func.name),
            }
        };

        // Append the `self` argument as the lhs expression
        self.append_func_call_sig(&func.sig, func_name, args, Some(lhs),
            return_type, scope)
    }

    /// Appends constraints for the given field access
    fn append_field_access<'s>(
        &mut self,
        access: &'a ast::FieldAccess<'a>,
        // The type expected from the field access
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::FieldAccess<'a>, Error> {
        let ast::FieldAccess {lhs, field} = access;

        // Generate the constraints for the left-hand side expression, with the hope that this
        // type variable gets assigned a type
        //TODO: Rather than hoping for a type, it would be neat to have a way to solve the current
        // set of constraints to see if we can get a type based on what we have.
        let lhs_ty_var = self.constraints.fresh_type_var();
        let lhs = self.append_expr(lhs, lhs_ty_var, scope)?;

        // In order to call the method, we must know the type of lhs at this point. Hopefully the
        // constraint generation for that expression gave us something.
        let lhs_ty = self.constraints.ty_so_far(lhs_ty_var)
            .with_context(|| AmbiguousFieldAccess {})?;

        let field_ty = self.decls.field_type(lhs_ty, field)
            .context(UnresolvedField {field_name: *field, ty: lhs_ty})?;

        let field_ty_id = self.lookup_type(field_ty)
            .expect("bug: field types should have been checked by this point");
        self.constraints.ty_var_is_ty(return_type, field_ty_id)?;

        Ok(tyir::FieldAccess {
            lhs,
            field,
        })
    }

    /// Appends constraints for the given conditional
    ///
    /// If return_type is None, the conditional must result in a unit type
    fn append_cond<'s>(
        &mut self,
        cond: &'a ast::Cond<'a>,
        // The type expected from the conditional blocks
        return_type: Option<TyVar>,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::Cond<'a>, Error> {
        let ast::Cond {conds, else_body} = cond;
        debug_assert!(!conds.is_empty(), "bug: conditional had no initial if block");

        let no_return_type = return_type.is_none();
        // Create a fresh type variable if none was provided
        let return_type = return_type.unwrap_or_else(|| self.constraints.fresh_type_var());

        // If the condition is used as a statement (no return type) or if there is no else clause,
        // the if condition must return unit
        if no_return_type || cond.else_body.is_none() {
            self.constraints.ty_var_is_ty(return_type, self.prims.unit())?;
        }

        let conds = conds.iter().map(|(cond, body)| {
            // Every condition must evaluate to a value of type bool
            let cond_var = self.constraints.fresh_type_var();
            self.constraints.ty_var_is_ty(cond_var, self.prims.bool())?;
            let cond = self.append_expr(cond, cond_var, scope)?;

            // The body of every condition must evaluate to the same type
            let mut child_scope = scope.child_scope();
            let body = self.append_block(body, return_type, &mut child_scope)?;

            Ok((cond, body))
        }).collect::<Result<Vec<_>, _>>()?;

        // The body of the else clause must evaluate to the same type as the other condition bodies
        let else_body = else_body.as_ref().map(|else_body| {
            let mut child_scope = scope.child_scope();
            self.append_block(else_body, return_type, &mut child_scope)
        }).transpose()?;

        Ok(tyir::Cond {conds, else_body})
    }

    /// Appends constraints for the given function call
    fn append_func_call<'s>(
        &mut self,
        call: &'a ast::CallExpr<'a>,
        // The type expected from the call expression
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::CallExpr<'a>, Error> {
        let ast::CallExpr {func_name, args} = call;

        let sig = match &func_name.components[..] {
            [] => unreachable!(),
            [func_name] => self.decls.func_sig(func_name)
                .context(UnresolvedFunction {name: *func_name})?,
            [ty_name, func_name] => {
                let ty_id = self.decls.type_id(ty_name).context(UnresolvedType {name: *ty_name})?;
                self.decls.method_sig(ty_id, func_name)
                    .context(UnresolvedFunction {name: *func_name})?
            },
            _ => return Err(Error::UnresolvedFunction {name: func_name.to_string()}),
        };

        self.append_func_call_sig(sig, func_name.clone(), args, None, return_type, scope)
    }

    /// Appends constraints for the given function call given the signature
    fn append_func_call_sig<'s>(
        &mut self,
        sig: &ast::FuncSig,
        // The function name to call, not necessarily the original function/method name
        func_name: ast::IdentPath<'a>,
        args: &'a [ast::Expr<'a>],
        // An extra argument to prepend on to the list of arguments passed to the call
        // Used to implement methods with a `self` parameter
        extra_first_arg: Option<tyir::Expr<'a>>,
        // The type expected from the call expression
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
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
        let ir::FuncSig {return_type: call_return_type, params} = self.resolve_sig(sig)?;
        self.constraints.ty_var_is_ty(return_type, call_return_type)?;

        let args = extra_first_arg.map(Ok).into_iter().chain(args.iter().zip(params).map(|(arg, param)| {
            let arg_ty_var = self.constraints.fresh_type_var();

            // Assert that each argument matches the corresponding parameter type
            let ir::FuncParam {name: _, ty: param_ty} = param;
            self.constraints.ty_var_is_ty(arg_ty_var, param_ty)?;

            self.append_expr(arg, arg_ty_var, scope)
        })).collect::<Result<Vec<_>, _>>()?;

        Ok(tyir::CallExpr {
            func_name,
            args,
        })
    }

    /// Appends constraints for the given assignment expression
    fn append_var_assign<'s>(
        &mut self,
        assign: &'a ast::VarAssign<'a>,
        // The type expected from the assignment expression
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
    ) -> Result<tyir::VarAssign<'a>, Error> {
        let ast::VarAssign {lhs, expr} = assign;

        // The return type of a variable assignment is ()
        self.constraints.ty_var_is_ty(return_type, self.prims.unit())?;

        // Get the type variable for the lhs expression
        let (lhs, lvalue_ty_var) = match lhs {
            ast::LValueExpr::FieldAccess(access) => {
                let field_ty_var = self.constraints.fresh_type_var();
                let access = self.append_field_access(access, field_ty_var, scope)?;
                let field_lvalue = tyir::LValueExpr::FieldAccess(access, field_ty_var);
                (field_lvalue, field_ty_var)
            },

            ast::LValueExpr::Var(ident) => {
                let var_ty_var = scope.get(ident).context(UnresolvedName {name: *ident})?;
                let var_lvalue = tyir::LValueExpr::Var(ident, var_ty_var);
                (var_lvalue, var_ty_var)
            },
        };

        // The type of the right-hand expression of the assignment must match the type of
        // the lvalue on the left
        let expr = self.append_expr(expr, lvalue_ty_var, scope)?;

        Ok(tyir::VarAssign {lhs, expr})
    }

    /// Appends constraints for the given return expression
    fn append_return<'s>(
        &mut self,
        ret_expr: Option<&'a ast::Expr<'a>>,
        // The type expected from the return expression
        return_type: TyVar,
        scope: &mut Scope<'a, 's>,
    ) -> Result<Option<tyir::Expr<'a>>, Error> {
        // A return expression always type checks to ()
        self.constraints.ty_var_is_ty(return_type, self.prims.unit())?;

        Ok(match ret_expr {
            // The return expression must match the type returned from the function
            Some(ret_expr) => {
                Some(self.append_expr(ret_expr, self.func_return_type, scope)?)
            },
            // No return expression, thus the function must be returning unit
            None => {
                self.constraints.ty_var_is_ty(self.func_return_type, self.prims.unit())?;

                None
            },
        })
    }

    /// Resolves all of the types in a function signature
    fn resolve_sig(&self, sig: &'a ast::FuncSig<'a>) -> Result<ir::FuncSig<'a>, Error> {
        let ast::FuncSig {return_type, params} = sig;
        let return_type = self.lookup_type(return_type)?;
        let params = params.iter().map(|param| {
            let ast::FuncParam {name, ty} = param;
            Ok(ir::FuncParam {name, ty: self.lookup_type(ty)?})
        }).collect::<Result<Vec<_>, _>>()?;

        Ok(ir::FuncSig {return_type, params})
    }

    /// Resolves a single type to either a declared type or a primitive
    fn lookup_type(&self, ty: &ast::Ty) -> Result<TyId, Error> {
        match ty {
            ast::Ty::Unit => Ok(self.prims.unit()),
            ast::Ty::SelfType => self.self_ty.context(UnresolvedType {name: "Self"}),
            ast::Ty::Named(ty) => self.decls.type_id(ty).context(UnresolvedType {name: *ty}),
        }
    }
}
