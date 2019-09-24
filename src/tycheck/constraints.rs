use std::collections::{HashSet, HashMap};

use snafu::{Snafu, OptionExt};
use maplit::hashset;

use crate::resolve::{DeclMap, Primitives, TyId};
use crate::ast;

use super::tyir;

/// Type inference and type checking errors
#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("cannot find value '{}' in this scope", name))]
    UnresolvedName {
        name: String,
    },
    #[snafu(display("cannot find type '{}' in this scope", name))]
    UnresolvedType {
        name: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(usize);

/// A type substitution that maps type variables to their concrete type
pub type TypeSubst = HashMap<TyVar, TyId>;

/// A map of variable names to their type variables
type LocalScope<'a> = HashMap<ast::Ident<'a>, TyVar>;

#[derive(Debug)]
enum Constraint {
    /// Asserts that a given type variable must be one of the given types
    TyVarOneOf {
        /// The type variable being constrained
        ty_var: TyVar,
        /// The set of types valid for this variable
        valid_tys: HashSet<TyId>,
    },

    /// Asserts that the given type variables must correspond to the same set of types
    TyVarEquals(TyVar, TyVar),
}

#[derive(Debug, Default)]
pub struct ConstraintSet {
    constraints: Vec<Constraint>,
    next_var: usize,
}

impl ConstraintSet {
    /// Generates a constraint set for the given function declaration. Any fresh type variables
    /// created are annotated inline into the returned `tyir::Function`
    pub fn generate<'a>(
        func: &'a ast::Function<'a>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<(Self, tyir::Function<'a>), Error> {
        let mut constraints = Self::default();

        //TODO: Figure out how to support nested scopes within a function (e.g. any curly braces)
        let mut local_scope = LocalScope::new();

        let ast::Function {name, body} = func;
        let body = constraints.append_block(body, &mut local_scope, decls, prims)?;
        Ok((constraints, tyir::Function {name, body}))
    }

    /// Attempts to solve the constraint set and return the solution as a substitution map
    pub fn solve(self) -> Result<TypeSubst, Error> {
        let subst = HashMap::new();
        println!("{:?}", self);
        //TODO
        Ok(subst)
    }

    /// Generates a fresh type variable and returns it
    fn fresh_type_var(&mut self) -> TyVar {
        let var = TyVar(self.next_var);
        self.next_var += 1;
        var
    }

    /// Appends constraints for the given block
    fn append_block<'a>(
        &mut self,
        block: &'a ast::Block<'a>,
        local_scope: &mut LocalScope<'a>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Block<'a>, Error> {
        let ast::Block {stmts} = block;
        Ok(tyir::Block {
            stmts: stmts.iter()
                .map(|stmt| self.append_stmt(stmt, local_scope, decls, prims))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    /// Appends constraints for the given statement
    fn append_stmt<'a>(
        &mut self,
        stmt: &'a ast::Stmt<'a>,
        local_scope: &mut LocalScope<'a>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Stmt<'a>, Error> {
        match stmt {
            ast::Stmt::VarDecl(decl) => self.append_var_decl(decl, local_scope, decls, prims)
                .map(tyir::Stmt::VarDecl),
            ast::Stmt::Expr(expr) => {
                // Generate a fresh variable that is never used after this point. By not using the
                // type variable, we indicate that the type of this expression does not matter.
                // Statement types do not matter because statements end with semicolons.
                let ty_var = self.fresh_type_var();
                self.append_expr(expr, ty_var, local_scope, decls, prims).map(tyir::Stmt::Expr)
            },
        }
    }

    /// Appends constraints for the given variable declaration
    fn append_var_decl<'a>(
        &mut self,
        var_decl: &'a ast::VarDecl<'a>,
        local_scope: &mut LocalScope<'a>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::VarDecl<'a>, Error> {
        let ast::VarDecl {ident, ty, expr} = var_decl;

        if local_scope.contains_key(ident) {
            //TODO: To support variable shadowing in this algorithm we just need to make sure that
            // statements are walked in order and that new variable declarations overwrite the
            // recorded types of previous declarations with a *fresh* variable.
            panic!("Variable shadowing is not supported yet.");
        }

        // Generate a fresh variable for the var decl
        let var_decl_ty_var = self.fresh_type_var();

        // The type variable should match the annotated type
        let var_decl_ty = decls.type_id(ty).context(UnresolvedType {name: *ty})?;
        self.constraints.push(Constraint::TyVarOneOf {
            ty_var: var_decl_ty_var,
            valid_tys: hashset!{var_decl_ty},
        });

        // Must append expr BEFORE updating local scope with the new type variable or else variable
        // shadowing will not work. Semantically, this variable does not come into scope until
        // *after* the variable expression has been evaluated.
        let expr = self.append_expr(expr, var_decl_ty_var, local_scope, decls, prims)?;

        // Associate the variable name with its type variable
        local_scope.insert(ident, var_decl_ty_var);

        Ok(tyir::VarDecl {
            ident,
            ty_var: var_decl_ty_var,
            expr,
        })
    }

    /// Appends constraints for the given expr
    fn append_expr<'a>(
        &mut self,
        expr: &'a ast::Expr<'a>,
        return_type: TyVar,
        local_scope: &mut LocalScope<'a>,
        decls: &DeclMap,
        prims: &Primitives,
    ) -> Result<tyir::Expr<'a>, Error> {
        match expr {
            ast::Expr::CallExpr(call) => unimplemented!(),
            &ast::Expr::IntegerLiteral(value) => {
                // Assert that the literal is one of the expected types for this kind of literal
                self.constraints.push(Constraint::TyVarOneOf {
                    ty_var: return_type,
                    valid_tys: hashset!{prims.int(), prims.real()},
                });
                Ok(tyir::Expr::IntegerLiteral(value, return_type))
            },
            ast::Expr::Var(name) => {
                // Assert that the type variable is equal to the return type variable
                let var_ty_var = local_scope.get(name).copied()
                    .with_context(|| UnresolvedName {name: name.to_string()})?;
                self.constraints.push(Constraint::TyVarEquals(var_ty_var, return_type));
                Ok(tyir::Expr::Var(name, var_ty_var))
            },
        }
    }
}
