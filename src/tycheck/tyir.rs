//! An intermediate representation used during type checking to
//! provide a way to store fresh type variables and type IDs directly
//! without having to invent a way to uniquely address AST nodes.

use crate::ir;
use crate::ast::Ident;
use crate::resolve::TyId;

use super::constraints::{TyVar, TypeSubst};

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub body: Block<'a>,
}

impl<'a> Function<'a> {
    /// Applies the given substitution to this function and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Function<'a> {
        let Function {name, body} = self;
        ir::Function {
            name,
            body: body.apply_subst(subst),
        }
    }
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

impl<'a> Block<'a> {
    /// Applies the given substitution to this block and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Block<'a> {
        let Block {stmts} = self;
        ir::Block {
            stmts: stmts.into_iter().map(|stmt| stmt.apply_subst(subst)).collect(),
        }
    }
}

#[derive(Debug)]
pub enum Stmt<'a> {
    VarDecl(VarDecl<'a>),
    Expr(Expr<'a>),
}

impl<'a> Stmt<'a> {
    /// Applies the given substitution to this statement and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Stmt<'a> {
        use Stmt::*;
        match self {
            VarDecl(decl) => ir::Stmt::VarDecl(decl.apply_subst(subst)),
            Expr(expr) => ir::Stmt::Expr(expr.apply_subst(subst)),
        }
    }
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    /// The identifier to assign a value to
    pub ident: Ident<'a>,
    /// The type variable of this variable declaration
    pub ty_var: TyVar,
    /// The expression for the value to assign to the variable
    pub expr: Expr<'a>,
}

impl<'a> VarDecl<'a> {
    /// Applies the given substitution to this variable decl and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::VarDecl<'a> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    CallExpr(CallExpr<'a>, TyVar),
    IntegerLiteral(i64, TyVar),
    Var(Ident<'a>, TyVar),
}

impl<'a> Expr<'a> {
    /// Applies the given substitution to this expression and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Expr<'a> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct CallExpr<'a> {
    pub func_name: Ident<'a>,
    pub args: Vec<Expr<'a>>,
}

impl<'a> CallExpr<'a> {
    /// Applies the given substitution to this function call and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::CallExpr<'a> {
        unimplemented!()
    }
}

impl TyVar {
    /// Applies the given substitution to this type and returns the corresponding type ID
    pub fn apply_subst(self, subst: &TypeSubst) -> TyId {
        subst.get(&self).copied()
            .expect("bug: substitution did not contain all type variables")
    }
}
