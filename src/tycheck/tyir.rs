//! An intermediate representation used during type checking to provide a way to store fresh type
//! variables and type IDs directly without having to invent a way to uniquely address AST nodes.

use crate::ir;
use crate::ast::Ident;
use crate::resolve::TyId;

use super::constraints::TyVar;
use super::solve::TypeSubst;

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub sig: ir::FuncSig<'a>,
    pub body: Block<'a>,
}

impl<'a> Function<'a> {
    /// Applies the given substitution to this function and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Function<'a> {
        let Function {name, sig, body} = self;
        ir::Function {
            name,
            sig,
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
        let VarDecl {ident, ty_var, expr} = self;
        ir::VarDecl {
            ident,
            ty: ty_var.apply_subst(subst),
            expr: expr.apply_subst(subst),
        }
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    Call(CallExpr<'a>, TyVar),
    IntegerLiteral(i64, TyVar),
    RealLiteral(f64, TyVar),
    ComplexLiteral(f64, TyVar),
    BoolLiteral(bool, TyVar),
    Var(Ident<'a>, TyVar),
}

impl<'a> Expr<'a> {
    /// Applies the given substitution to this expression and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Expr<'a> {
        use Expr::*;
        match self {
            Call(call, ty_var) => {
                ir::Expr::Call(call.apply_subst(subst), ty_var.apply_subst(subst))
            },

            IntegerLiteral(value, ty_var) => {
                ir::Expr::IntegerLiteral(value, ty_var.apply_subst(subst))
            },

            RealLiteral(value, ty_var) => {
                ir::Expr::RealLiteral(value, ty_var.apply_subst(subst))
            },

            ComplexLiteral(value, ty_var) => {
                ir::Expr::ComplexLiteral(value, ty_var.apply_subst(subst))
            },

            BoolLiteral(value, ty_var) => {
                ir::Expr::BoolLiteral(value, ty_var.apply_subst(subst))
            },

            Var(var_name, ty_var) => {
                ir::Expr::Var(var_name, ty_var.apply_subst(subst))
            },
        }
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
        let CallExpr {func_name, args} = self;
        ir::CallExpr {
            func_name,
            args: args.into_iter().map(|expr| expr.apply_subst(subst)).collect(),
        }
    }
}

impl TyVar {
    /// Applies the given substitution to this type and returns the corresponding type ID
    pub fn apply_subst(self, subst: &TypeSubst) -> TyId {
        subst.get(&self).copied()
            .expect("bug: substitution did not contain all type variables")
    }
}
