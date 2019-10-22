//! An intermediate representation used during type checking to provide a way to store fresh type
//! variables and type IDs directly without having to invent a way to uniquely address AST nodes.

use crate::ir;
use crate::ast::{Ident, IdentPath};
use crate::resolve::TyId;

use super::constraints::TyVar;
use super::subst::TypeSubst;

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub sig: ir::FuncSig<'a>,
    pub body: Block<'a>,
}

impl<'a> Function<'a> {
    /// Applies the given substitution to this function and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Function<'a> {
        let Self {name, sig, body} = self;
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
    /// The final statement of the block, used as the return value of the block
    pub ret: Option<Expr<'a>>,
    /// The type variable of the return expression (still provided even if the return expression
    /// is None)
    pub ret_ty_var: TyVar,
}

impl<'a> Block<'a> {
    /// Applies the given substitution to this block and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Block<'a> {
        let Self {stmts, ret, ret_ty_var} = self;
        ir::Block {
            stmts: stmts.into_iter().map(|stmt| stmt.apply_subst(subst)).collect(),
            ret: ret.map(|ret| ret.apply_subst(subst)),
            ret_ty: ret_ty_var.apply_subst(subst),
        }
    }
}

#[derive(Debug)]
pub enum Stmt<'a> {
    /// A conditional in statement position always has type unit
    Cond(Cond<'a>),
    WhileLoop(WhileLoop<'a>),
    VarDecl(VarDecl<'a>),
    Expr(Expr<'a>),
}

impl<'a> Stmt<'a> {
    /// Applies the given substitution to this statement and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Stmt<'a> {
        use Stmt::*;
        match self {
            Cond(cond) => ir::Stmt::Cond(cond.apply_subst(subst)),
            WhileLoop(wloop) => ir::Stmt::WhileLoop(wloop.apply_subst(subst)),
            VarDecl(decl) => ir::Stmt::VarDecl(decl.apply_subst(subst)),
            Expr(expr) => ir::Stmt::Expr(expr.apply_subst(subst)),
        }
    }
}

#[derive(Debug)]
pub struct WhileLoop<'a> {
    /// The condition for which the loop is expected to continue
    pub cond: Expr<'a>,
    /// The body of the loop, executed until the condition is false
    pub body: Block<'a>,
}

impl<'a> WhileLoop<'a> {
    /// Applies the given substitution to this while loop and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::WhileLoop<'a> {
        let Self {cond, body} = self;
        ir::WhileLoop {
            cond: cond.apply_subst(subst),
            body: body.apply_subst(subst),
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
        let Self {ident, ty_var, expr} = self;
        ir::VarDecl {
            ident,
            ty: ty_var.apply_subst(subst),
            expr: expr.apply_subst(subst),
        }
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    /// A conditional without an else clause always has type unit. With an else clause, the type of
    /// the conditional can be anything.
    Cond(Box<Cond<'a>>, TyVar),
    Call(CallExpr<'a>, TyVar),
    VarAssign(Box<VarAssign<'a>>, TyVar),
    Return(Option<Box<Expr<'a>>>, TyVar),
    BStrLiteral(&'a [u8], TyVar),
    IntegerLiteral(i64, TyVar),
    RealLiteral(f64, TyVar),
    ComplexLiteral(f64, TyVar),
    BoolLiteral(bool, TyVar),
    UnitLiteral(TyVar),
    Var(Ident<'a>, TyVar),
}

impl<'a> Expr<'a> {
    /// Applies the given substitution to this expression and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Expr<'a> {
        use Expr::*;
        match self {
            Cond(cond, ty_var) => {
                ir::Expr::Cond(Box::new(cond.apply_subst(subst)), ty_var.apply_subst(subst))
            },

            Call(call, ty_var) => {
                ir::Expr::Call(call.apply_subst(subst), ty_var.apply_subst(subst))
            },

            VarAssign(assign, ty_var) => {
                ir::Expr::VarAssign(Box::new(assign.apply_subst(subst)), ty_var.apply_subst(subst))
            },

            Return(ret_expr, ty_var) => {
                ir::Expr::Return(ret_expr.map(|expr| Box::new(expr.apply_subst(subst))), ty_var.apply_subst(subst))
            },

            BStrLiteral(value, ty_var) => {
                ir::Expr::BStrLiteral(value, ty_var.apply_subst(subst))
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

            UnitLiteral(ty_var) => {
                ir::Expr::UnitLiteral(ty_var.apply_subst(subst))
            },

            Var(var_name, ty_var) => {
                ir::Expr::Var(var_name, ty_var.apply_subst(subst))
            },
        }
    }
}

#[derive(Debug)]
pub struct Cond<'a> {
    /// A list of (condition, body) that corresponds to:
    /// if cond1 { body1 } else if cond2 { body2 } ...
    ///
    /// This must be non-empty (or else there would be no condition).
    pub conds: Vec<(Expr<'a>, Block<'a>)>,
    /// The `else` clause (if any)
    pub else_body: Option<Block<'a>>,
}

impl<'a> Cond<'a> {
    /// Applies the given substitution to this conditional and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::Cond<'a> {
        let Self {conds, else_body} = self;
        ir::Cond {
            conds: conds.into_iter().map(|(cond, body)| {
                (cond.apply_subst(subst), body.apply_subst(subst))
            }).collect(),
            else_body: else_body.map(|else_body| else_body.apply_subst(subst)),
        }
    }
}

#[derive(Debug)]
pub struct CallExpr<'a> {
    pub func_name: &'a IdentPath<'a>,
    pub args: Vec<Expr<'a>>,
}

impl<'a> CallExpr<'a> {
    /// Applies the given substitution to this function call and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::CallExpr<'a> {
        let Self {func_name, args} = self;
        ir::CallExpr {
            func_name,
            args: args.into_iter().map(|expr| expr.apply_subst(subst)).collect(),
        }
    }
}

#[derive(Debug)]
pub struct VarAssign<'a> {
    /// The identifier to assign a value to
    pub ident: Ident<'a>,
    /// The expression for the value to assign to the variable
    pub expr: Expr<'a>,
}

impl<'a> VarAssign<'a> {
    /// Applies the given substitution to this function call and returns the corresponding IR
    pub fn apply_subst(self, subst: &TypeSubst) -> ir::VarAssign<'a> {
        let Self {ident, expr} = self;
        ir::VarAssign {
            ident,
            expr: expr.apply_subst(subst),
        }
    }
}

impl TyVar {
    /// Applies the given substitution to this type and returns the corresponding type ID
    pub fn apply_subst(self, subst: &TypeSubst) -> TyId {
        subst.get(self)
            .expect("bug: substitution did not contain all type variables")
    }
}
