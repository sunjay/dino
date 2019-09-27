//! An intermediate representation of the program designed for easier code generation.
//!
//! By creating values of the types in this module, you guarantee that:
//! * All types are inferred and checked at this point
//! * Method resolution has been completed
//!     * Every call knows all its types and operators have been desugared
//! * All declaration names are unique within any given module

pub use crate::ast::Ident;

use crate::resolve::TyId;

#[derive(Debug)]
pub struct Program<'a> {
    pub top_level_module: Module<'a>,
}

#[derive(Debug)]
pub struct Module<'a> {
    pub decls: Vec<Decl<'a>>,
}

#[derive(Debug)]
pub enum Decl<'a> {
    Function(Function<'a>),
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub sig: FuncSig<'a>,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct FuncSig<'a> {
    pub return_type: TyId,
    pub params: Vec<FuncParam<'a>>,
}

#[derive(Debug)]
pub struct FuncParam<'a> {
    pub name: Ident<'a>,
    pub ty: TyId,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    VarDecl(VarDecl<'a>),
    Expr(Expr<'a>),
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    /// The identifier to assign a value to
    pub ident: Ident<'a>,
    /// The type of the identifier
    pub ty: TyId,
    /// The expression for the value to assign to the variable
    pub expr: Expr<'a>,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Call(CallExpr<'a>, TyId),
    IntegerLiteral(i64, TyId),
    RealLiteral(f64, TyId),
    ComplexLiteral(f64, TyId),
    Var(Ident<'a>, TyId),
}

#[derive(Debug)]
pub struct CallExpr<'a> {
    /// The name of the function to call
    pub func_name: Ident<'a>,
    /// The argument expressions to pass to the function
    pub args: Vec<Expr<'a>>,
}
