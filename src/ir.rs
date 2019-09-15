//! An intermediate representation of the program designed for easier code generation.
//!
//! By creating values of the types in this module, you guarantee that:
//! * All types are inferred and checked at this point
//! * Method resolution has been completed
//! * All declaration names are unique within any given module

pub use crate::ast::Ident;

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

impl<'a> Decl<'a> {
    pub fn name(&self) -> &Ident<'a> {
        use Decl::*;
        match self {
            Function(func) => &func.name,
        }
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
    VarDecl(VarDecl<'a>),
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    /// The identifier to assign a value to
    pub ident: Ident<'a>,
    /// The type of the identifier
    pub ty: Ident<'a>,
    /// The expression for the value to assign to the variable
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Expr {
    IntegerLiteral(i64),
}
