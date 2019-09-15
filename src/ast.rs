//! The abstract syntax tree of the program.
//!
//! This is the closest representation to the actual syntax.

mod parser;

pub use parser::Error as ParseError;

#[derive(Debug)]
pub struct Program<'a> {
    pub top_level_module: Module<'a>,
}

impl<'a> Program<'a> {
    pub fn parse(input: &'a str) -> Result<Self, ParseError> {
        Ok(Program {
            top_level_module: parser::parse_module(input)?,
        })
    }
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
    ident: Ident<'a>,
    /// The type of the identifier
    ty: Ident<'a>,
    /// The expression for the value to assign to the variable
    expr: Expr,
}

#[derive(Debug)]
pub enum Expr {
    IntegerLiteral(i64),
}

pub type Ident<'a> = &'a str;
