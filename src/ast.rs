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
    Expr(Expr<'a>),
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    /// The identifier to assign a value to
    pub ident: Ident<'a>,
    /// The type of the identifier
    pub ty: Ident<'a>,
    /// The expression for the value to assign to the variable
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    CallExpr(CallExpr<'a>),
    IntegerLiteral(i64),
    Var(Ident<'a>),
}

#[derive(Debug, Clone)]
pub struct CallExpr<'a> {
    pub func_name: Ident<'a>,
    pub args: Vec<Expr<'a>>,
}

pub type Ident<'a> = &'a str;
