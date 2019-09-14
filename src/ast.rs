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

#[derive(Debug, Hash)]
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

#[derive(Debug, Hash)]
pub struct Function<'a> {
    pub name: Ident<'a>,
}

pub type Ident<'a> = &'a str;
