//! An intermediate representation of the program designed for easier code generation.
//!
//! * All types are inferred and checked at this point
//! * Method resolution has been completed

use snafu::Snafu;

use crate::ast;
pub use crate::ast::Ident;

/// Errors encountered when lowering AST to IR
#[derive(Debug, Snafu)]
pub enum Error {
}

#[derive(Debug)]
pub struct Program<'a> {
    pub top_level_module: Module<'a>,
}

impl<'a> Program<'a> {
    pub fn from_ast(prog: ast::Program<'a>) -> Result<Self, Error> {
        Ok(Self {
            top_level_module: Module::from_ast(prog.top_level_module)?,
        })
    }
}

#[derive(Debug)]
pub struct Module<'a> {
    pub decls: Vec<Decl<'a>>,
}

impl<'a> Module<'a> {
    pub fn from_ast(module: ast::Module<'a>) -> Result<Self, Error> {
        Ok(Self {
            decls: module.decls.into_iter().map(Decl::from_ast).collect::<Result<_, _>>()?,
        })
    }
}

#[derive(Debug)]
pub enum Decl<'a> {
    Function(Function<'a>),
}

impl<'a> Decl<'a> {
    pub fn from_ast(decl: ast::Decl<'a>) -> Result<Self, Error> {
        Ok(match decl {
            ast::Decl::Function(func) => Decl::Function(Function::from_ast(func)?),
        })
    }
}

#[derive(Debug)]
pub struct Function<'a> {
    pub name: Ident<'a>,
}

impl<'a> Function<'a> {
    pub fn from_ast(func: ast::Function<'a>) -> Result<Self, Error> {
        Ok(Self {
            name: func.name,
        })
    }
}
