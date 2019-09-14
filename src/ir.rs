//! An intermediate representation of the program designed for easier code generation
//!
//! * All types are inferred at this point
//! * Method resolution has been completed

use crate::ast::Ident;

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
}
