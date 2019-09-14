//! The abstract syntax tree of the program.
//!
//! This is the closest representation to the actual syntax.

#[derive(Debug)]
pub struct Program {
    pub decls: Vec<Decl>,
}

#[derive(Debug)]
pub enum Decl {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
}
