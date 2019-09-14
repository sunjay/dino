//! The abstract syntax tree of the program.
//!
//! This is the closest representation to the actual syntax.

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

pub type Ident<'a> = &'a str;
