//! An intermediate representation of the program designed for easier code generation
//!
//! * All types are inferred at this point
//! * Method resolution has been completed

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
