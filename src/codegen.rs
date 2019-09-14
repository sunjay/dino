//! Types for representing the minimal subset of C used for code generation.
//!
//! The "C" in the types either stands for "C" or for "codegen". The idea behind this module is to
//! only use a very general subset of C that could allow us to potentially switch to a different
//! code generation representation.

use std::fmt;

#[derive(Debug)]
pub enum CType {
    Int,
}

impl fmt::Display for CType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            CType::Int => "int",
        })
    }
}

#[derive(Debug)]
pub struct CFunction {
    pub return_type: CType,
    pub name: String,
}

impl fmt::Display for CFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} {}() {{", self.return_type, self.name)?;
        write!(f, "}}")
    }
}
