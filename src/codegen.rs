//! Types for representing the minimal subset of C used for code generation.
//!
//! The "C" in the types either stands for "C" or for "codegen". The idea behind this module is to
//! only use a very general subset of C that could allow us to potentially switch to a different
//! code generation representation.

use std::fmt;

#[derive(Debug)]
pub enum CType {
    DInt,
}

impl fmt::Display for CType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            CType::DInt => "DInt",
        })
    }
}

/// Special wrapper for the entry point ("main") function. Deals with returning an integer
/// properly.
#[derive(Debug)]
pub struct CEntryPoint {
    pub body: CFunctionBody,
}

impl fmt::Display for CEntryPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // This is the only place where `int` is explicitly used. Use DInt everywhere else.
        writeln!(f, "int main(void) {{")?;
        writeln!(f, "{}", self.body)?;
        // Return an exit code of zero because if the program got to this point it succeeded
        writeln!(f, "return 0;")?;
        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct CFunction {
    pub name: String,
    pub return_type: CType,
    pub body: CFunctionBody,
}

impl fmt::Display for CFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} {}() {{", self.return_type, self.name)?;
        writeln!(f, "{}", self.body)?;
        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct CFunctionBody {
    //TODO
}

impl fmt::Display for CFunctionBody {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(()) //TODO
    }
}
