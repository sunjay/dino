//! Types for representing the minimal subset of C used for code generation.
//!
//! The "C" in the types either stands for "C" or for "codegen". The idea behind this module is to
//! only use a very general subset of C that could allow us to potentially switch to a different
//! code generation representation.

use std::fmt;

#[derive(Debug)]
pub struct CExecutableProgram {
    /// The list of functions, not including the entry point
    ///
    /// Each of these MUST have a unique name
    pub functions: Vec<CFunction>,
    /// The entry point ("main") function of the program
    pub entry_point: CEntryPoint,
}

impl fmt::Display for CExecutableProgram {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //TODO: Include runtime header
        //TODO: Generate forward declarations first, then entry point, then all functions
        unimplemented!()
    }
}

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

/// Special wrapper for the entry point ("main") function. Deals with properly returning an integer
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
    pub sig: CFunctionSignature,
    pub body: CFunctionBody,
}

impl fmt::Display for CFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} {{", self.sig)?;
        writeln!(f, "{}", self.body)?;
        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct CFunctionSignature {
    /// The mangled name of the function.
    ///
    /// In this case, "mangled" just refers to the fact that the symbol name has been changed from
    /// what it was in the original program to something more appropriate for code generation.
    pub mangled_name: String,
    pub return_type: CType,
}

impl fmt::Display for CFunctionSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}(", self.return_type, self.mangled_name)?;

        //TODO: Write out parameter types AND parameter names
        // if self.param_types.is_empty() {
        //     // Empty parentheses in C imply any number of arguments being allowed. Using `void`
        //     // is more explicit.
        //     write!(f, "void")?;
        // } else {
        //     // Need to avoid trailing commas
        //     write!(f, "{}", self.param_types[0])?;
        //
        //     for param_ty in &self.param_types[1..] {
        //         write!(f, ", {}", param_ty)?;
        //     }
        // }

        write!(f, ")")?;

        Ok(())
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
