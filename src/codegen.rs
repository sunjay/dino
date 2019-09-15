//! Types for representing the minimal subset of C used for code generation.
//!
//! The "C" in the types either stands for "C" or for "codegen". The idea behind this module is to
//! only use a very general subset of C that could allow us to potentially switch to a different
//! code generation representation.

mod trans;
pub use trans::*;

use std::fmt;

use crate::runtime::RUNTIME_HEADER_FILENAME;

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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "#include \"{}\"\n", RUNTIME_HEADER_FILENAME)?;

        let Self {functions, entry_point} = self;

        // Output forward declarations so we don't have to worry about outputting the functions in
        // a specific order
        for func in functions {
            writeln!(f, "{};", func.sig)?;
        }

        // Write out entry point, which may rely on any number of the forward declarations
        writeln!(f, "{}", entry_point)?;

        // Finally, write out the code for each forward declared function
        for func in functions {
            writeln!(f, "{};", func)?;
        }

        Ok(())
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
        let Self {body} = self;

        // This is the only place where `int` is explicitly used. Use DInt everywhere else.
        writeln!(f, "int main(void) {{")?;
        writeln!(f, "{}", body)?;
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
        let Self {sig, body} = self;
        writeln!(f, "{} {{", sig)?;
        writeln!(f, "{}", body)?;
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
    /// The type returned from the function
    pub return_type: CType,
}

impl fmt::Display for CFunctionSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_name, return_type} = self;

        write!(f, "{} {}(", return_type, mangled_name)?;

        //TODO: Write out parameter types AND parameter names
        // if param_types.is_empty() {
        //     // Empty parentheses in C imply any number of arguments being allowed. Using `void`
        //     // is more explicit.
        //     write!(f, "void")?;
        // } else {
        //     // Need to avoid trailing commas
        //     write!(f, "{}", param_types[0])?;
        //
        //     for param_ty in &param_types[1..] {
        //         write!(f, ", {}", param_ty)?;
        //     }
        // }

        write!(f, ")")?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct CFunctionBody {
    pub stmts: Vec<CStmt>,
}

impl fmt::Display for CFunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {stmts} = self;

        for stmt in stmts {
            writeln!(f, "{}", stmt)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum CStmt {
    VarDecl(CVarDecl),
}

impl fmt::Display for CStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CStmt::*;
        match self {
            VarDecl(var_decl) => write!(f, "{}", var_decl),
        }
    }
}

#[derive(Debug)]
pub struct CVarDecl {
    /// The mangled name of the variable. This name MUST be unique within the boundary of the
    /// function block containing this declaration.
    ///
    /// In this case, "mangled" just refers to the fact that the symbol name has been changed from
    /// what it was in the original program to something more appropriate for code generation.
    pub mangled_name: String,
    /// The type of the variable
    pub ty: CType,
    /// The initializer expression for this variable
    pub init_expr: CInitializerExpr,
}

impl fmt::Display for CVarDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_name, ty, init_expr} = self;
        write!(f, "{} {} = {};", ty, mangled_name, init_expr)
    }
}

/// Special expressions that can be used to initialize a variable
#[derive(Debug)]
pub enum CInitializerExpr {
    Expr(CExpr),
}

impl fmt::Display for CInitializerExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CInitializerExpr::*;
        match self {
            Expr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug)]
pub enum CExpr {
    IntegerLiteral(i64),
}

impl fmt::Display for CExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CExpr::*;
        match self {
            IntegerLiteral(value) => write!(f, "{}", value),
        }
    }
}
