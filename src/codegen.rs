//! Types for representing the minimal subset of C used for code generation.
//!
//! The "C" in the types either stands for "C" or for "codegen". The idea behind this module is to
//! only use a very general subset of C that could allow us to potentially switch to a different
//! code generation representation.
//!
//! All types here should directly map to concepts expressible in C. This is the last step in code
//! generation and no further processing should be required in order to convert these types to C.

mod trans;
pub use trans::*;

use std::fmt;

use crate::runtime::RUNTIME_HEADER_FILENAME;
use crate::dino_std::DINO_STD_HEADER_FILENAME;

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
        writeln!(f, "#include \"{}\"\n", DINO_STD_HEADER_FILENAME)?;

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
    pub return_type: String,
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
    Cond(CCond),
    VarDecl(CVarDecl),
    /// An expression followed by a semi-colon is a statement
    Expr(CExpr),
}

impl fmt::Display for CStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CStmt::*;
        match self {
            Cond(cond) => write!(f, "{}", cond),
            VarDecl(var_decl) => write!(f, "{}", var_decl),
            Expr(expr) => write!(f, "{};", expr),
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
    pub ty: String,
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
    Call(CCallExpr),
    IntegerLiteral(i64),
    DoubleLiteral(f64),
    BoolLiteral(bool),
    Var(String),
}

impl fmt::Display for CExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CExpr::*;
        match self {
            Call(call) => write!(f, "{}", call),
            // Since DInt is 64-bits, we need the LL suffix or the literal is not 64-bits wide.
            // https://en.cppreference.com/w/c/language/integer_constant
            IntegerLiteral(value) => write!(f, "{}LL", value),
            DoubleLiteral(value) => write!(f, "{}", value),
            BoolLiteral(value) => write!(f, "{}", value),
            Var(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug)]
pub struct CCond {
    /// A list of (condition, body) that corresponds to:
    /// if cond1 { body1 } else if cond2 { body2 } ...
    pub conds: Vec<(CExpr, Vec<CStmt>)>,
    /// The `else` clause (if any)
    pub else_body: Option<Vec<CStmt>>,
}

impl fmt::Display for CCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {conds, else_body} = self;

        for (i, (cond, body)) in conds.iter().enumerate() {
            if i != 0 {
                write!(f, " else ")?;
            }

            writeln!(f, "if ({}) {{", cond)?;
            for stmt in body {
                writeln!(f, "{}", stmt)?;
            }
            writeln!(f, "}}")?;
        }

        if let Some(else_body) = else_body {
            writeln!(f, "else {{")?;
            for stmt in else_body {
                writeln!(f, "{}", stmt)?;
            }
            writeln!(f, "}}")?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct CCallExpr {
    /// The name of the function to call
    pub func_name: String,
    /// The argument expressions to pass to the function
    pub args: Vec<CExpr>,
}

impl fmt::Display for CCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {func_name, args} = self;
        write!(f, "{}({})", func_name, Commas {values: args, empty: ""})?;
        Ok(())
    }
}

/// Writes out a comma-separated list, ensuring that there is no trailing comma since that is not
/// supported in C.
struct Commas<'a, T: fmt::Display, D: fmt::Display> {
    /// The values to write out in a comma-separated list
    values: &'a [T],
    /// The default value to write out if the list of values is empty
    empty: D,
}

impl<'a, T: fmt::Display, D: fmt::Display> fmt::Display for Commas<'a, T, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {values, empty} = self;

        if values.is_empty() {
            return write!(f, "{}", empty);
        }

        write!(f, "{}", values[0])?;
        for value in &values[1..] {
            write!(f, ", {}", value)?;
        }

        Ok(())
    }
}
