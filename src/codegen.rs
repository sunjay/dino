//! Types for representing the minimal subset of C used for code generation.
//!
//! The "C" in the types either stands for "C" or for "codegen". The idea behind this module is to
//! only use a very general subset of C that could allow us to potentially switch to a different
//! code generation representation.
//!
//! All types here should directly map to concepts expressible in C. This is the last step in code
//! generation and no further processing should be required in order to convert these types to C.

use std::fmt;

use crate::gc_lib::GC_LIB_HEADER_FILENAME;
use crate::runtime::RUNTIME_HEADER_FILENAME;
use crate::dino_std::DINO_STD_HEADER_FILENAME;

#[derive(Debug)]
pub struct CStruct {
    /// The mangled name of the struct.
    ///
    /// In this case, "mangled" just refers to the fact that the symbol name has been changed from
    /// what it was in the original program to something more appropriate for code generation.
    pub mangled_name: String,
    pub fields: Vec<CStructField>,
}

impl fmt::Display for CStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_name, fields} = self;

        writeln!(f, "typedef struct {{")?;

        for field in fields {
            writeln!(f, "{},", field)?;
        }

        write!(f, "}} {}", mangled_name)
    }
}

#[derive(Debug)]
pub struct CStructField {
    /// The mangled name of the struct field.
    ///
    /// In this case, "mangled" just refers to the fact that the symbol name has been changed from
    /// what it was in the original program to something more appropriate for code generation.
    pub mangled_name: String,
    /// The type of the struct field
    pub ty: CTy,
}

impl fmt::Display for CStructField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_name, ty} = self;
        write!(f, "{}: {}", mangled_name, ty)
    }
}

#[derive(Debug)]
pub struct CExecutableProgram {
    /// The structs generated for the program
    pub structs: Vec<CStruct>,
    /// The list of functions, not including the entry point
    ///
    /// Each of these MUST have a unique name
    pub functions: Vec<CFunction>,
    /// The entry point ("main") function of the program
    pub entry_point: CEntryPoint,
}

impl fmt::Display for CExecutableProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "#include \"{}\"", GC_LIB_HEADER_FILENAME)?;
        writeln!(f, "#include \"{}\"", RUNTIME_HEADER_FILENAME)?;
        writeln!(f, "#include \"{}\"\n", DINO_STD_HEADER_FILENAME)?;

        let Self {structs, functions, entry_point} = self;

        for struct_decl in structs {
            writeln!(f, "{}", struct_decl)?;
        }

        // Output forward declarations so we don't have to worry about outputting the functions in
        // a specific order
        for func in functions {
            writeln!(f, "{};", func.sig)?;
        }

        // Write out entry point, which may rely on any number of the forward declarations
        writeln!(f, "{}", entry_point)?;

        // Finally, write out the code for each forward declared function
        for func in functions {
            writeln!(f, "{}", func)?;
        }

        Ok(())
    }
}

/// Special wrapper for the entry point ("main") function. Deals with properly returning an integer
#[derive(Debug)]
pub struct CEntryPoint {
    pub body: CStmts,
}

impl fmt::Display for CEntryPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {body} = self;

        // The dino entry point returns unit, but the C entry point needs to return int.
        // This generates a special function which is then called from the C entry point.
        //TODO: Do not hard code DUnit here
        writeln!(f, "DUnit* __dino__main(void) {{")?;
        writeln!(f, "{}", body)?;
        writeln!(f, "}}")?;

        // The "actual" C entry point
        // This is the only place where `int` is explicitly used. Use DInt everywhere else.
        writeln!(f, "int main(void) {{")?;
        writeln!(f, "__dino__main();")?;
        // Return an exit code of zero because if the program got to this point it succeeded
        writeln!(f, "return 0;")?;
        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct CFunction {
    pub sig: CFunctionSignature,
    pub body: CStmts,
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
    pub return_type: CTy,
    /// The parameters of the function
    pub params: Vec<CFunctionParam>,
}

impl fmt::Display for CFunctionSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_name, params, return_type} = self;

        //TODO: Use a better calling convention that allows every function to return void and uses
        // out pointers instead

        // Empty parentheses in C imply any number of arguments being allowed.
        // Using `void` is more explicit
        let params = Commas {values: params, empty: "void"};
        write!(f, "{} {}({})", return_type, mangled_name, params)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct CFunctionParam {
    /// The mangled name of the function parameter.
    ///
    /// In this case, "mangled" just refers to the fact that the symbol name has been changed from
    /// what it was in the original program to something more appropriate for code generation.
    pub mangled_name: String,
    /// The type of the function parameter
    pub ty: CTy,
}

impl fmt::Display for CFunctionParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_name, ty} = self;
        writeln!(f, "{} {}", ty, mangled_name)
    }
}

/// A list of C statements
#[derive(Debug, Default)]
pub struct CStmts(pub Vec<CStmt>);

impl Extend<CStmt> for CStmts {
    fn extend<T: IntoIterator<Item = CStmt>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

impl IntoIterator for CStmts {
    type Item = <Vec<CStmt> as IntoIterator>::Item;
    type IntoIter = <Vec<CStmt> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl fmt::Display for CStmts {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let CStmts(stmts) = self;

        for stmt in stmts {
            writeln!(f, "{}", stmt)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum CStmt {
    /// A conditional statement
    Cond(CCond),
    /// An infinite loop
    Loop(CInfiniteLoop),
    /// The only way to stop a loop
    BreakLoop,
    /// A variable assignment of the form `var-name = value-expr;`
    VarAssign(CVarAssign),
    /// A variable declaration of the form `type-name var-name = value-expr;`
    VarDecl(CVarDecl),
    /// A temporary variable, generated by the compiler
    TempVarDecl(CTempVarDecl),
    /// An expression followed by a semi-colon is a statement
    Expr(CExpr),
    /// A `return expr;` statement. The expression is not optional because every function at least
    /// returns unit.
    Return(CExpr),
}

impl fmt::Display for CStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CStmt::*;
        match self {
            Cond(cond) => write!(f, "{}", cond),
            Loop(iloop) => write!(f, "{}", iloop),
            BreakLoop => write!(f, "break;"),
            VarAssign(var_assign) => write!(f, "{};", var_assign),
            VarDecl(var_decl) => write!(f, "{}", var_decl),
            TempVarDecl(temp_var_decl) => write!(f, "{}", temp_var_decl),
            Expr(expr) => write!(f, "{};", expr),
            Return(expr) => write!(f, "return {};", expr),
        }
    }
}

/// An infinite loop. Using `while (cond)` loops is not permitted because the condition of the loop
/// may be non-trivial. Making a while loop unrepresentable forces us to think about this.
#[derive(Debug)]
pub struct CInfiniteLoop {
    /// The body of the loop, executed until the loop is explicitly stopped with `break`
    pub body: CStmts,
}

impl fmt::Display for CInfiniteLoop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {body} = self;
        writeln!(f, "while (true) {{")?;
        writeln!(f, "{}", body)?;
        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct CVarAssign {
    pub lvalue: CLValue,
    /// The initializer expression for this variable
    pub init_expr: CInitializerExpr,
}

impl fmt::Display for CVarAssign {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {lvalue, init_expr} = self;
        write!(f, "{} = {}", lvalue, init_expr)
    }
}

#[derive(Debug)]
pub enum CLValue {
    FieldAccess(CFieldAccess),
    Var {
        /// The mangled name of the variable to assign to.
        ///
        /// In this case, "mangled" just refers to the fact that the symbol name has been changed
        /// from what it was in the original program to something more appropriate for code
        /// generation.
        mangled_name: String,
    },
}

impl fmt::Display for CLValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CLValue::*;
        match self {
            FieldAccess(access) => write!(f, "{}", access),
            Var {mangled_name} => write!(f, "{}", mangled_name),
        }
    }
}

#[derive(Debug)]
pub struct CFieldAccess {
    /// The expression of the left-hand side of the field access
    ///
    /// This expression is assumed to evaluate to a pointer
    //TODO: Enforce that this is a pointer with the type system using an enum that limits which
    //  variants of CExpr can be used here
    pub lhs: CExpr,
    /// The mangled name of the field.
    ///
    /// In this case, "mangled" just refers to the fact that the symbol name has been changed from
    /// what it was in the original program to something more appropriate for code generation.
    pub field_mangled_name: String,
}

impl fmt::Display for CFieldAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {lhs, field_mangled_name} = self;
        write!(f, "{}->{}", lhs, field_mangled_name)
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
    pub ty: CTy,
    /// The initializer expression for this variable
    pub init_expr: CInitializerExpr,
}

impl fmt::Display for CVarDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_name, ty, init_expr} = self;
        write!(f, "{} {} = {};", ty, mangled_name, init_expr)
    }
}

/// The declaration of a temporary variable generated by the compiler.
///
/// The main difference between this and `CVarDecl` is that this variable is allowed to be
/// uninitialized.
#[derive(Debug)]
pub struct CTempVarDecl {
    /// The mangled name of the variable to assign to.
    ///
    /// In this case, "mangled" just refers to the fact that the symbol name has been changed from
    /// what it was in the original program to something more appropriate for code generation.
    pub mangled_name: String,
    /// The type of the variable
    pub ty: CTy,
    /// The initializer expression for this variable
    pub init_expr: Option<CInitializerExpr>,
}

impl fmt::Display for CTempVarDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_name, ty, init_expr} = self;

        write!(f, "{} {}", ty, mangled_name)?;

        match init_expr {
            Some(init_expr) => write!(f, " = {};", init_expr),
            None => write!(f, ";"),
        }
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
    FieldAccess(Box<CFieldAccess>),
    /// A null-terminated C byte string literal with the given data.
    /// The data is allowed to contain null characters.
    //TODO: Avoid having to copy the data into a Vec
    NTStrLiteral(Vec<u8>),
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
            FieldAccess(access) => write!(f, "{}", access),
            NTStrLiteral(data) => {
                write!(f, "(const unsigned char *)\"")?;
                for &ch in data {
                    match ch {
                        b'\\' => write!(f, "\\\\")?,
                        b'"' => write!(f, "\\\"")?,
                        b'\n' => write!(f, "\\n")?,
                        b'\r' => write!(f, "\\r")?,
                        b'\t' => write!(f, "\\t")?,
                        _ => write!(f, "{}", ch as char)?,
                    }
                }
                write!(f, "\"")
            },
            // Since DInt is 64-bits, we need the LL suffix or the literal is not 64-bits wide.
            // https://en.cppreference.com/w/c/language/integer_constant
            IntegerLiteral(value) => write!(f, "{}LL", value),
            DoubleLiteral(value) => write!(f, "{}", value),
            BoolLiteral(value) => write!(f, "{}", value),
            Var(name) => write!(f, "{}", name),
        }
    }
}

/// A single if-else clause. Note that we are not using `else if` because it makes code generation
/// more difficult when we want to lazily evaluate the `else if` condition expressions.
#[derive(Debug)]
pub struct CCond {
    /// The conditional expression that will be branched on
    ///
    /// Note that this expression should evaluate to a C bool value
    pub cond_expr: CExpr,
    /// The statements to execute if the condition is true
    pub if_body: CStmts,
    /// The statements to execute if the condition is false (if any)
    pub else_body: Option<CStmts>,
}

impl fmt::Display for CCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {cond_expr, if_body, else_body} = self;

        writeln!(f, "if ({}) {{", cond_expr)?;
        writeln!(f, "{}", if_body)?;
        match else_body {
            Some(else_body) => {
                writeln!(f, "}} else {{")?;
                writeln!(f, "{}", else_body)?;
                writeln!(f, "}}")?;
            },
            None => {
                writeln!(f, "}}")?;
            },
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct CCallExpr {
    /// The mangled name of the function to call
    ///
    /// In this case, "mangled" just refers to the fact that the symbol name has been changed from
    /// what it was in the original program to something more appropriate for code generation.
    pub mangled_func_name: String,
    /// The argument expressions to pass to the function
    pub args: Vec<CExpr>,
}

impl fmt::Display for CCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {mangled_func_name, args} = self;
        write!(f, "{}({})", mangled_func_name, Commas {values: args, empty: ""})?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum CTy {
    Named {mangled_name: String},
    Pointer(Box<CTy>),
}

impl CTy {
    pub fn pointer(mangled_name: String) -> Self {
        CTy::Pointer(Box::new(CTy::Named {mangled_name}))
    }
}

impl fmt::Display for CTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CTy::Named {mangled_name} => write!(f, "{}", mangled_name),
            CTy::Pointer(ty) => write!(f, "{}*", ty),
        }
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
