//! C IR - Directly represents C code
//!
//! Not all of C is implemented since only a very small subset is actually used during code
//! generation. This is a very flexible IR designed to allow us to generate whatever C code we want.
//! To that end, no special-casing is done specific to our compiler. It's just C.

mod csymbols;

pub use csymbols::*;

use std::fmt;
use std::borrow::Cow;

use crate::{cwrite, cwriteln};
use crate::fmt_ctx::DisplayCtx;

#[derive(Debug, Clone)]
pub struct Program {
    pub decls: Vec<Decl>,
}

impl DisplayCtx<CSymbols> for Program {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {decls} = self;
        cwrite!(f, ctx, "{}", Lines(decls))
    }
}

/// A declaration or other top-level C statement
#[derive(Debug, Clone)]
pub enum Decl {
    /// `#include` preprocessor directive with the given value in double quotes
    ///
    /// The string must NOT contain any newlines
    Include(Cow<'static, str>),
    /// Forward declaration of a `typedef struct`
    TypedefForwardDecl(TypedefForwardDecl),
    /// `typedef struct`
    Typedef(Typedef),
    /// Forward declaration of a function with only the function's signature
    FunctionForwardDecl(FunctionForwardDecl),
    /// Full function, with return type and body
    Function(Function),
    /// A comment starting with a double slash `//`
    ///
    /// The comment string must NOT contain newlines
    Comment(Cow<'static, str>),
    /// A blank line (helps make the generated code more readable)
    BlankLine,
}

impl DisplayCtx<CSymbols> for Decl {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use Decl::*;
        match self {
            Include(header) => cwrite!(f, ctx, "#include \"{}\"", header),
            TypedefForwardDecl(decl) => cwrite!(f, ctx, "{}", decl),
            Typedef(decl) => cwrite!(f, ctx, "{}", decl),
            FunctionForwardDecl(decl) => cwrite!(f, ctx, "{}", decl),
            Function(decl) => cwrite!(f, ctx, "{}", decl),
            Comment(decl) => cwrite!(f, ctx, "// {}", decl),
            // Blank means that we write nothing
            BlankLine => Ok(()),
        }
    }
}

/// Produces `typedef struct {struct_name} {typedef_name};`
#[derive(Debug, Clone)]
pub struct TypedefForwardDecl {
    /// The name of the struct being aliased
    pub struct_name: Ident,
    /// The name of the alias for the struct
    pub typedef_name: Ident,
}

impl DisplayCtx<CSymbols> for TypedefForwardDecl {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let TypedefForwardDecl {struct_name, typedef_name} = self;

        cwrite!(f, ctx, "typedef struct {} {};", struct_name, typedef_name)
    }
}

/// Produces `typedef {struct_type} {typedef_name};`
#[derive(Debug, Clone)]
pub struct Typedef {
    /// The name and full set of fields of a struct
    pub struct_type: Struct,
    /// The name of the alias for the struct
    pub typedef_name: Ident,
}

impl DisplayCtx<CSymbols> for Typedef {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Typedef {struct_type, typedef_name} = self;

        cwrite!(f, ctx, "typedef {} {};", struct_type, typedef_name)
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<StructField>,
}

impl DisplayCtx<CSymbols> for Struct {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, fields} = self;

        cwriteln!(f, ctx, "struct {} {{", name)?;
        cwrite!(f, ctx, "{}", Lines(fields))?;
        cwrite!(f, ctx, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Ident,
    pub ty: Type,
}

impl DisplayCtx<CSymbols> for StructField {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, ty} = self;

        cwrite!(f, ctx, "{} {};", ty, name)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionForwardDecl {
    pub sig: FuncSig,
}

impl DisplayCtx<CSymbols> for FunctionForwardDecl {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {sig} = self;

        cwrite!(f, ctx, "{};", sig)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub sig: FuncSig,
    pub body: Vec<Stmt>,
}

impl DisplayCtx<CSymbols> for Function {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Function {sig, body} = self;

        cwriteln!(f, ctx, "{} {{", sig)?;
        cwrite!(f, ctx, "{}", Lines(body))?;
        cwrite!(f, ctx, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    /// The name of the function
    pub name: FuncName,
    /// The return type of the function
    pub ret_type: Type,
    /// The parameters of the function
    pub params: Vec<FuncParam>,
}

impl DisplayCtx<CSymbols> for FuncSig {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, ret_type, params} = self;

        // Empty parentheses in C imply any number of arguments being allowed.
        // Using `void` is more explicit
        let params = Commas {values: params, empty: "void"};
        cwrite!(f, ctx, "{} {}({})", ret_type, name, params)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FuncName {
    Ident(Ident),
    Name(&'static str),
}

impl<'a, T: Into<FuncName> + Copy> From<&'a T> for FuncName {
    fn from(value: &'a T) -> Self {
        (*value).into()
    }
}

impl From<Ident> for FuncName {
    fn from(ident: Ident) -> Self {
        FuncName::Ident(ident)
    }
}

impl From<&'static str> for FuncName {
    fn from(name: &'static str) -> Self {
        FuncName::Name(name)
    }
}

impl DisplayCtx<CSymbols> for FuncName {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use FuncName::*;
        match self {
            Ident(ident) => cwrite!(f, ctx, "{}", ident),
            Name(name) => cwrite!(f, ctx, "{}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: Ident,
    pub ty: Type,
}

impl DisplayCtx<CSymbols> for FuncParam {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, ty} = self;

        cwrite!(f, ctx, "{} {}", ty, name)
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    VarDecl(VarDecl),
    FuncCall(FuncCall),
    Assign(Assign),
    If(Cond),
    WhileLoop(WhileLoop),
    Break,
    Continue,
    Return(Option<Expr>),
}

impl DisplayCtx<CSymbols> for Stmt {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use Stmt::*;
        match self {
            VarDecl(decl) => cwrite!(f, ctx, "{}", decl),
            FuncCall(call) => cwrite!(f, ctx, "{};", call),
            Assign(assign) => cwrite!(f, ctx, "{}", assign),
            If(cond) => cwrite!(f, ctx, "{}", cond),
            WhileLoop(wloop) => cwrite!(f, ctx, "{}", wloop),
            Break => cwrite!(f, ctx, "break;"),
            Continue => cwrite!(f, ctx, "continue;"),
            Return(expr) => match expr {
                Some(expr) => cwrite!(f, ctx, "return {};", expr),
                None => cwrite!(f, ctx, "return;"),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: Ident,
    pub ty: Type,
    /// The variable initilizer expression
    ///
    /// If not provided, the variable will be left uninitialized
    pub body: Option<Expr>,
}

impl DisplayCtx<CSymbols> for VarDecl {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, ty, body} = self;

        cwrite!(f, ctx, "{} {}", ty, name)?;

        match body {
            Some(expr) => cwrite!(f, ctx, " = {};", expr),
            None => cwrite!(f, ctx, ";"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    /// The name of the function to call
    pub name: FuncName,
    pub args: Vec<Expr>,
}

impl DisplayCtx<CSymbols> for FuncCall {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, args} = self;

        cwrite!(f, ctx, "{}({})", name, Commas {values: args, empty: ""})
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    /// The left-hand side of the assignment
    pub target: LValue,
    /// The right-hand side of the assignment
    pub value: Expr,
}

impl DisplayCtx<CSymbols> for Assign {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {target, value} = self;

        cwrite!(f, ctx, "{} = {};", target, value)
    }
}

#[derive(Debug, Clone)]
pub enum LValue {
    /// Writes to the variable with the given name, e.g. `foo = ...`
    Var(Ident),

    /// Writes to one of the fields of a variable using `->`, e.g. `foo->bar = ...`
    ///
    /// The type of the variable must be a pointer for this to work.
    Field {
        /// The name of the variable
        name: Ident,
        /// The name of the field to write to
        field: Ident,
    },

    /// Dereferences and writes to a variable, e.g. `*foo = ...`
    ///
    /// The type of the variable must be a pointer for this to work.
    Deref {
        /// The name of the variable to dereference
        name: Ident,
    },

    /// Dereferences a variable and writes to one of its fields using `->`, e.g. `(*foo)->bar = ...`
    ///
    /// The dereferenced value must have a pointer type for this to work.
    ///
    /// Could have nested `LValue` to model this, but having this special case avoids `Box`
    DerefField {
        /// The name of the variable to dereference
        name: Ident,
        /// The name of the field to access on the dereferenced variable
        field: Ident,
    },
}

impl DisplayCtx<CSymbols> for LValue {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use LValue::*;
        match *self {
            Var(name) => cwrite!(f, ctx, "{}", name),
            Field {name, field} => cwrite!(f, ctx, "{}->{}", name, field),
            Deref {name} => cwrite!(f, ctx, "*{}", name),
            DerefField {name, field} => cwrite!(f, ctx, "(*{})->{}", name, field),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Cond {
    /// The if condition (must evaluate to a `bool`)
    pub cond: Expr,
    /// The `if` body, executed if `cond` is true
    pub if_body: Vec<Stmt>,
    /// The `else` body, executed if `cond` is false
    pub else_body: Vec<Stmt>,
}

impl DisplayCtx<CSymbols> for Cond {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {cond, if_body, else_body} = self;
        cwriteln!(f, ctx, "if ({}) {{", cond)?;
        cwrite!(f, ctx, "{}", Lines(if_body))?;
        cwriteln!(f, ctx, "}} else {{")?;
        cwrite!(f, ctx, "{}", Lines(else_body))?;
        cwrite!(f, ctx, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    /// The loop condition (must evaluate to a `bool`)
    pub cond: Expr,
    pub body: Vec<Stmt>,
}

impl WhileLoop {
    pub fn infinite_loop(body: Vec<Stmt>) -> Self {
        Self {
            cond: Expr::Bool(true),
            body,
        }
    }
}

impl DisplayCtx<CSymbols> for WhileLoop {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {cond, body} = self;
        cwriteln!(f, ctx, "while ({}) {{", cond)?;
        cwrite!(f, ctx, "{}", Lines(body))?;
        cwrite!(f, ctx, "}}")
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    /// Just a variable
    Var(Ident),
    /// Accesses a field on a variable
    FieldAccess(FieldAccess),
    /// Calls a function
    FuncCall(FuncCall),
    /// Takes the address of a variable, e.g. `&var`
    AddrOf(Ident),
    /// Computes the `sizeof` a type
    Sizeof(Type),
    /// Byte string literal, e.g. `"abc"`
    BStr(BStrLiteral),
    /// 64-bit unsigned integer literal, e.g. `-139ULL`, `839090ULL`
    UInt64(u64),
    /// 64-bit integer literal, e.g. `-139LL`, `839090LL`
    Int64(i64),
    /// 32-bit integer literal, e.g. `-139`, `839090`
    Int32(i32),
    /// A double literal, e.g. `6.38`, `891.3e-10`
    Double(i64),
    /// A boolean literal, `true` or `false`
    Bool(bool),
}

impl DisplayCtx<CSymbols> for Expr {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use Expr::*;
        match self {
            Var(name) => cwrite!(f, ctx, "{}", name),
            FieldAccess(access) => cwrite!(f, ctx, "{}", access),
            FuncCall(call) => cwrite!(f, ctx, "{}", call),
            AddrOf(name) => cwrite!(f, ctx, "&{}", name),
            Sizeof(name) => cwrite!(f, ctx, "sizeof({})", name),
            BStr(lit) => cwrite!(f, ctx, "{}", lit),
            UInt64(lit) => cwrite!(f, ctx, "{}ULL", lit),
            Int64(lit) => cwrite!(f, ctx, "{}LL", lit),
            Int32(lit) => cwrite!(f, ctx, "{}", lit),
            Double(lit) => cwrite!(f, ctx, "{}", lit),
            Bool(lit) => cwrite!(f, ctx, "{}", lit),
        }
    }
}

/// Accesses a field on a variable using `->`
///
/// The variable must have a pointer type for this to work.
#[derive(Debug, Clone)]
pub struct FieldAccess {
    /// The name of the variable
    pub name: Ident,
    /// The name of the field to access
    pub field: Ident,
}

impl DisplayCtx<CSymbols> for FieldAccess {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, field} = self;
        cwrite!(f, ctx, "{}->{}", name, field)
    }
}

/// A null-terminated C byte-string literal with the given data.
///
/// When code is generated, special characters will be correctly escaped. (WIP)
///
/// The data is allowed to contain null characters.
#[derive(Debug, Clone)]
pub struct BStrLiteral(pub Vec<u8>);

impl DisplayCtx<CSymbols> for BStrLiteral {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, _ctx: &CSymbols) -> fmt::Result {
        let BStrLiteral(data) = self;
        write!(f, "\"")?;
        for &ch in data {
            match ch {
                //TODO: Escape more characeters (e.g. null, control chars, unicode, etc.)
                b'\\' => write!(f, "\\\\")?,
                b'"' => write!(f, "\\\"")?,
                b'\n' => write!(f, "\\n")?,
                b'\r' => write!(f, "\\r")?,
                b'\t' => write!(f, "\\t")?,
                _ => write!(f, "{}", ch as char)?,
            }
        }
        write!(f, "\"")
    }
}

// We have lots of special cases in this type to allow us to avoid `Box`. If this gets too hard to
// manage in the future, we may want to just switch to a more general type representation that
// allows arbitrary nesting (at the cost of allocations).
#[derive(Debug, Clone)]
pub enum Type {
    /// A simple named type, e.g. `Foo` (usually from a typedef)
    Named(Ident),
    /// A struct pointer type, e.g. `struct Foo *`
    StructPtr(Ident),
    /// A pointer to another type, e.g. `Foo *`
    Ptr(Ident),
    /// A pointer to a pointer of another type, e.g. `Foo **`
    OutPtr(Ident),
    /// A primitive `bool` type
    Bool,
    /// A primtiive `int` type (32-bit integer)
    Int32,
    /// The `void` type
    Void,
}

impl DisplayCtx<CSymbols> for Type {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use Type::*;
        match self {
            Named(name) => cwrite!(f, ctx, "{}", name),
            StructPtr(name) => cwrite!(f, ctx, "struct {}*", name),
            Ptr(name) => cwrite!(f, ctx, "{}*", name),
            OutPtr(name) => cwrite!(f, ctx, "{}**", name),
            Bool => cwrite!(f, ctx, "bool"),
            Int32 => cwrite!(f, ctx, "int"),
            Void => cwrite!(f, ctx, "void"),
        }
    }
}

/// Writes out a comma-separated list, ensuring that there is no trailing comma since that is not
/// supported in C.
struct Commas<'a, T: DisplayCtx<CSymbols>, D: DisplayCtx<CSymbols>> {
    /// The values to write out in a comma-separated list
    values: &'a [T],
    /// The default value to write out if the list of values is empty
    empty: D,
}

impl<'a, T: DisplayCtx<CSymbols>, D: DisplayCtx<CSymbols>> DisplayCtx<CSymbols> for Commas<'a, T, D> {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {values, empty} = self;

        if values.is_empty() {
            return cwrite!(f, ctx, "{}", empty);
        }

        cwrite!(f, ctx, "{}", values[0])?;
        for value in &values[1..] {
            cwrite!(f, ctx, ", {}", value)?;
        }

        Ok(())
    }
}

/// Writes out each item followed by a newline
struct Lines<'a, T: DisplayCtx<CSymbols>>(&'a [T]);

impl<'a, T: DisplayCtx<CSymbols>> DisplayCtx<CSymbols> for Lines<'a, T> {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let &Lines(values) = self;
        for value in values {
            cwriteln!(f, ctx, "{}", value)?;
        }
        Ok(())
    }
}
