//! C IR - Types for representing the minimal subset of C used for code generation.
//!
//! All types here should directly map to concepts expressible in C. This is the last step in code
//! generation and no further processing should be required in order to convert these types to C.

use std::fmt;

use crate::{cwrite, cwriteln};
use crate::runtime::ALLOCATE;
use crate::fmt_ctx::DisplayCtx;
use crate::symbol_table::{SymbolTable, SymId, GenId};

/// Represents a complete C program with an optional entry point (for executables)
///
/// The program is written as follows:
/// 1. Forward declarations for all structs
/// 2. All struct declarations
/// 3. Forward declarations for all functions
/// 4. All function declarations
///
/// Printing forward declarations first enables structs and functions to refer to each other
/// without forcing us to write them out in dependency order. This even works if there are cyclic
/// dependencies between functions and types.
#[derive(Debug, Clone)]
pub struct CProgram {
    pub structs: Vec<CStruct>,
    pub functions: Vec<CFunction>,
    /// If provided, the program will end with an entry point (`int main`) that calls the given
    /// function with no parameters
    pub entry_point: Option<CIdent>,
}

impl DisplayCtx<CSymbols> for CProgram {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {structs, functions, entry_point} = self;

        cwriteln!(f, ctx, "// struct forward declarations")?;
        for struct_decl in structs {
            cwriteln!(f, ctx, "{}", struct_decl.forward_decl())?;
        }
        cwriteln!(f, ctx)?;

        cwriteln!(f, ctx, "// struct declarations")?;
        for struct_decl in structs {
            cwriteln!(f, ctx, "{}", struct_decl)?;
        }
        cwriteln!(f, ctx)?;

        cwriteln!(f, ctx, "// function forward declarations")?;
        for func in functions {
            cwriteln!(f, ctx, "{}", func.forward_decl())?;
        }
        cwriteln!(f, ctx)?;

        cwriteln!(f, ctx, "// function declarations")?;
        for func in functions {
            cwriteln!(f, ctx, "{}", func)?;
        }
        cwriteln!(f, ctx)?;

        if let Some(entry_point) = entry_point {
            cwriteln!(f, ctx, "int main() {{")?;
            cwriteln!(f, ctx, "    {}();", entry_point)?;
            cwriteln!(f, ctx, "    return 0;")?;
            cwriteln!(f, ctx, "}}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct CStruct {
    pub name: CIdent,
    /// If any of the fields have the same name as the struct, the
    /// `struct` C keyword will be added before them
    pub fields: Vec<CStructField>,
}

impl CStruct {
    fn forward_decl(&self) -> ForwardDecl<Self> {
        ForwardDecl {value: self}
    }
}

impl<'a> DisplayCtx<CSymbols> for ForwardDecl<'a, CStruct> {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let CStruct {name, fields: _} = self.value;
        cwrite!(f, ctx, "typedef struct {} {};", name, name)
    }
}

impl DisplayCtx<CSymbols> for CStruct {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, fields} = self;
        cwriteln!(f, ctx, "typedef struct {} {{", name)?;
        for field in fields {
            let CStructField {name: field_name, ptr_typ} = field;
            if ptr_typ == name {
                // Recursive type
                cwriteln!(f, ctx, "struct {}* {};", ptr_typ, field_name)?;
            } else {
                cwriteln!(f, ctx, "{}* {};", ptr_typ, field_name)?;
            }
        }
        cwrite!(f, ctx, "}} {};", name)
    }
}

/// All struct fields are pointers to a type
#[derive(Debug, Clone)]
pub struct CStructField {
    pub name: CIdent,
    /// The type of the pointer stored in this field
    pub ptr_typ: CIdent,
}

#[derive(Debug, Clone)]
pub struct CFunction {
    pub name: CIdent,
    pub params: Vec<CFuncParam>,
    pub body: Vec<CStmt>,
}

impl CFunction {
    fn forward_decl(&self) -> ForwardDecl<Self> {
        ForwardDecl {value: self}
    }
}

impl<'a> DisplayCtx<CSymbols> for ForwardDecl<'a, CFunction> {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let CFunction {name, params, body: _} = self.value;
        cwrite!(f, ctx, "void {}({});", name, Commas {values: params, empty: "void"})
    }
}

impl DisplayCtx<CSymbols> for CFunction {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, params, body} = self;
        cwriteln!(f, ctx, "void {}({}) {{", name, Commas {values: params, empty: "void"})?;
        for stmt in body {
            cwriteln!(f, ctx, "{}", stmt)?;
        }
        cwrite!(f, ctx, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct CFuncParam {
    pub name: CIdent,
    pub typ: CFuncParamType,
}

impl DisplayCtx<CSymbols> for CFuncParam {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, typ} = self;
        cwrite!(f, ctx, "{} {}", typ, name)
    }
}

#[derive(Debug, Clone)]
pub enum CFuncParamType {
    /// A pointer to the given type
    InPtr {typ: CIdent},
    /// An out pointer to the given type (i.e. pointer to a pointer)
    OutPtr {typ: CIdent},
}

impl DisplayCtx<CSymbols> for CFuncParamType {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CFuncParamType::*;
        match self {
            InPtr {typ} => cwrite!(f, ctx, "{}*", typ),
            OutPtr {typ} => cwrite!(f, ctx, "{}**", typ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CStmt {
    /// An uninitialized variable declaration
    VarDecl(CVarDecl),
    /// Calls a function
    FuncCall(CFuncCall),
    /// Variable Assignment
    Assign(CAssign),
}

impl DisplayCtx<CSymbols> for CStmt {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CStmt::*;
        match self {
            VarDecl(decl) => cwrite!(f, ctx, "{};", decl),
            FuncCall(decl) => cwrite!(f, ctx, "{};", decl),
            Assign(decl) => cwrite!(f, ctx, "{};", decl),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CVarDecl {
    pub name: CIdent,
    pub typ: CVarType,
}

impl DisplayCtx<CSymbols> for CVarDecl {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {name, typ} = self;
        cwrite!(f, ctx, "{} {}", typ, name)
    }
}

#[derive(Debug, Clone)]
pub enum CVarType {
    /// A pointer to the given type
    Ptr {typ: CIdent},
    /// A plain `bool` type
    Bool,
}

impl DisplayCtx<CSymbols> for CVarType {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CVarType::*;
        match self {
            Ptr {typ} => cwrite!(f, ctx, "{}*", typ),
            Bool => cwrite!(f, ctx, "bool"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CFuncCall {
    pub func_name: CIdent,
    pub args: Vec<CFuncArg>,
}

impl DisplayCtx<CSymbols> for CFuncCall {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {func_name, args} = self;
        cwrite!(f, ctx, "{}({})", func_name, Commas {values: args, empty: ""})
    }
}

#[derive(Debug, Clone)]
pub enum CFuncArg {
    /// A plain variable passed as the argument
    Var(CIdent),
    /// An argument to an out parameter (pointer to the given variable)
    OutVar(CIdent),
}

impl DisplayCtx<CSymbols> for CFuncArg {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CFuncArg::*;
        match self {
            Var(var) => cwrite!(f, ctx, "{}", var),
            OutVar(var) => cwrite!(f, ctx, "&{}", var),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CAssign {
    /// The left-hand side of the assignment
    pub target: CAssignTarget,
    /// The right-hand side of the assignment
    pub value: CAssignValue,
}

impl DisplayCtx<CSymbols> for CAssign {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        let Self {target, value} = self;
        cwrite!(f, ctx, "{} = {}", target, value)
    }
}

#[derive(Debug, Clone)]
pub enum CAssignTarget {
    /// Dereferences and writes to an out pointer
    OutPtr {
        /// The name of the variable that is an out pointer
        name: CIdent,
    },
    /// Dereferences an out pointer and writes to one of its fields
    OutPtrField {
        /// The name of the variable that is an out pointer
        name: CIdent,
        /// The name of the field to write to
        field: CIdent,
    },
    /// Writes to one of the fields of an in-pointer
    InPtrField {
        /// The name of the variable that is an in-pointer
        name: CIdent,
        /// The name of the field to write to
        field: CIdent,
    },
    /// Writes to the variable with the given name
    Var {
        /// The name of the variable
        name: CIdent,
    },
}

impl DisplayCtx<CSymbols> for CAssignTarget {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CAssignTarget::*;
        match *self {
            OutPtr {name} => cwrite!(f, ctx, "*{}", name),
            OutPtrField {name, field} => cwrite!(f, ctx, "(*{})->{}", name, field),
            InPtrField {name, field} => cwrite!(f, ctx, "{}->{}", name, field),
            Var {name} => cwrite!(f, ctx, "{}", name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CAssignValue {
    /// Allocates memory for the size of the given type
    Alloc {
        typ: CIdent,
    },
    /// Accesses a field on an in-pointer
    FieldAccess {
        /// The value to access a field from
        value: CIdent,
        /// The name of the field to access
        field: CIdent,
    },
    /// Accesses a plain variable
    Var {
        /// The name of the variable
        name: CIdent,
    },
}

impl DisplayCtx<CSymbols> for CAssignValue {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        use CAssignValue::*;
        match *self {
            Alloc {typ} => cwrite!(f, ctx, "{}(sizeof({}))", ALLOCATE, typ),
            FieldAccess {value, field} => cwrite!(f, ctx, "{}->{}", value, field),
            Var {name} => cwrite!(f, ctx, "{}", name),
        }
    }
}

/// Symbol table for C identifiers
pub type CSymbols = SymbolTable<String, CIdent>;

/// The ID of an identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CIdent(usize);

impl DisplayCtx<CSymbols> for CIdent {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        write!(f, "{}", ctx.symbol(*self))
    }
}

impl SymId for CIdent {
    type Gen = CIdentGen;
}

#[derive(Debug, Default)]
pub struct CIdentGen {
    next: usize,
}

impl GenId<CIdent> for CIdentGen {
    fn next_id(&mut self) -> CIdent {
        let id = self.next;
        self.next += 1;
        CIdent(id)
    }
}

/// Represents a foward declaration of the given type
///
/// Allows us to specialize the implementation of `DisplayCtx` for forward declarations
struct ForwardDecl<'a, T> {
    value: &'a T,
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
