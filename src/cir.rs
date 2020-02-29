//! C IR - Types for representing the minimal subset of C used for code generation.
//!
//! All types here should directly map to concepts expressible in C. This is the last step in code
//! generation and no further processing should be required in order to convert these types to C.

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

#[derive(Debug, Clone)]
pub struct CStruct {
    pub name: CIdent,
    /// If any of the fields have the same name as the struct, the
    /// `struct` C keyword will be added before them
    pub fields: Vec<CStructField>,
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

#[derive(Debug, Clone)]
pub struct CFuncParam {
    pub name: CIdent,
    pub typ: CFuncParamType,
}

#[derive(Debug, Clone)]
pub enum CFuncParamType {
    /// A pointer to the given type
    InPtr {typ: CIdent},
    /// An out pointer to the given type (i.e. pointer to a pointer)
    OutPtr {typ: CIdent},
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

#[derive(Debug, Clone)]
pub struct CVarDecl {
    pub name: CIdent,
    pub typ: CVarType,
}

#[derive(Debug, Clone)]
pub enum CVarType {
    /// A pointer to the given type
    Ptr {typ: CIdent},
    /// A plain `bool` type
    Bool,
}

#[derive(Debug, Clone)]
pub struct CFuncCall {
    pub func_name: CIdent,
    pub args: Vec<CFuncArg>,
}

#[derive(Debug, Clone)]
pub enum CFuncArg {
    /// A plain variable passed as the argument
    Var(CIdent),
    /// An out parameter (pointer to a variable)
    OutVar(CIdent),
}

#[derive(Debug, Clone)]
pub struct CAssign {
    /// The left-hand side of the assignment
    pub target: CAssignTarget,
    /// The right-hand side of the assignment
    pub value: CAssignValue,
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

/// Symbol table for C identifiers
pub type CSymbols = SymbolTable<String, CIdent>;

/// The ID of an identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CIdent(usize);

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
