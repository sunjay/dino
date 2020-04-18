use std::sync::Arc;
use std::collections::HashMap;

use super::{FuncSig, Ty};

#[derive(Debug)]
pub struct StructField {
    pub name: Arc<str>,
    pub fields: Ty,
}

#[derive(Debug, Default)]
pub struct Struct {
    pub fields: Vec<StructField>,
}

impl Struct {
    pub fn without_fields() -> Self {
        Struct::default()
    }
}

#[derive(Debug)]
pub enum TypeDeclKind {
    Struct(Struct),
}

/// Constructors to allow a type to be created from different literals
///
/// Note that no constructors are allowed to have side effects that depend on the order in
/// which the constructor is run. Constructor functions may run in any order relative to other
/// subexpressions.
#[derive(Debug, Default)]
pub struct LiteralConstructors {
    /// (optional) A function that returns a value of the extern type given no parameters
    ///
    /// Only required if a unit literal () may type check to this type
    pub unit_literal_constructor: Option<Arc<str>>,

    /// (optional) A function that takes a bool and returns a value of the extern type
    ///
    /// Only required if a boolean literal (true, false) may type check to this type
    pub bool_literal_constructor: Option<Arc<str>>,
    /// (optional) A function that returns a bool when given a value of the extern type
    ///
    /// Only required if a boolean literal (true, false) may type check to this type
    pub coerce_bool: Option<Arc<str>>,

    /// (optional) A function that takes an int64_t and returns a value of the extern type
    ///
    /// Only required if an integer literal may type check to this type
    pub int_literal_constructor: Option<Arc<str>>,

    /// (optional) A function that takes a double and returns a value of the extern type
    ///
    /// Only required if a real number literal may type check to this type
    pub real_literal_constructor: Option<Arc<str>>,

    /// (optional) A function that takes a double and returns a value of the extern type
    ///
    /// Only required if a complex number literal may type check to this type
    pub complex_literal_constructor: Option<Arc<str>>,

    /// (optional) A function that takes a *const u8 pointer and a uintptr_t length and returns a
    /// value of the extern type
    ///
    /// Only required if a byte string literal may type check to this type
    pub bstr_literal_constructor: Option<Arc<str>>,
}

/// The type information stored for all different kinds of types
#[derive(Debug)]
pub struct TypeDecl {
    pub kind: TypeDeclKind,
    pub methods: HashMap<Arc<str>, FuncSig>,
    pub literal_constructors: LiteralConstructors,
}
