use std::collections::HashMap;

use crate::ast::{Ident, Function, Ty};

/// Information about a type
#[derive(Debug)]
pub struct TypeInfo<'a> {
    /// The name of the type
    pub name: Ident<'a>,

    /// If true, the type name will be used verbatim during code generation
    pub is_extern: bool,

    /// Constructors to allow the type to be created from different literals
    pub constructors: LiteralConstructors<'a>,

    /// The fields of this type (if any)
    ///
    /// Extern types are not required to declare their fields.
    pub fields: HashMap<Ident<'a>, Ty<'a>>,

    /// The methods provided by this type.
    ///
    /// The keys of the map are the method names (e.g. `add`) whereas the `name` field of
    /// the `Function` can be anything. If the Function is extern, this `name` field will
    /// be used in the generated code.
    pub methods: HashMap<Ident<'a>, Function<'a>>,
}

impl<'a> TypeInfo<'a> {
    pub fn new(name: Ident<'a>) -> Self {
        Self {
            name,
            is_extern: false,
            constructors: LiteralConstructors::default(),
            fields: HashMap::default(),
            methods: HashMap::default(),
        }
    }
}

/// Constructors to allow a type to be created from different literals
///
/// Note that no constructors are allowed to have side effects that depend on the order in
/// which the constructor is run. Constructor functions may run in any order relative to other
/// subexpressions.
#[derive(Debug, Default)]
pub struct LiteralConstructors<'a> {
    /// (optional) A function that returns a value of the extern type given no parameters
    ///
    /// Only required if a unit literal () may type check to this type
    pub unit_literal_constructor: Option<Ident<'a>>,

    /// (optional) A function that takes a bool and returns a value of the extern type
    ///
    /// Only required if a boolean literal (true, false) may type check to this type
    pub bool_literal_constructor: Option<Ident<'a>>,

    /// (optional) A function that takes an int64_t and returns a value of the extern type
    ///
    /// Only required if an integer literal may type check to this type
    pub int_literal_constructor: Option<Ident<'a>>,

    /// (optional) A function that takes a double and returns a value of the extern type
    ///
    /// Only required if a real number literal may type check to this type
    pub real_literal_constructor: Option<Ident<'a>>,

    /// (optional) A function that takes a double and returns a value of the extern type
    ///
    /// Only required if a complex number literal may type check to this type
    pub complex_literal_constructor: Option<Ident<'a>>,

    /// (optional) A function that takes a *const u8 pointer and a uintptr_t length and returns a
    /// value of the extern type
    ///
    /// Only required if a byte string literal may type check to this type
    pub bstr_literal_constructor: Option<Ident<'a>>,
}
