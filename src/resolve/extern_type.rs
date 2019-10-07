/// A type described by extern symbols
///
/// Note that no constructors are allowed to have side effects that depend on the order in
/// which the constructor is run. Constructor functions may run in any order relative to other
/// subexpressions.
#[derive(Debug, Default)]
pub struct ExternType {
    /// The name of the extern type to use during code generation
    pub extern_name: String,

    /// (optional) A function that returns a value of the extern type given no parameters
    ///
    /// Only required if a unit literal () may type check to this type
    pub unit_literal_constructor: Option<String>,

    /// (optional) A function that takes a bool and returns a value of the extern type
    ///
    /// Only required if a boolean literal (true, false) may type check to this type
    pub bool_literal_constructor: Option<String>,

    /// (optional) A function that takes an int64_t and returns a value of the extern type
    ///
    /// Only required if an integer literal may type check to this type
    pub int_literal_constructor: Option<String>,

    /// (optional) A function that takes a double and returns a value of the extern type
    ///
    /// Only required if a real number literal may type check to this type
    pub real_literal_constructor: Option<String>,

    /// (optional) A function that takes a double and returns a value of the extern type
    ///
    /// Only required if a complex number literal may type check to this type
    pub complex_literal_constructor: Option<String>,
}
