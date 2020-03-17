//! The syntax tree of the program
//!
//! This is the closest representation to the actual syntax.

use parser::Token;

/// Represents a single module within the current package
#[derive(Debug, Clone, PartialEq)]
pub struct Module<'a> {
    pub decls: Vec<Decl<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl<'a> {
    Import(ImportPath<'a>),
    Struct(Struct<'a>),
    Impl(Impl<'a>),
    Function(Function<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportPath<'a> {
    pub use_token: Token<'a>,
    pub path: IdentPath<'a>,
    /// If `None`, only the path itself is imported
    pub selection: Option<ImportSelection<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PathComponent<'a> {
    Ident(Ident<'a>),
    /// The `package` keyword
    Package,
    /// The `Self` keyword
    SelfType,
    /// The `self` keyword
    SelfValue,
    /// The `super` keyword
    Super,
}
