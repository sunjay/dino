use std::fmt;
use std::collections::HashMap;
use std::borrow::Borrow;
use std::error::Error;
use std::hash::{Hash, Hasher};

use crate::ast::{Decl, Ident, Function, Method};

#[derive(Debug)]
pub struct DuplicateDecl {
    duplicate: String,
}

impl fmt::Display for DuplicateDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "the name '{}' is defined multiple times", self.duplicate)
    }
}

impl Error for DuplicateDecl {}

/// Maps method Self types to a function declaration
type MethodMap<'a> = HashMap<Ident<'a>, Function<'a>>;

#[derive(Debug)]
pub struct DeclEntry<'a> {
    Function(Function<'a>),
    ImplMethod(Method<'a>),
}

/// The declarations in a module, indexed by name and/or type
#[derive(Debug)]
pub struct ModuleDecls<'a> {
    /// Maps function fully-qualified names to a function declaration
    functions: HashMap<Ident<'a>, Function<'a>>,
    /// Maps fully-qualified method names to a table of their Self types and declarations
    impl_methods: HashMap<Ident<'a>, MethodMap<'a>>,
}

impl<'a> ModuleDecls<'a> {
    /// Collects the declarations and groups them by name
    pub fn new(input_decls: Vec<Decl<'a>>) -> Result<Self, DuplicateDecl> {
        let mut decls = HashSet::new();

        for decl in input_decls {
            let entry = DeclEntry {decl};
            if decls.contains(&entry) {
                return Err(DuplicateDecl {
                    duplicate: entry.decl.name().to_string(),
                });
            }

            debug_assert!(decls.insert(entry), "bug: duplicate declaration went undetected");
        }

        Ok(Self {decls})
    }

    /// Returns a reference to the declaration corresponding to the given name, if any
    pub fn get(&self, name: &Ident<'a>) -> Option<&Decl<'a>> {
        self.decls.get(name).map(|entry| &entry.decl)
    }

    /// Returns an iterator that goes through each declaration in the map
    pub fn iter(&self) -> impl Iterator<Item=&Decl<'a>> {
        self.decls.iter().map(|entry| &entry.decl)
    }
}
