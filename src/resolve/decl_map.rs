use std::fmt;
use std::collections::HashSet;
use std::borrow::Borrow;
use std::error::Error;
use std::hash::{Hash, Hasher};

use crate::ast::{Decl, Ident};

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

/// Allows decls to be looked up by name while still storing the name in the Decl type
#[derive(Debug)]
struct DeclEntry<'a> {
    decl: Decl<'a>,
}

impl<'a> Hash for DeclEntry<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.decl.name().hash(state)
    }
}

impl<'a> Borrow<&'a str> for DeclEntry<'a> {
    fn borrow(&self) -> &&'a str {
        self.decl.name()
    }
}

impl<'a> PartialEq for DeclEntry<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.decl.name() == other.decl.name()
    }
}

impl<'a> PartialEq<str> for DeclEntry<'a> {
    fn eq(&self, other: &str) -> bool {
        *self.decl.name() == other
    }
}

impl<'a> Eq for DeclEntry<'a> {}

/// The declarations in a module, indexed by name
#[derive(Debug)]
pub struct DeclMap<'a> {
    decls: HashSet<DeclEntry<'a>>,
}

impl<'a> DeclMap<'a> {
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