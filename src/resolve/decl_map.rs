use std::fmt;
use std::collections::{HashSet, HashMap};
use std::borrow::Borrow;
use std::error::Error;
use std::hash::{Hash, Hasher};

use crate::ast::{Function, Ident};

// Allows functions to be looked up by name without requiring us to use a HashMap and duplicating
// the name in the key.
#[derive(Debug)]
struct FunctionEntry<'a> {
    func: Function<'a>,
}

impl<'a> Hash for FunctionEntry<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.func.name.hash(state)
    }
}

impl<'a> Borrow<Ident<'a>> for FunctionEntry<'a> {
    fn borrow(&self) -> &Ident<'a> {
        &self.func.name
    }
}

// Don't want all of Function to need to implement PartialEq
impl<'a> PartialEq for FunctionEntry<'a> {
    fn eq(&self, other: &FunctionEntry<'a>) -> bool {
        self.func.name == other.func.name
    }
}

impl<'a> PartialEq<str> for FunctionEntry<'a> {
    fn eq(&self, other: &str) -> bool {
        self.func.name == other
    }
}

impl<'a> Eq for FunctionEntry<'a> {}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyId(usize);

/// The declarations in a module, indexed by name
#[derive(Debug, Default)]
pub struct DeclMap<'a> {
    functions: HashSet<FunctionEntry<'a>>,
    types: Vec<Ident<'a>>,
    type_ids: HashMap<Ident<'a>, TyId>,
}

impl<'a> DeclMap<'a> {
    /// Inserts a new function declaration
    pub fn insert_func(&mut self, func: Function<'a>) -> Result<(), DuplicateDecl> {
        let name = func.name;
        let func_entry = FunctionEntry {func};
        
        if !self.functions.insert(func_entry) {
            return Err(DuplicateDecl {
                duplicate: name.to_string(),
            });
        }

        Ok(())
    }

    /// Inserts a new type and returns its type ID
    pub fn insert_ty(&mut self, ty: Ident<'a>) -> Result<TyId, DuplicateDecl> {
        let id = TyId(self.types.len());
        if self.type_ids.insert(ty, id).is_some() {
            return Err(DuplicateDecl {
                duplicate: ty.to_string(),
            });
        }

        self.types.push(ty);
        Ok(id)
    }

    /// Returns the ID of the given type name
    pub fn get_ty_id(&self, ty: &Ident<'a>) -> Option<TyId> {
        self.type_ids.get(ty).copied()
    }

    /// Returns the name of the given type ID
    pub fn get_ty_name(&self, id: &TyId) -> Option<&Ident<'a>> {
        let &TyId(id) = id;
        self.types.get(id)
    }

    /// Returns a reference to the declaration corresponding to the given name, if any
    pub fn get_func(&self, name: &Ident<'a>) -> Option<&Function<'a>> {
        self.functions.get(name).map(|entry| &entry.func)
    }

    /// Returns an iterator that goes through each declaration in the map
    pub fn iter_func(&self) -> impl Iterator<Item=&Function<'a>> {
        self.functions.iter().map(|entry| &entry.func)
    }
}
