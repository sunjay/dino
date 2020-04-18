use std::sync::Arc;

use parking_lot::Mutex;

use crate::package::PkgId;

use super::DefKind;

/// An ID for anything that can be defined in the program (e.g. types, functions, variables, etc.)
///
/// Note that method names in method calls do NOT get a DefId, but once the method name is
/// resolved, it gets resolved to the DefId of the method declaration. The same is true for
/// field names in field access expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId {
    pkg: PkgId,
    def_index: usize,
}

/// A version of the `DefStore` that can be shared
pub type DefStoreSync = Arc<Mutex<DefStore>>;

#[derive(Debug)]
pub struct DefStore {
    pkg: PkgId,
    /// `def_index` indexes into this field
    defs: Vec<(Arc<str>, DefKind)>,
}

impl DefStore {
    pub fn new(pkg: PkgId) -> Self {
        Self {
            pkg,
            defs: Vec::new(),
        }
    }

    /// Adds a new item to the store, returning a new ID for that item
    pub fn push(&mut self, sym: Arc<str>, data: DefKind) -> DefId {
        self.defs.push((sym, data));

        DefId {
            pkg: self.pkg,
            def_index: self.defs.len() - 1,
        }
    }

    /// Retrieves the def associated with the given ID
    pub fn get(&self, id: DefId) -> &DefKind {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (_, data) = &self.defs[id.def_index];
        data
    }

    /// Retrieves a mutable version of the def associated with the given ID
    pub fn get_mut(&mut self, id: DefId) -> &mut DefKind {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (_, data) = &mut self.defs[id.def_index];
        data
    }

    /// Retrieves the symbol associated with the given ID
    pub fn symbol(&self, id: DefId) -> &Arc<str> {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (sym, _) = &self.defs[id.def_index];
        sym
    }
}
