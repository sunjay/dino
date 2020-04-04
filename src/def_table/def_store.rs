use std::sync::Arc;

use parking_lot::Mutex;

use crate::package::PkgId;

/// An ID for any module, type, function, variable, etc.
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
pub type DefStoreSync<T> = Arc<Mutex<DefStore<T>>>;

/// Stores a mapping from `DefId` to each `T` one particular package
#[derive(Debug)]
pub struct DefStore<T> {
    pkg: PkgId,
    /// `def_index` indexes into this field
    defs: Vec<(Arc<str>, T)>,
}

impl<T> DefStore<T> {
    pub fn new(pkg: PkgId) -> Self {
        Self {
            pkg,
            defs: Vec::new(),
        }
    }

    /// Pushes a new item into the store, returning a new ID for that item
    pub fn push(&mut self, sym: Arc<str>, data: T) -> DefId {
        self.defs.push((sym, data));
        DefId {
            pkg: self.pkg,
            def_index: self.defs.len() - 1,
        }
    }

    /// Retrieves the data for an item already in the store
    pub fn data(&self, id: DefId) -> &T {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (_, data) = &self.defs[id.def_index];
        data
    }

    /// Retrieves a mutable version of the data for an item already in the store
    pub fn data_mut(&mut self, id: DefId) -> &mut T {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (_, data) = &mut self.defs[id.def_index];
        data
    }

    /// Retrieves the symbol corresponding to the give ID
    pub fn symbol(&self, id: DefId) -> &Arc<str> {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (sym, _) = &self.defs[id.def_index];
        sym
    }
}
