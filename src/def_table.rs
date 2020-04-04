mod def_store;

pub use def_store::*;

use std::sync::Arc;
use std::hash::Hash;
use std::borrow::Borrow;
use std::collections::HashMap;

use parking_lot::MutexGuard;

/// A symbol table containing a shared `DefStore`
///
/// This allows all levels of scope to have their own table for looking up names, but a single
/// source of data for looking up `DefId`s
#[derive(Debug)]
pub struct DefTable<T> {
    ids: HashMap<Arc<str>, DefId>,
    store: DefStoreSync<T>,
}

impl<T> DefTable<T> {
    /// Creates a new symbol table with the given store
    pub fn new(store: DefStoreSync<T>) -> Self {
        Self {
            ids: HashMap::default(),
            store,
        }
    }

    /// Returns true if no symbols have been inserted into this table
    pub fn is_empty(&self) -> bool {
        self.ids.is_empty()
    }

    /// Returns the number of symbols have been inserted into this table
    pub fn len(&self) -> usize {
        self.ids.len()
    }

    /// Inserts a new symbol into the symbol table with the given data and returns its ID
    ///
    /// If the symbol was already present in the table, it will be returned in an `Err`
    pub fn insert(&mut self, sym: Arc<str>, data: T) -> Result<DefId, (Arc<str>, T)> {
        if self.ids.contains_key(&sym) {
            return Err((sym, data));
        }

        Ok(self.insert_overwrite(sym, data))
    }

    /// Inserts a new symbol into the symbol table with the given data and returns its ID
    ///
    /// If the symbol was previously present in the table, it will be overwritten with a new ID.
    pub fn insert_overwrite(&mut self, sym: Arc<str>, data: T) -> DefId {
        let id = self.store().push(sym.clone(), data);
        self.ids.insert(sym, id);

        id
    }

    /// Returns the ID associated with the given symbol
    pub fn id<Q: ?Sized>(&self, sym: &Q) -> Option<DefId>
        where Arc<str>: Borrow<Q>,
              Q: Hash + Eq,
    {
        self.ids.get(sym).copied()
    }

    fn store(&self) -> MutexGuard<DefStore<T>> {
        self.store.lock()
    }
}
