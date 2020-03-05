use std::sync::MutexGuard;
use std::hash::Hash;
use std::borrow::Borrow;
use std::collections::HashMap;

use super::{DefStoreSync, DefStore, DefId, DefData};

/// A symbol table containing a shared `DefStore`
///
/// This allows all levels of scope to have their own table for looking up names, but a single
/// source of data for looking up `DefId`s
#[derive(Debug)]
pub struct DefTable {
    ids: HashMap<String, DefId>,
    store: DefStoreSync,
}

impl DefTable {
    /// Creates a new symbol table with the given store
    pub fn new(store: DefStoreSync) -> Self {
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
    pub fn insert_with(&mut self, sym: String, data: DefData) -> Result<DefId, (String, DefData)> {
        if self.ids.contains_key(&sym) {
            return Err((sym, data));
        }

        Ok(self.insert_overwrite_with(sym, data))
    }

    /// Inserts a new symbol into the symbol table with the given data and returns its ID
    ///
    /// If the symbol was previously present in the table, it will be overwritten with a new ID.
    pub fn insert_overwrite_with(&mut self, sym: String, data: DefData) -> DefId {
        let id = self.store().push(sym.clone(), data);
        self.ids.insert(sym, id);

        id
    }

    /// Returns the ID associated with the given symbol
    pub fn id<Q: ?Sized>(&self, sym: &Q) -> Option<DefId>
        where String: Borrow<Q>,
              Q: Hash + Eq,
    {
        self.ids.get(sym).copied()
    }

    fn store(&self) -> MutexGuard<DefStore> {
        self.store.lock().expect("bug: lock poisoned")
    }
}
