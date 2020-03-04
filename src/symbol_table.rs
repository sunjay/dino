mod id_store;

pub use id_store::*;

use std::fmt;
use std::hash::Hash;
use std::borrow::Borrow;
use std::collections::HashMap;

/// A symbol table that uses an ID generator
pub type IdGenSymbolTable<Sym, Id, Data = ()> = SymbolTable<IdGenStore<Sym, Id, Data>>;

/// Represents a general symbol table that maps symbol names to unique IDs and back
pub struct SymbolTable<S: IdStore> {
    ids: HashMap<S::Sym, S::Id>,
    store: S,
}

impl<S> fmt::Debug for SymbolTable<S>
    where S: IdStore + fmt::Debug,
          S::Sym: fmt::Debug,
          S::Id: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {ids, store} = self;
        f.debug_struct("SymbolTable")
            .field("ids", ids)
            .field("store", store)
            .finish()
    }
}

impl<S: IdStore + Default> Default for SymbolTable<S> {
    fn default() -> Self {
        SymbolTable::new(S::default())
    }
}

impl<S: IdStore<Data=()>> SymbolTable<S> {
    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// If the symbol was already present in the table, it will be returned in an `Err`
    pub fn insert(&mut self, sym: S::Sym) -> Result<S::Id, S::Sym> {
        self.insert_with(sym, ()).map_err(|(sym, _)| sym)
    }

    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// If the symbol was previously present in the table, it will be overwritten with a new ID.
    pub fn insert_overwrite(&mut self, sym: S::Sym) -> S::Id {
        self.insert_overwrite_with(sym, ())
    }

    /// Gets the ID for the given symbol, or inserts the symbol and returns its new ID
    ///
    /// This is different from `insert_overwrite` because it does *not* insert a new ID when the
    /// symbol is already present in the table.
    pub fn get_or_insert(&mut self, sym: S::Sym) -> S::Id {
        match self.id(&sym) {
            Some(id) => id,
            // The symbol isn't present, so this doesn't overwrite anything
            None => self.insert_overwrite(sym),
        }
    }
}

impl<S: IdStore> SymbolTable<S> {
    /// Creates a new symbol table with the given store
    pub fn new(store: S) -> Self {
        Self {
            ids: HashMap::default(),
            store,
        }
    }

    /// Inserts a new symbol into the symbol table with the given data and returns its ID
    ///
    /// If the symbol was already present in the table, it will be returned in an `Err`
    pub fn insert_with(&mut self, sym: S::Sym, data: S::Data) -> Result<S::Id, (S::Sym, S::Data)> {
        if self.ids.contains_key(&sym) {
            return Err((sym, data));
        }

        Ok(self.insert_overwrite_with(sym, data))
    }

    /// Inserts a new symbol into the symbol table with the given data and returns its ID
    ///
    /// If the symbol was previously present in the table, it will be overwritten with a new ID.
    pub fn insert_overwrite_with(&mut self, sym: S::Sym, data: S::Data) -> S::Id {
        let id = self.store.push(sym.clone(), data);
        self.ids.insert(sym, id);

        id
    }

    /// Returns the ID associated with the given symbol
    pub fn id<Q: ?Sized>(&self, sym: &Q) -> Option<S::Id>
        where S::Sym: Borrow<Q>,
              Q: Hash + Eq,
    {
        self.ids.get(sym).copied()
    }

    /// Returns the symbol associated with the given ID
    pub fn data(&self, id: S::Id) -> &S::Data {
        self.store.data(id)
    }

    /// Returns the symbol associated with the given ID
    pub fn data_mut(&mut self, id: S::Id) -> &mut S::Data {
        self.store.data_mut(id)
    }

    /// Returns the symbol associated with the given ID
    pub fn symbol(&self, id: S::Id) -> &S::Sym {
        self.store.symbol(id)
    }
}
