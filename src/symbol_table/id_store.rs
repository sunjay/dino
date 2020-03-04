use std::fmt;
use std::hash::Hash;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;

/// Represents a type that can be a backend/store for the data in the symbol table
pub trait IdStore {
    type Sym: Hash + Eq + Clone;
    type Id: Hash + Eq + Copy;
    type Data;

    /// Pushes a new item into the store, returning a new ID for that item
    fn push(&mut self, sym: Self::Sym, data: Self::Data) -> Self::Id;
    /// Retrieves the data for an item already in the store
    fn data(&self, id: Self::Id) -> &Self::Data;
    /// Retrieves a mutable version of the data for an item already in the store
    fn data_mut(&mut self, id: Self::Id) -> &mut Self::Data;
    /// Retrieves the symbol corresponding to the give ID
    fn symbol(&self, id: Self::Id) -> &Self::Sym;
}

impl<T: IdStore> IdStore for Arc<Mutex<T>> {
    type Sym = <T as IdStore>::Sym;
    type Id = <T as IdStore>::Id;
    type Data = <T as IdStore>::Data;

    fn push(&mut self, sym: Self::Sym, data: Self::Data) -> Self::Id {
        self.lock()
            .expect("bug: lock poisoned")
            .push(sym, data)
    }

    fn data(&self, id: Self::Id) -> &Self::Data {
        self.lock()
            .expect("bug: lock poisoned")
            .data(id)
    }

    fn data_mut(&mut self, id: Self::Id) -> &mut Self::Data {
        self.lock()
            .expect("bug: lock poisoned")
            .data_mut(id)
    }

    fn symbol(&self, id: Self::Id) -> &Self::Sym {
        self.lock()
            .expect("bug: lock poisoned")
            .symbol(id)
    }
}

/// A trait for generating unique IDs
pub trait GenId<Id> {
    fn next_id(&mut self) -> Id;
}

/// Represents a type that can be used as an ID
pub trait SymId: Hash + Eq + Copy {
    type Gen: GenId<Self> + Default;
}

/// A simple ID store that relies on an ID generator
pub struct IdGenStore<Sym, Id, Data = ()>
    where Sym: Hash + Eq + Clone,
          Id: SymId,
{
    symbols: HashMap<Id, (Sym, Data)>,
    id_gen: <Id as SymId>::Gen,
}

impl<Sym, Id, Data> fmt::Debug for IdGenStore<Sym, Id, Data>
    where Sym: Hash + Eq + Clone + fmt::Debug,
          Id: SymId + fmt::Debug,
          Data: fmt::Debug,
          <Id as SymId>::Gen: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {symbols, id_gen} = self;
        f.debug_struct("IdGenStore")
            .field("symbols", symbols)
            .field("id_gen", id_gen)
            .finish()
    }
}

impl<Sym, Id, Data> Default for IdGenStore<Sym, Id, Data>
    where Sym: Hash + Eq + Clone,
          Id: SymId,
          <Id as SymId>::Gen: Default,
{
    fn default() -> Self {
        Self {
            symbols: Default::default(),
            id_gen: Default::default(),
        }
    }
}

impl<Sym, Id, Data> IdStore for IdGenStore<Sym, Id, Data>
    where Sym: Hash + Eq + Clone,
          Id: SymId,
{
    type Sym = Sym;
    type Id = Id;
    type Data = Data;

    fn push(&mut self, sym: Self::Sym, data: Self::Data) -> Self::Id {
        let id = self.id_gen.next_id();
        self.symbols.insert(id, (sym.clone(), data));

        id
    }

    fn data(&self, id: Self::Id) -> &Self::Data {
        self.symbols.get(&id).map(|(_, data)| data)
            .expect("bug: should be impossible to get an ID that hasn't been inserted")
    }

    fn data_mut(&mut self, id: Self::Id) -> &mut Self::Data {
        self.symbols.get_mut(&id).map(|(_, data)| data)
            .expect("bug: should be impossible to get an ID that hasn't been inserted")
    }

    fn symbol(&self, id: Self::Id) -> &Self::Sym {
        self.symbols.get(&id).map(|(sym, _)| sym)
            .expect("bug: should be impossible to get an ID that hasn't been inserted")
    }
}
