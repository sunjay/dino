use std::sync::Arc;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::collections::HashMap;

/// A trait for generating unique IDs
pub trait GenId<Id> {
    fn next_id(&mut self) -> Id;
}

/// Represents a type that can be used as an ID
pub trait SymId: Hash + Eq + Copy {
    type Gen: GenId<Self> + Default;
}

/// Represents a general symbol table that maps symbol names to unique IDs and back
pub struct SymbolTable<Sym, Id>
    where Sym: Hash + Eq,
          Id: SymId,
{
    symbols: HashMap<Id, Arc<Sym>>,
    ids: HashMap<Arc<Sym>, Id>,
    id_gen: <Id as SymId>::Gen,
}

impl<Sym, Id> fmt::Debug for SymbolTable<Sym, Id>
    where Sym: Hash + Eq + fmt::Debug,
          Id: SymId + fmt::Debug,
          <Id as SymId>::Gen: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {symbols, ids, id_gen} = self;
        f.debug_struct("SymbolTable")
            .field("symbols", symbols)
            .field("ids", ids)
            .field("id_gen", id_gen)
            .finish()
    }
}

impl<Sym, Id> Default for SymbolTable<Sym, Id>
    where Sym: Hash + Eq,
          Id: SymId,
{
    fn default() -> Self {
        Self {
            symbols: HashMap::default(),
            ids: HashMap::default(),
            id_gen: Default::default(),
        }
    }
}

impl<Sym, Id> SymbolTable<Sym, Id>
    where Sym: Hash + Eq,
          Id: SymId,
{
    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// # Panics
    ///
    /// Panics if the symbol was previously present in the table
    pub fn insert(&mut self, sym: Sym) -> Id
        where Sym: Display,
    {
        if self.ids.contains_key(&sym) {
            unreachable!("bug: attempt to insert duplicate symbol: `{}`", sym);
        }

        let sym = Arc::new(sym);
        let id = self.id_gen.next_id();
        self.symbols.insert(id, sym.clone());
        self.ids.insert(sym, id);

        id
    }

    /// Returns the ID associated with the given symbol
    pub fn id(&self, sym: &Sym) -> Option<Id> {
        self.ids.get(sym).copied()
    }

    /// Returns the symbol associated with the given ID
    pub fn symbol(&self, id: Id) -> &Sym {
        self.symbols.get(&id).map(|sym| &**sym)
            .expect("bug: should be impossible to get an ID that hasn't been inserted")
    }
}
