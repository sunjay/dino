use std::fmt::Display;
use std::hash::Hash;
use std::collections::HashMap;

/// A trait for generating unique IDs
pub trait GenId<Id> {
    fn next_id(&mut self) -> Id;
}

/// Represents a general symbol table that maps symbol names to unique IDs and back
#[derive(Debug)]
pub struct SymbolTable<Sym, Id, Gen>
    where Sym: Hash + Eq,
          Id: Hash + Eq + Copy,
{
    symbols: HashMap<Id, Sym>,
    ids: HashMap<Sym, Id>,
    id_gen: Gen,
}

impl<Sym, Id, Gen> Default for SymbolTable<Sym, Id, Gen>
    where Sym: Hash + Eq,
          Id: Hash + Eq + Copy,
          Gen: Default,
{
    fn default() -> Self {
        Self {
            symbols: HashMap::default(),
            ids: HashMap::default(),
            id_gen: Gen::default(),
        }
    }
}

impl<Sym, Id, Gen> SymbolTable<Sym, Id, Gen>
    where Sym: Hash + Eq,
          Id: Hash + Eq + Copy,
{
    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// # Panics
    ///
    /// Panics if the symbol was previously present in the table
    pub fn insert(&mut self, sym: Sym) -> Id
        where Sym: Display + Clone,
              Gen: GenId<Id>,
    {
        if self.ids.contains_key(&sym) {
            unreachable!("bug: attempt to insert duplicate symbol: `{}`", sym);
        }

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
    pub fn symbol(&self, id: Id) -> Option<&Sym> {
        self.symbols.get(&id)
    }
}
