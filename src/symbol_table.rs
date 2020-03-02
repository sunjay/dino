use std::sync::Arc;
use std::fmt;
use std::hash::Hash;
use std::collections::HashMap;

/// A trait for generating unique IDs
pub trait GenId<Id> {
    fn next_id(&mut self) -> Id;
}

/// A trait for generating unique IDs (with synchronization)
pub trait GenIdSync<Id> {
    fn next_id(&self) -> Id;
}

impl<Id, T: GenIdSync<Id>> GenId<Id> for Arc<T> {
    fn next_id(&mut self) -> Id {
        GenIdSync::next_id(&**self)
    }
}

/// Represents a type that can be used as an ID
pub trait SymId: Hash + Eq + Copy {
    type Gen: GenId<Self> + Default;
}

/// Represents a general symbol table that maps symbol names to unique IDs and back
pub struct SymbolTable<Sym, Id, Data = ()>
    where Sym: Hash + Eq,
          Id: SymId,
{
    symbols: HashMap<Id, (Arc<Sym>, Data)>,
    ids: HashMap<Arc<Sym>, Id>,
    id_gen: <Id as SymId>::Gen,
}

impl<Sym, Id, Data> fmt::Debug for SymbolTable<Sym, Id, Data>
    where Sym: Hash + Eq + fmt::Debug,
          Id: SymId + fmt::Debug,
          <Id as SymId>::Gen: fmt::Debug,
          Data: fmt::Debug,
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

impl<Sym, Id, Data> Default for SymbolTable<Sym, Id, Data>
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

impl<Sym, Id> SymbolTable<Sym, Id, ()>
    where Sym: Hash + Eq,
          Id: SymId,
{
    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// If the symbol was already present in the table, it will be returned in an `Err`
    pub fn insert(&mut self, sym: Sym) -> Result<Id, Sym> {
        self.insert_with(sym, ()).map_err(|(sym, _)| sym)
    }

    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// If the symbol was previously present in the table, it will be overwritten with a new ID.
    pub fn insert_overwrite(&mut self, sym: Sym) -> Id {
        self.insert_overwrite_with(sym, ())
    }
}

impl<Sym, Id, Data> SymbolTable<Sym, Id, Data>
    where Sym: Hash + Eq,
          Id: SymId,
{
    /// Inserts a new symbol into the symbol table with the given data and returns its ID
    ///
    /// If the symbol was already present in the table, it will be returned in an `Err`
    pub fn insert_with(&mut self, sym: Sym, data: Data) -> Result<Id, (Sym, Data)> {
        if self.ids.contains_key(&sym) {
            return Err((sym, data));
        }

        Ok(self.insert_overwrite_with(sym, data))
    }

    /// Inserts a new symbol into the symbol table with the given data and returns its ID
    ///
    /// If the symbol was previously present in the table, it will be overwritten with a new ID.
    pub fn insert_overwrite_with(&mut self, sym: Sym, data: Data) -> Id {
        let sym = Arc::new(sym);
        let id = self.id_gen.next_id();
        self.symbols.insert(id, (sym.clone(), data));
        self.ids.insert(sym, id);

        id
    }

    /// Returns the ID associated with the given symbol
    pub fn id(&self, sym: &Sym) -> Option<Id> {
        self.ids.get(sym).copied()
    }

    /// Returns the symbol associated with the given ID
    pub fn data(&self, id: Id) -> &Data {
        self.symbols.get(&id).map(|(_, data)| data)
            .expect("bug: should be impossible to get an ID that hasn't been inserted")
    }

    /// Returns the symbol associated with the given ID
    pub fn data_mut(&mut self, id: Id) -> &mut Data {
        self.symbols.get_mut(&id).map(|(_, data)| data)
            .expect("bug: should be impossible to get an ID that hasn't been inserted")
    }

    /// Returns the symbol associated with the given ID
    pub fn symbol(&self, id: Id) -> &Sym {
        self.symbols.get(&id).map(|(sym, _)| &**sym)
            .expect("bug: should be impossible to get an ID that hasn't been inserted")
    }
}
