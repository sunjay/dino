use std::fmt;
use std::hash::Hash;
use std::borrow::Borrow;
use std::collections::HashMap;

use crate::fmt_ctx::DisplayCtx;

/// The ID of an identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident(usize);

impl DisplayCtx<CSymbols> for Ident {
    fn fmt_ctx(&self, f: &mut fmt::Formatter<'_>, ctx: &CSymbols) -> fmt::Result {
        write!(f, "{}", ctx.symbol(*self))
    }
}

/// Symbol table for C identifiers
#[derive(Debug, Default)]
pub struct CSymbols {
    ids: HashMap<String, Ident>,
    symbols: Vec<String>,
}

impl CSymbols {
    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// If the symbol was already present in the table, it will be returned in an `Err`
    pub fn insert(&mut self, sym: String) -> Result<Ident, String> {
        if self.ids.contains_key(&sym) {
            return Err(sym);
        }

        Ok(self.insert_overwrite(sym))
    }

    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// If the symbol was previously present in the table, it will be overwritten with a new ID.
    pub fn insert_overwrite(&mut self, sym: String) -> Ident {
        let id = Ident(self.symbols.len());
        self.symbols.push(sym.clone());
        self.ids.insert(sym, id);

        id
    }

    /// Returns the ID associated with the given symbol
    pub fn id<Q: ?Sized>(&self, sym: &Q) -> Option<Ident>
        where String: Borrow<Q>,
              Q: Hash + Eq,
    {
        self.ids.get(sym).copied()
    }

    /// Retrieves the symbol corresponding to the give ID
    pub fn symbol(&self, id: Ident) -> &str {
        let Ident(id) = id;
        &self.symbols[id]
    }
}
