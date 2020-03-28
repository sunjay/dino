use std::fmt;
use std::sync::Arc;
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

/// Interned C identifiers
#[derive(Debug, Default)]
pub struct CSymbols {
    idents: HashMap<Arc<str>, Ident>,
    /// Every symbol `Arc<str>` will only be added once to this field
    symbols: Vec<Arc<str>>,
}

impl CSymbols {
    /// Inserts a new symbol into the symbol table and returns its ID
    ///
    /// If the symbol was already present in the table, its old ID will be returned.
    pub fn insert(&mut self, sym: &str) -> Ident {
        match self.idents.get(sym).copied() {
            Some(id) => id,
            None => {
                let sym: Arc<str> = sym.into();
                let id = Ident(self.symbols.len());
                self.symbols.push(sym.clone());
                self.idents.insert(sym, id);
                id
            }
        }
    }

    /// Retrieves the symbol corresponding to the give ID
    pub fn symbol(&self, id: Ident) -> &str {
        let Ident(id) = id;
        &self.symbols[id]
    }
}
