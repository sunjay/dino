use std::sync::{Arc, Mutex};

use crate::package::PkgId;
use crate::symbol_table::{SymbolTable, IdStore};

use super::TypeInfo;

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

#[derive(Debug)]
pub enum Def {
    Module,
    Type(TypeInfo),
    Field,
    Function(super::FuncSig),
    FuncParam,
    Variable,
    /// A placeholder for a def which could not be computed; this is
    /// propagated to avoid useless error messages.
    Error,
}

impl Def {
    pub fn new_struct() -> Self {
        Def::Type(TypeInfo::new_struct())
    }

    pub fn new_func(sig: super::FuncSig) -> Self {
        Def::Function(sig)
    }
}

/// A symbol table contain a shared `DefStore`
///
/// This allows all levels of scope to have their own table for looking up names, but a single
/// source of data for looking up `DefId`s
pub type DefTable = SymbolTable<DefStoreSync>;

/// A version of the `DefStore` that can be shared
pub type DefStoreSync = Arc<Mutex<DefStore>>;

/// Stores a mapping from `DefId` to each `Def` one particular package
#[derive(Debug)]
pub struct DefStore {
    pkg: PkgId,
    /// `def_index` indexes into this field
    defs: Vec<(String, Def)>,
}

impl DefStore {
    pub fn new(pkg: PkgId) -> Self {
        Self {
            pkg,
            defs: Vec::new(),
        }
    }
}

impl IdStore for DefStore {
    type Sym = String;
    type Id = DefId;
    type Data = Def;

    fn push(&mut self, sym: Self::Sym, data: Self::Data) -> Self::Id {
        self.defs.push((sym, data));
        DefId {
            pkg: self.pkg,
            def_index: self.defs.len() - 1,
        }
    }

    fn data(&self, id: Self::Id) -> &Self::Data {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (_, data) = &self.defs[id.def_index];
        data
    }

    fn data_mut(&mut self, id: Self::Id) -> &mut Self::Data {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (_, data) = &mut self.defs[id.def_index];
        data
    }

    fn symbol(&self, id: Self::Id) -> &Self::Sym {
        assert_eq!(id.pkg, self.pkg, "bug: attempt to access a DefId from another package");
        let (sym, _) = &self.defs[id.def_index];
        sym
    }
}
