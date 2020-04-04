//! Types for representing a complete compilation unit, all of its declarations, and information
//! about its generated code.

use std::sync::Arc;
use std::collections::HashMap;

use parking_lot::Mutex;

use crate::def_table::{DefId, DefTable, DefStore, DefStoreSync};

#[derive(Debug)]
pub struct Package {
    /// The ID of the package
    id: PkgId,
    /// The root module of the package
    ///
    /// The name of the root module is the same as the package name.
    root: Module,
    /// The storage for all definitions in the package
    store: DefStoreSync,
}

#[derive(Debug)]
pub struct Module {
    id: DefId,
    name: Arc<str>,
    modules: DefTable<Item>,
    types: DefTable<Item>,
}

#[derive(Debug)]
pub enum Item {
    Module(Module),
}

/// An ID for a package
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PkgId(usize);

/// The registery of all packages in the root scope
#[derive(Debug, Default)]
pub struct Packages {
    /// Mapping of package name to the package ID
    ids:  HashMap<Arc<str>, PkgId>,
    /// The stored packages, indexed by PkgId
    packages: Vec<Package>,
}

impl Packages {
    /// Inserts a new package with the given name. Returns a new package ID.
    ///
    /// Returns an error if the package was already present
    pub fn insert(&mut self, name: Arc<str>) -> Result<PkgId, Arc<str>> {
        if self.ids.contains_key(&name) {
            return Err(name);
        }

        let pkg_id = PkgId(self.packages.len());

        let store = Arc::new(Mutex::new(DefStore::new(pkg_id)));
        let root_id = store.lock().push(name.clone(), DefData::Module);
        let root = Module {
            id: root_id,
            name: name.clone(),
            modules: HashMap::new(),
        };
        self.packages.push(Package {
            id: pkg_id,
            root,
            store,
        });

        self.ids.insert(name, pkg_id);

        Ok(pkg_id)
    }
}
