//! Types for representing a complete compilation unit, all of its declarations, and information
//! about its generated code.

use std::sync::Arc;
use std::collections::HashMap;

use parking_lot::{Mutex, MutexGuard, MappedMutexGuard};

use crate::nir::{DefId, DefStore, DefStoreSync, DefData};

/// An ID for a package
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PkgId(usize);

#[derive(Debug)]
pub struct Package {
    /// The ID of the package
    id: PkgId,
    /// The root module of the package
    ///
    /// The name of the root module is the same as the package name.
    root: DefId,
    /// The storage for all definitions in the package
    store: DefStoreSync,
}

impl Package {
    /// Returns the ID of this package
    pub fn id(&self) -> PkgId {
        self.id
    }

    /// Returns the root module of the package
    pub fn root(&mut self) -> &mut Module {
        &mut self.root
    }
}

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
    pub fn insert(&mut self, name: Arc<str>) -> Result<&mut Package, Arc<str>> {
        if self.ids.contains_key(&name) {
            return Err(name);
        }

        let pkg_id = PkgId(self.packages.len());

        let store = Arc::new(Mutex::new(DefStore::new(pkg_id)));
        let root_id = store.lock().push(name.clone(), DefData::Module);
        let root = Module {
            id: root_id,
            name: name.clone(),
            modules: DefTable::new(store.clone()),
            types: DefTable::new(store.clone()),
            functions: DefTable::new(store.clone()),
        };
        self.packages.push(Package {
            id: pkg_id,
            root,
            store,
        });

        self.ids.insert(name, pkg_id);

        // This unwrap is safe because we just pushed
        Ok(self.packages.last_mut().unwrap())
    }

    /// Returns the package with the given ID
    pub fn package(&self, id: PkgId) -> &Package {
        let PkgId(index) = id;
        &self.packages[index]
    }

    /// Returns the package with the given ID
    pub fn package_mut(&mut self, id: PkgId) -> &mut Package {
        let PkgId(index) = id;
        &mut self.packages[index]
    }

    /// Retrieves a mutable version of the data for an item already in the store
    fn data_mut(&self, id: DefId) -> MappedMutexGuard<DefData> {
        let store = self.package(id.pkg_id()).store.lock();
        MutexGuard::map(store, |store| store.data_mut(id))
    }
}
