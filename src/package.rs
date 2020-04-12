mod def_store;
mod def_kind;
mod ty;
mod scope;
mod cursor;

pub use def_store::*;
pub use def_kind::*;
pub use ty::*;
pub use scope::*;
pub use cursor::*;

use std::sync::Arc;
use std::collections::HashMap;

use parking_lot::{Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};

/// An ID for a package
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PkgId(usize);

/// Represents a complete compilation unit, all of its declarations, and information about its
/// generated code
#[derive(Debug)]
pub struct Package {
    id: PkgId,
    def_store: DefStoreSync,
    scopes: ScopeTreeSync,
}

impl Package {
    pub fn store_mut(&self) -> MutexGuard<DefStore> {
        self.def_store.lock()
    }

    pub fn scopes(&self) -> RwLockReadGuard<ScopeTree> {
        self.scopes.read()
    }

    pub fn scopes_mut(&self) -> RwLockWriteGuard<ScopeTree> {
        self.scopes.write()
    }
}

/// The registery of all packages in the root scope
#[derive(Debug, Default)]
pub struct Packages {
    packages: HashMap<Arc<str>, Package>,
    next_pkg_id: usize,
}

impl Packages {
    pub fn insert(&mut self, name: Arc<str>) -> &mut Package {
        let id = PkgId(self.next_pkg_id);
        self.next_pkg_id += 1;

        let package = Package {
            id,
            def_store: Arc::new(Mutex::new(DefStore::new(id))),
            scopes: Arc::new(RwLock::new(ScopeTree::new(id))),
        };

        assert!(!self.packages.contains_key(&name), "bug: same package inserted twice");
        self.packages.entry(name).or_insert(package)
    }
}
