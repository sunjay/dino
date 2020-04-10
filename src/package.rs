//! Types for representing a complete compilation unit, all of its declarations, and information
//! about its generated code.

use std::sync::Arc;
use std::collections::HashMap;

use crate::nir::{self, DefId};

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
    root: Module,
}

#[derive(Debug)]
pub struct Module {
    name: Arc<str>,
    /// The submodules of this module, guaranteed to have unique names
    modules: Vec<Module>,
    /// The types defined in this module, guaranteed to have unique names
    types: Vec<Type>,
}

#[derive(Debug)]
pub enum TypeKind {
    Struct(Struct),
}

#[derive(Debug)]
pub struct Struct {
    name: Arc<str>,
    /// The fields of the struct, guaranteed to have unique names
    fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct StructField {
    pub name: Arc<str>,
    pub typ: DefId,
}

#[derive(Debug)]
pub struct Type {
    kind: TypeKind,
    /// All functions scoped within this type (through `impl` blocks)
    methods: Vec<Function>,
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
