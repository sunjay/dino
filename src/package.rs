//! Types for representing a complete compilation unit, all of its declarations, and information
//! about its generated code.

mod type_decl;

pub use type_decl::*;

use std::sync::Arc;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Ty {
    TyId(PkgTyId),
}

impl From<PkgTyId> for Ty {
    fn from(id: PkgTyId) -> Self {
        Ty::TyId(id)
    }
}

#[derive(Debug)]
pub struct FuncSig {
    pub self_param: bool,
    pub params: Vec<FuncParam>,
    pub return_type: Ty,
}

#[derive(Debug)]
pub struct FuncParam {
    pub name: Arc<str>,
    pub ty: Ty,
}

#[derive(Debug, Default)]
pub struct Module {
    pub modules: HashMap<Arc<str>, Module>,
    pub types: HashMap<Arc<str>, PkgTyId>,
    pub functions: HashMap<Arc<str>, FuncSig>,
}

/// Inserts a function signature (no `self` parameter) into the given module
#[macro_export]
macro_rules! insert_sig {
    (
        $module:ident;
        fn $func:ident(
            $(self)? $(,)?
            $($param:ident : $param_ty:expr),* $(,)?
        ) -> $ret_ty:expr
    ) => {
        assert!($module.functions.insert(stringify!($func).into(), FuncSig {
            self_param: false,
            params: vec![
                $(
                    FuncParam {
                        name: stringify!($param).into(),
                        ty: $param_ty.into(),
                    }
                ),*
            ],
            return_type: $ret_ty.into(),
        }).is_none(), "bug: function `{}` was inserted twice for primitive `{}`", stringify!($func), stringify!($prim));
    };
}
// Makes this name available to import
pub use crate::insert_sig;

/// An ID for a type in a package
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PkgTyId {
    pkg: PkgId,
    type_index: usize
}

#[derive(Debug)]
pub struct Package {
    id: PkgId,
    /// `PkgTyId` indexes into this field
    types: Vec<TypeDecl>,
    /// The root module in the package
    root: Module,
}

impl Package {
    pub fn insert_type(&mut self, type_decl: TypeDecl) -> PkgTyId {
        let id = PkgTyId {pkg: self.id, type_index: self.types.len()};
        self.types.push(type_decl);
        id
    }

    pub fn type_mut(&mut self, ty_id: PkgTyId) -> &mut TypeDecl {
        let PkgTyId {pkg, type_index} = ty_id;
        assert_eq!(pkg, self.id, "bug: attempt to get type defined in another package");

        &mut self.types[type_index]
    }

    pub fn root_mut(&mut self) -> &mut Module {
        &mut self.root
    }
}

/// An ID for a package
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PkgId(usize);

/// The registery of all packages in the root scope
#[derive(Debug, Default)]
pub struct Packages {
    package_ids: HashMap<Arc<str>, PkgId>,
    /// `PkgId` indexes into this field
    packages: Vec<Package>,
}

impl Packages {
    pub fn insert(&mut self, name: Arc<str>) -> PkgId {
        let id = PkgId(self.packages.len());

        let package = Package {
            id,
            types: Vec::new(),
            root: Module::default(),
        };

        assert!(!self.package_ids.contains_key(&name), "bug: package `{}` inserted twice", name);
        self.package_ids.insert(name, id);
        self.packages.push(package);

        id
    }

    pub fn package_mut(&mut self, pkg: PkgId) -> &mut Package {
        let PkgId(index) = pkg;

        &mut self.packages[index]
    }
}
