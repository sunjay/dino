mod unit;
mod bool;
mod uint;
mod int;
mod real;
mod complex;
mod bstr;

use std::collections::HashMap;

use crate::package::{Packages, PkgTyId, TypeDecl, TypeDeclKind, Struct, LiteralConstructors};

macro_rules! primitives {
    (
        $(#[$attr:meta])*
        $vis:vis struct $struct_name:ident {
            $($prim:ident),* $(,)?
        }
    ) => {
        $(#[$attr])*
        $vis struct $struct_name {
            $(
                $prim: PkgTyId
            ),*
        }

        impl $struct_name {
            pub fn new(packages: &mut Packages) -> Self {
                // The primitives are inserted as an unnamed package that is implicitly available
                // in every scope. Since it is unnamed, it cannot be looked up as a regular package.
                let prims_id = packages.insert("".into());
                let prims_pkg = packages.package_mut(prims_id);

                let prims = Self {
                    $(
                        $prim: prims_pkg.insert_type(TypeDecl {
                            // No visible fields on primitives
                            kind: TypeDeclKind::Struct(Struct::without_fields()),
                            // Methods and constructors can't be defined until all primitives are inserted
                            methods: HashMap::new(),
                            literal_constructors: LiteralConstructors::default(),
                        })
                    ),*
                };

                $($prim::populate(prims_pkg, &prims);)*

                prims
            }

            $(
                pub fn $prim(&self) -> PkgTyId {
                    self.$prim
                }
            )*
        }
    };
}

primitives! {
    /// The core primitives of the compiler
    #[derive(Debug)]
    pub struct Primitives {
        unit,
        bool,
        uint,
        int,
        real,
        complex,
        bstr,
    }
}

#[macro_export]
macro_rules! define_prim {
    (
        given $package:ident, $prims:ident

        literal_constructors: $lit_cons:expr;

        impl $prim:ident {
            $(
                fn $method:ident(
                    self $(,)?
                    $($param:ident : $param_ty:ident),* $(,)?
                ) -> $ret_ty:ident;
            )*
        }
    ) => {
        let type_decl = $package.type_mut($prims.$prim());
        type_decl.literal_constructors = $lit_cons;

        $(
            assert!(type_decl.methods.insert(stringify!($method).into(), FuncSig {
                self_param: true,
                params: vec![
                    $(
                        FuncParam {
                            name: stringify!($param).into(),
                            ty: $prims.$param_ty().into(),
                        }
                    ),*
                ],
                return_type: $prims.$ret_ty().into(),
            }).is_none(), "bug: method `{}` was inserted twice for primitive `{}`", stringify!($method), stringify!($prim));
        )*
    };
}
// Makes this name available to submodules
use crate::define_prim;
