use crate::package::{Package, FuncSig, FuncParam, LiteralConstructors};

use super::{Primitives, define_prim};

pub fn populate(package: &mut Package, prims: &Primitives) {
    define_prim! {
        given package, prims

        literal_constructors: LiteralConstructors {
            bstr_literal_constructor: Some("__dino__DBStr_from_bstr_literal".into()),
            ..LiteralConstructors::default()
        };

        impl bstr {
            fn eq(self, other: bstr) -> bool;
        }
    }
}
