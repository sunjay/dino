use crate::package::{Package, FuncSig, FuncParam, LiteralConstructors};

use super::{Primitives, define_prim};

pub fn populate(package: &mut Package, prims: &Primitives) {
    define_prim! {
        given package, prims

        literal_constructors: LiteralConstructors {
            int_literal_constructor: Some("__dino__DUInt_from_int_literal".into()),
            ..LiteralConstructors::default()
        };

        impl uint {
            fn eq(self, other: uint) -> bool;
        }
    }
}
