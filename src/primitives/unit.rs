use crate::package::{Package, FuncSig, FuncParam, LiteralConstructors};

use super::{Primitives, define_prim};

pub fn populate(package: &mut Package, prims: &Primitives) {
    define_prim! {
        given package, prims

        literal_constructors: LiteralConstructors {
            unit_literal_constructor: Some("__dino__DUnit_from_unit_literal".into()),
            ..LiteralConstructors::default()
        };

        impl unit {
            fn eq(self, other: unit) -> bool;
        }
    }
}
