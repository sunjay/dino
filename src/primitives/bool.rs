use crate::package::{Package, FuncSig, FuncParam, LiteralConstructors};

use super::{Primitives, define_prim};

pub fn populate(package: &mut Package, prims: &Primitives) {
    define_prim! {
        given package, prims

        literal_constructors: LiteralConstructors {
            bool_literal_constructor: Some("__dino__DBool_from_bool_literal".into()),
            coerce_bool: Some("__dino__DBool_coerce_bool".into()),
            ..LiteralConstructors::default()
        };

        impl bool {
            fn eq(self, other: bool) -> bool;
            fn not(self) -> bool;
        }
    }
}
