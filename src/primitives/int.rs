use crate::package::{Package, FuncSig, FuncParam, LiteralConstructors};

use super::{Primitives, define_prim};

pub fn populate(package: &mut Package, prims: &Primitives) {
    define_prim! {
        given package, prims

        literal_constructors: LiteralConstructors {
            int_literal_constructor: Some("__dino__DInt_from_int_literal".into()),
            ..LiteralConstructors::default()
        };

        impl int {
            fn eq(self, other: int) -> bool;
            fn gt(self, other: int) -> bool;
            fn gte(self, other: int) -> bool;
            fn lt(self, other: int) -> bool;
            fn lte(self, other: int) -> bool;

            fn add(self, other: int) -> int;
            fn sub(self, other: int) -> int;
            fn mul(self, other: int) -> int;
            fn div(self, other: int) -> int;
            fn rem(self, other: int) -> int;
            fn neg(self) -> int;
        }
    }
}
