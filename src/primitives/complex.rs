use crate::package::{Package, FuncSig, FuncParam, LiteralConstructors};

use super::{Primitives, define_prim};

pub fn populate(package: &mut Package, prims: &Primitives) {
    define_prim! {
        given package, prims

        literal_constructors: LiteralConstructors {
            int_literal_constructor: Some("__dino__DComplex_from_int_literal".into()),
            real_literal_constructor: Some("__dino__DComplex_from_real_literal".into()),
            complex_literal_constructor: Some("__dino__DComplex_from_complex_literal".into()),
            ..LiteralConstructors::default()
        };

        impl complex {
            fn eq(self, other: complex) -> bool;
            fn gt(self, other: complex) -> bool;
            fn gte(self, other: complex) -> bool;
            fn lt(self, other: complex) -> bool;
            fn lte(self, other: complex) -> bool;

            fn add(self, other: complex) -> complex;
            fn sub(self, other: complex) -> complex;
            fn mul(self, other: complex) -> complex;
            fn div(self, other: complex) -> complex;
            fn pos(self) -> complex;
            fn neg(self) -> complex;
        }
    }
}
