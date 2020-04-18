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
            fn len(self) -> uint;

            fn eq(self, other: bstr) -> bool;
            fn gt(self, other: bstr) -> bool;
            fn gte(self, other: bstr) -> bool;
            fn lt(self, other: bstr) -> bool;
            fn lte(self, other: bstr) -> bool;

            fn add(self, other: bstr) -> bstr;

            //TODO: return u8
            fn index(self, idx: uint) -> uint;
        }
    }
}
