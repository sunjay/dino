use std::collections::HashMap;

use crate::resolve::{TyId, DeclMap, TypeInfo, LiteralConstructors};

macro_rules! primitives {
    (
        $(#[$attr:meta])*
        $vis:vis struct $struct_name:ident {
            $($prim:ident => $name:literal => $extern_type:expr),* $(,)?
        }
    ) => {
        $(#[$attr])*
        $vis struct $struct_name {
            $(
                $prim: TyId
            ),*
        }

        impl $struct_name {
            /// Inserts all of the primitives into the declaration map
            pub(crate) fn new(decls: &mut DeclMap) -> Self {
                Self {
                    $(
                        $prim: decls.insert_type($name, $extern_type)
                            .expect("bug: primitive type defined more than once"),
                    )*
                }
            }

            $(
                pub fn $prim(&self) -> TyId {
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
        // The unit type has a special syntax, not any particular name (like "int"), so we just use
        // an empty string here.
        unit => "" => TypeInfo {
            name: "DUnit",
            is_extern: true,
            constructors: LiteralConstructors {
                unit_literal_constructor: Some("__dino__DUnit_from_unit_literal"),
                ..LiteralConstructors::default()
            },
            fields: HashMap::default(),
            methods: HashMap::default(),
        },

        bool => "bool" => TypeInfo {
            name: "DBool",
            is_extern: true,
            constructors: LiteralConstructors {
                bool_literal_constructor: Some("__dino__DBool_from_bool_literal"),
                coerce_bool: Some("__dino__DBool_coerce_bool"),
                ..LiteralConstructors::default()
            },
            fields: HashMap::default(),
            methods: HashMap::default(),
        },

        int => "int" => TypeInfo {
            name: "DInt",
            is_extern: true,
            constructors: LiteralConstructors {
                int_literal_constructor: Some("__dino__DInt_from_int_literal"),
                ..LiteralConstructors::default()
            },
            fields: HashMap::default(),
            methods: HashMap::default(),
        },

        real => "real" => TypeInfo {
            name: "DReal",
            is_extern: true,
            constructors: LiteralConstructors {
                int_literal_constructor: Some("__dino__DReal_from_int_literal"),
                real_literal_constructor: Some("__dino__DReal_from_real_literal"),
                ..LiteralConstructors::default()
            },
            fields: HashMap::default(),
            methods: HashMap::default(),
        },

        complex => "complex" => TypeInfo {
            name: "DComplex",
            is_extern: true,
            constructors: LiteralConstructors {
                int_literal_constructor: Some("__dino__DComplex_from_int_literal"),
                real_literal_constructor: Some("__dino__DComplex_from_real_literal"),
                complex_literal_constructor: Some("__dino__DComplex_from_complex_literal"),
                ..LiteralConstructors::default()
            },
            fields: HashMap::default(),
            methods: HashMap::default(),
        },

        bstr => "bstr" => TypeInfo {
            name: "DBStr",
            is_extern: true,
            constructors: LiteralConstructors {
                bstr_literal_constructor: Some("__dino__DBStr_from_bstr_literal"),
                ..LiteralConstructors::default()
            },
            fields: HashMap::default(),
            methods: HashMap::default(),
        },
    }
}
