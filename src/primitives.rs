use crate::resolve::{TyId, DeclMap, ExternType};

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
        unit => "" => ExternType {
            extern_name: "DUnit".to_string(),
            unit_literal_constructor: Some("__dino__DUnit_from_unit_literal".to_string()),
            ..ExternType::default()
        },

        bool => "bool" => ExternType {
            extern_name: "DBool".to_string(),
            bool_literal_constructor: Some("__dino__DBool_from_bool_literal".to_string()),
            ..ExternType::default()
        },

        int => "int" => ExternType {
            extern_name: "DInt".to_string(),
            int_literal_constructor: Some("__dino__DInt_from_int_literal".to_string()),
            ..ExternType::default()
        },

        real => "real" => ExternType {
            extern_name: "DReal".to_string(),
            int_literal_constructor: Some("__dino__DReal_from_int_literal".to_string()),
            real_literal_constructor: Some("__dino__DReal_from_real_literal".to_string()),
            ..ExternType::default()
        },

        complex => "complex" => ExternType {
            extern_name: "DComplex".to_string(),
            int_literal_constructor: Some("__dino__DComplex_from_int_literal".to_string()),
            real_literal_constructor: Some("__dino__DComplex_from_real_literal".to_string()),
            complex_literal_constructor: Some("__dino__DComplex_from_complex_literal".to_string()),
            ..ExternType::default()
        },

        bstr => "bstr" => ExternType {
            extern_name: "DBStr".to_string(),
            bstr_literal_constructor: Some("__dino__DBStr_from_bstr_literal".to_string()),
            ..ExternType::default()
        },
    }
}
