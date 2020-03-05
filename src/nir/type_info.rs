use std::collections::HashMap;

use super::{DefId, DefTable};

/// A named-field of a type
#[derive(Debug)]
pub struct NamedField {
    pub name: DefId,
    pub ty: DefId,
}

#[derive(Debug)]
pub enum TypeFields {
    Struct(DefTable),
}

impl TypeFields {
    pub fn struct_fields(&self) -> &DefTable {
        use TypeFields::*;
        match self {
            Struct(fields) => &fields,
        }
    }

    pub fn struct_fields_mut(&mut self) -> &mut DefTable {
        use TypeFields::*;
        match self {
            Struct(fields) => fields,
        }
    }
}

/// The type information stored for each type
#[derive(Debug)]
pub struct TypeInfo {
    pub fields: TypeFields,
    /// Mapping of method name to its function `DefId`
    pub methods: HashMap<String, DefId>,
}

impl TypeInfo {
    pub fn new_struct(fields: DefTable) -> Self {
        Self {
            fields: TypeFields::Struct(fields),
            methods: HashMap::new(),
        }
    }
}
