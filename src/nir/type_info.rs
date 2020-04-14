use std::sync::Arc;
use std::collections::HashMap;

use super::{def_store2::DefId, def_table::DefTable};

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
    pub fn is_empty(&self) -> bool {
        use TypeFields::*;
        match self {
            Struct(fields) => fields.is_empty(),
        }
    }

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
    pub methods: HashMap<Arc<str>, DefId>,
}

impl TypeInfo {
    pub fn new_struct(fields: DefTable) -> Self {
        Self {
            fields: TypeFields::Struct(fields),
            methods: HashMap::new(),
        }
    }
}
