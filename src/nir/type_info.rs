use std::collections::HashMap;

use super::{DefId, FuncSig};

/// A named-field of a type
#[derive(Debug)]
pub struct NamedField {
    pub name: DefId,
    pub ty: DefId,
}

#[derive(Debug)]
pub enum TypeFields {
    Struct(Vec<NamedField>),
}

/// The type information stored for each type
#[derive(Debug)]
pub struct TypeInfo {
    pub fields: TypeFields,
    /// Mapping of method name to its function `DefId`
    pub methods: HashMap<String, DefId>,
}

impl TypeInfo {
    pub fn new_struct() -> Self {
        Self {
            fields: TypeFields::Struct(Vec::new()),
            methods: HashMap::new(),
        }
    }

    /// Pushes a new struct field into the type
    ///
    /// The field name MUST be unique. That is, the same field should not be pushed twice.
    pub fn push_struct_field(&mut self, name: DefId, ty: DefId) {
        use TypeFields::*;
        match &mut self.fields {
            Struct(fields) => fields.push(NamedField {name, ty}),
        }
    }
}
