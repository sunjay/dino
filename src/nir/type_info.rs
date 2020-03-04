use super::DefId;

/// The type information stored for each type
#[derive(Debug)]
pub struct TypeInfo {
    fields: TypeFields,
    methods: Vec<super::FuncSig>,
}

impl TypeInfo {
    pub fn new_struct() -> Self {
        Self {
            fields: TypeFields::Struct(Vec::new()),
            methods: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum TypeFields {
    Struct(Vec<NamedField>),
}

/// A named-field of a type
#[derive(Debug)]
pub struct NamedField {
    pub name: DefId,
    pub ty: DefId,
}
