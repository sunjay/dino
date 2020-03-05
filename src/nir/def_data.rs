use super::{TypeInfo, DefId, DefTable};

#[derive(Debug)]
pub enum DefData {
    Module,
    Type(TypeInfo),
    Field {ty: DefId},
    Function(super::FuncSig),
    FuncParam,
    Variable,
    /// A placeholder for a def which could not be computed; this is
    /// propagated to avoid useless error messages.
    Error,
}

impl DefData {
    pub fn new_struct(fields: DefTable) -> Self {
        DefData::Type(TypeInfo::new_struct(fields))
    }

    pub fn new_func(sig: super::FuncSig) -> Self {
        DefData::Function(sig)
    }

    pub fn unwrap_type(&self) -> &TypeInfo {
        match self {
            DefData::Type(data) => data,
            _ => unreachable!("bug: expected a type"),
        }
    }

    pub fn unwrap_type_mut(&mut self) -> &mut TypeInfo {
        match self {
            DefData::Type(data) => data,
            _ => unreachable!("bug: expected a type"),
        }
    }
}
