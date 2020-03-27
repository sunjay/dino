use super::{TypeInfo, DefId, DefTable};

#[derive(Debug)]
pub enum DefData {
    Type(TypeInfo),
    Field {ty: DefId},
    /// The function signature will be initialized exactly once during name resolution
    Function(Option<super::FuncSig>),
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

    pub fn new_func() -> Self {
        DefData::Function(None)
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
