use super::TypeInfo;

#[derive(Debug)]
pub enum DefData {
    Module,
    Type(TypeInfo),
    Field,
    Function(super::FuncSig),
    FuncParam,
    Variable,
    /// A placeholder for a def which could not be computed; this is
    /// propagated to avoid useless error messages.
    Error,
}

impl DefData {
    pub fn new_struct() -> Self {
        DefData::Type(TypeInfo::new_struct())
    }

    pub fn new_func(sig: super::FuncSig) -> Self {
        DefData::Function(sig)
    }
}
