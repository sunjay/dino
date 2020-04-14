#[derive(Debug)]
pub enum DefKind {
    Struct,
    StructField,
    Function,
    Variable,
    /// A placeholder for a def which could not be computed; this is
    /// propagated to avoid useless error messages.
    Error,
}
