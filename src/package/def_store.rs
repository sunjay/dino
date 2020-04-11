use super::PkgId;

use crate::span::Span;

/// An ID for any module, type, function, variable, etc.
///
/// Note that method names in method calls do NOT get a DefId, but once the method name is
/// resolved, it gets resolved to the DefId of the method declaration. The same is true for
/// field names in field access expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId {
    pkg: PkgId,
    def_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefSpan {
    pub id: DefId,
    pub span: Span,
}
