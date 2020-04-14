//! Types for representing a complete compilation unit, all of its declarations, and information
//! about its generated code.

/// An ID for a package
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PkgId(usize);
