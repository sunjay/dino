#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// The start index of the span (inclusive)
    pub start: usize,
    /// The end index of the span (exclusive)
    pub end: usize,
}
