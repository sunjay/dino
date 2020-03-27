use std::iter::once;

use super::FileSource;

#[derive(Debug)]
pub struct LineNumbers {
    /// The index in `SourceFiles::source` of the start of each line
    ///
    /// An index into this field is 1 less than the line number of
    /// the offset contained at that index.
    offsets: Vec<usize>,
}

impl LineNumbers {
    pub fn new(source: FileSource) -> Self {
        // There is always at least one line, starting at offset 0
        let offsets = once(source.start_index()).chain(source.iter_bytes().filter_map(|(offset, ch)| match ch {
            // Each line starts right after each newline
            b'\n' => Some(offset + 1),
            _ => None,
        })).collect();

        Self {offsets}
    }

    /// Returns the line number corresponding to the given index in the source file
    pub fn number(&self, index: usize) -> usize {
        self.offsets.binary_search(&index).unwrap_or_else(|index| index)
    }
}
