#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a [u8],
    current: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            current: 0,
        }
    }

    /// Returns the current position (in bytes) in the source
    pub fn current_pos(&self) -> usize {
        self.current
    }

    /// Returns the next character in the source text or returns None if there are no more left
    pub fn next(&mut self) -> Option<u8> {
        let ch = self.peek()?;
        self.current += 1;
        Some(ch)
    }

    /// Advances the scanner twice
    ///
    /// Usually used after a call to `peek2`.
    pub fn next2(&mut self) {
        self.next();
        self.next();
    }

    /// Returns the next character in the source text, but does not advance the scanner
    pub fn peek(&self) -> Option<u8> {
        self.source.get(self.current).copied()
    }

    /// Returns the next character after the next character in the source text, but does not
    /// advance the scanner
    pub fn peek2(&self) -> Option<u8> {
        self.source.get(self.current+1).copied()
    }

    /// Creates a new span for a single byte
    pub fn byte_span(&self, index: usize) -> &'a [u8] {
        self.span(index, index+1)
    }

    /// Advances the scanner, and creates a new span between `start` and its current position
    ///
    /// Usually used after `peek` to include that character in the span
    pub fn next_span(&mut self, start: usize) -> &'a [u8] {
        self.next();
        self.span(start, self.current)
    }

    /// Creates a new span between the given byte indexes
    ///
    /// `start` is included in the range, `end` is not.
    pub fn span(&self, start: usize, end: usize) -> &'a [u8] {
        &self.source[start..end]
    }
}
