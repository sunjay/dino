use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::ops::Range;

/// A span of a source file
///
/// The only invariant is that a span `start` and `end` indexes MUST remain within the boundaries
/// of a single file. That is, you can never span two files at the same time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// The start index of the span (inclusive)
    pub start: usize,
    /// The end index of the span (exclusive)
    pub end: usize,
}

impl Span {
    /// Creates a span from the start of `self` to the end of `other`
    pub fn to(self, other: Self) -> Self {
        assert!(other.end >= self.start, "bug: span should have at least zero size");

        Self {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileHandle {
    /// The index of the first byte of the file in `SourceFiles::source`
    start: usize,
    /// The number of bytes in the file
    len: usize,
}

/// The source for a file, represented as a slice of bytes and indexed from `start_index()` onwards
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileSource<'a> {
    /// A slice of `SourceFiles::source`
    bytes: &'a [u8],
    /// The offset at which the slice of bytes was extracted from `SourceFiles::source`
    offset: usize,
}

impl<'a> FileSource<'a> {
    /// Returns the first index into this slice
    pub fn start_index(&self) -> usize {
        self.offset
    }

    /// Returns the number of bytes in the file
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Returns the byte in this file at the given index, if any
    ///
    /// Note that the index MUST be greater than or equal to `start_index()` for this method to
    /// return anything.
    pub fn get(&self, index: usize) -> Option<u8> {
        let index = index - self.offset;
        self.bytes.get(index).copied()
    }

    /// Slices from the bytes of this file's source
    pub fn slice(&self, range: Range<usize>) -> &'a [u8] {
        let Self {bytes, offset} = self;
        let Range {start, end} = range;

        &bytes[start-offset..end-offset]
    }

    /// Iterates over the bytes of this file's source, yielding the offset for each one
    pub fn iter_bytes(&self) -> impl Iterator<Item=(usize, u8)> + '_ {
        self.bytes.iter().copied().enumerate().map(move |(index, byte)| (self.offset + index, byte))
    }
}

#[derive(Debug)]
pub struct LineNumbers {
    /// The index in `SourceFiles::source` of the first byte in each line
    ///
    /// The first byte of a line is defined as either the first byte in the file or a byte
    /// immediately after a b`\n`.
    offsets: Vec<usize>,
}

impl LineNumbers {
    pub fn new(source: FileSource) -> Self {
        // There is always at least one line, starting at offset 0
        let mut offsets = vec![source.start_index()];

        for (offset, ch) in source.iter_bytes() {
            if ch == b'\n' {
                // Each line starts right *after* each newline
                offsets.push(offset+1);
            }
        }

        // Push the offset for one past the end of the file. This allows us to index into `offsets`
        // without worrying about panicking since any result of `binary_search` for a valid index
        // into the file will have a corresponding value in `offsets`. (this simplifies some code)
        offsets.push(source.start_index() + source.len());

        Self {offsets}
    }

    /// Returns the (line number, offset) corresponding to the given index in the source file
    ///
    /// Both the line number and offset are 1-based
    pub fn number_offset(&self, index: usize) -> (usize, usize) {
        let line;
        let offset;

        match self.offsets.binary_search(&index) {
            // `index` corresponds to the first byte of a line
            Ok(i) => {
                line = i+1;
                offset = 1;
            },

            // `index` is on the line `i`
            Err(i) => {
                line = i;
                // the offset of the first byte of this line is at i-1 because `binary_search`
                // always finds the index *after* the first byte when `Err(_)` is returned
                offset = index - self.offsets[i-1] + 1;
            }
        }

        (line, offset)
    }
}

#[derive(Debug)]
struct File {
    path: PathBuf,
    /// The index into `SourceFiles::source` that represents the start of this file
    start_offset: usize,
    /// An index of the line numbers for all offsets in the file
    line_numbers: LineNumbers,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FilePos<'a> {
    pub path: &'a Path,
    pub start_line: usize,
    pub start_offset: usize,
    pub end_line: usize,
    pub end_offset: usize,
}

#[derive(Debug, Default)]
pub struct SourceFiles {
    /// The source code of all files concatenated together.
    ///
    /// This allows spans across files to be uniquely identifiable
    source: Vec<u8>,
    /// Metadata about each file stored in `source`
    ///
    /// Sorted by the offset
    files: Vec<File>,
}

impl SourceFiles {
    /// Reads a file and adds it to the set of source files. Returns a handle to that file's
    /// contents.
    pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<FileHandle> {
        let path = path.as_ref();
        let start = self.source.len();

        let mut file = fs::File::open(path)?;
        file.read_to_end(&mut self.source)?;

        let len = self.source.len() - start;
        Ok(self.create_handle(path, start, len))
    }

    /// Adds the given source to the set of source files. Returns a handle to that file's
    /// contents.
    pub fn add_source<P: AsRef<Path>>(&mut self, path: P, source: &[u8]) -> FileHandle {
        let path = path.as_ref();
        let start = self.source.len();

        self.source.extend(source);

        let len = source.len();
        self.create_handle(path, start, len)
    }

    fn create_handle(&mut self, path: &Path, start: usize, len: usize) -> FileHandle {
        let handle = FileHandle {start, len};
        let source = self.source(handle);
        let line_numbers = LineNumbers::new(source);
        self.files.push(File {
            path: path.to_path_buf(),
            start_offset: start,
            line_numbers,
        });
        handle
    }

    /// Returns the resolved file and position information for a span
    pub fn pos(&self, span: Span) -> FilePos {
        let File {path, line_numbers, ..} = self.file(span.start);
        let (start_line, start_offset) = line_numbers.number_offset(span.start);
        // Subtract 1 because end actually represents one past the end of the span
        let (end_line, end_offset) = line_numbers.number_offset(span.end - 1);

        FilePos {path, start_line, start_offset, end_line, end_offset}
    }

    /// Returns the path of the file whose source contains the given index
    pub fn path(&self, index: usize) -> &Path {
        &self.file(index).path
    }

    /// Returns the source for the given file handle
    pub fn source(&self, handle: FileHandle) -> FileSource {
        let FileHandle {start, len} = handle;
        FileSource {
            bytes: &self.source[start..start+len],
            offset: start,
        }
    }

    fn file(&self, index: usize) -> &File {
        let file_index = self.files.binary_search_by_key(&index, |file| file.start_offset)
            .unwrap_or_else(|index| index - 1);
        &self.files[file_index]
    }
}
