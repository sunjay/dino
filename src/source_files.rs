use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::ops::{Index, Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileHandle {
    /// The index of the first byte of the file in `SourceFiles::source`
    start: usize,
    /// The number of bytes in the file
    len: usize,
}

/// The slice of bytes for a file, indexed from `start_index()` onwards
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileSlice<'a> {
    /// A slice of `SourceFiles::source`
    bytes: &'a [u8],
    /// The offset at which the slice of bytes was extracted from `SourceFiles::source`
    offset: usize,
}

impl<'a> FileSlice<'a> {
    /// Returns the first index into this slice
    pub fn start_index(&self) -> usize {
        self.offset
    }

    /// Returns the number of bytes in the file
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    /// Returns the byte at the given index, if any
    pub fn get(&self, index: usize) -> Option<u8> {
        let index = index - self.offset;
        self.bytes.get(index).copied()
    }
}

impl<'a> Index<Range<usize>> for FileSlice<'a> {
    type Output = [u8];

    fn index(&self, range: Range<usize>) -> &Self::Output {
        let Self {bytes, offset} = self;
        let Range {start, end} = range;

        &bytes[start-offset..end-offset]
    }
}

#[derive(Debug, Default)]
pub struct SourceFiles {
    /// The source code of all files concatenated together.
    ///
    /// This allows spans across files to be uniquely identifiable
    source: Vec<u8>,
}

impl SourceFiles {
    /// Reads a file and adds it to the set of source files. Returns a handle to that file's
    /// contents.
    pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<FileHandle> {
        let start = self.source.len();

        let mut file = fs::File::open(path)?;
        file.read_to_end(&mut self.source)?;

        let len = self.source.len() - start;
        Ok(FileHandle {start, len})
    }

    /// Returns a file slice for the given file handle
    pub fn file(&self, handle: FileHandle) -> FileSlice {
        let FileHandle {start, len} = handle;
        FileSlice {
            bytes: &self.source[start..start+len],
            offset: start,
        }
    }
}
