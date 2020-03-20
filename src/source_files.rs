use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::ops::Range;

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
}

#[derive(Debug, Default)]
pub struct SourceFiles {
    /// The source code of all files concatenated together.
    ///
    /// This allows spans across files to be uniquely identifiable
    source: Vec<u8>,
    /// The path and offset of each source file in `source`
    ///
    /// Sorted by the offset
    paths: Vec<(PathBuf, usize)>,
}

impl SourceFiles {
    /// Reads a file and adds it to the set of source files. Returns a handle to that file's
    /// contents.
    pub fn add_file<P: AsRef<Path>>(&mut self, path: P) -> io::Result<FileHandle> {
        let path = path.as_ref();

        let start = self.source.len();
        self.paths.push((path.to_path_buf(), start));

        let mut file = fs::File::open(path)?;
        file.read_to_end(&mut self.source)?;

        let len = self.source.len() - start;
        Ok(FileHandle {start, len})
    }

    /// Adds the given source to the set of source files. Returns a handle to that file's
    /// contents.
    pub fn add_source<P: AsRef<Path>>(&mut self, path: P, source: &[u8]) -> FileHandle {
        let path = path.as_ref();

        let start = self.source.len();
        self.paths.push((path.to_path_buf(), start));

        self.source.extend(source);

        let len = source.len();
        FileHandle {start, len}
    }

    /// Returns the path of the file whose source contains the given index
    pub fn path(&self, index: usize) -> &Path {
        let path_index = self.paths.binary_search_by_key(&index, |&(_, offset)| offset)
            .unwrap_or_else(|index| index - 1);
        let (path, _) = &self.paths[path_index];
        path
    }

    /// Returns the source for the given file handle
    pub fn file(&self, handle: FileHandle) -> FileSource {
        let FileHandle {start, len} = handle;
        FileSource {
            bytes: &self.source[start..start+len],
            offset: start,
        }
    }
}
