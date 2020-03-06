use std::fs;
use std::io;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileId(usize);

/// A span produces a start-inclusive, end-exclusive range within a file
///
/// Spans must not cross file-boundaries.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Start index from a file source (inclusive)
    start: usize,
    /// End index from the file source (exclusive)
    ///
    /// Must be greater than or equal to `start`
    end: usize,
}

#[derive(Debug)]
pub struct File {
    path: PathBuf,
    source: String,
}

#[derive(Debug, Default)]
pub struct Files {
    /// The total offset of the file in the list of files and the file itself.
    /// All files are considered to be one large buffer that the `Span` indexes into.
    /// The files are stored in sorted order by their offset.
    files: Vec<(usize, File)>,
}

impl Files {
    /// Adds a file by opening and reading the given path
    pub fn open<P: Into<PathBuf>>(&mut self, path: P) -> io::Result<FileId> {
        let path = path.into();
        let source = fs::read_to_string(&path)?;
        Ok(self.add_source(path, source))
    }

    /// Adds a file using the given path and source text
    pub fn add_source<P: Into<PathBuf>>(&mut self, path: P, source: String) -> FileId {
        let path = path.into();
        let offset = self.files.last().map(|(offset, f)| offset + f.source.len()).unwrap_or(0);
        self.files.push((offset, File {path, source}));
        FileId(self.files.len() - 1)
    }

    fn byte_span(
        &self,
        file: FileId,
        from_index: usize,
        to_index: usize
    ) -> Option<Span> {
        todo!()
    }

    fn file_id(&self, span: Span) -> FileId {
        let file_index = self.files.binary_search_by_key(&span.start(), |&(offset, _)| offset)
            .expect("bug: unable to find span in files");
        FileId(file_index)
    }

    fn file_name(&self, file: FileId) -> FileName {
        todo!()
    }

    fn byte_index(
        &self,
        file: FileId,
        line: usize,
        column: usize
    ) -> Option<usize> {
        todo!()
    }

    fn location(
        &self,
        file: FileId,
        byte_index: usize
    ) -> Option<Location> {
        todo!()
    }

    fn line_span(&self, file: FileId, lineno: usize) -> Option<Span> {
        todo!()
    }

    fn source(&self, span: Span) -> Option<String> {
        todo!()
    }
}
