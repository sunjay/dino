mod writer;
mod diagnostic;

pub use diagnostic::*;

use std::borrow::Cow;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use parking_lot::{Mutex, RwLock};
use termcolor::ColorChoice;

use crate::span::Span;
use crate::source_files::SourceFiles;

#[cfg(not(test))]
type OutputStream = termcolor::StandardStream;
#[cfg(test)]
type OutputStream = writer::NullWriter;

pub struct Diagnostics {
    source_files: Arc<RwLock<SourceFiles>>,
    /// The stream where diagnostics will be written to
    out: Mutex<OutputStream>,
    /// The number of errors that have been emitted
    errors: AtomicUsize,
}

impl Diagnostics {
    pub fn new(source_files: Arc<RwLock<SourceFiles>>, color_choice: ColorChoice) -> Self {
        Self {
            source_files,
            #[cfg(not(test))]
            out: Mutex::new(termcolor::StandardStream::stderr(color_choice)),
            #[cfg(test)]
            out: Mutex::new(writer::NullWriter::new(color_choice)),
            errors: AtomicUsize::default(),
        }
    }

    /// Returns the number of errors that have been emitted
    pub fn emitted_errors(&self) -> usize {
        self.errors.load(Ordering::SeqCst)
    }

    pub fn error<'a>(&'a self, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.level(Level::Error, message)
    }

    pub fn warning<'a>(&'a self, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.level(Level::Warning, message)
    }

    pub fn level<'a>(&'a self, level: Level, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.diagnostic_writer(Diagnostic {
            title: Message {
                level,
                label: message.into(),
            },
            fragments: Vec::new(),
        })
    }

    pub fn span_error<'a>(&'a self, span: Span, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.span_level(Level::Error, span, message)
    }

    pub fn span_warning<'a>(&'a self, span: Span, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        self.span_level(Level::Warning, span, message)
    }

    pub fn span_level<'a>(&'a self, level: Level, span: Span, message: impl Into<Cow<'a, str>>) -> DiagnosticWriter<'a> {
        let message = message.into();

        self.level(level, message.clone())
            .span_level(level, span, message)
    }

    fn diagnostic_writer<'a>(&'a self, data: Diagnostic<'a>) -> DiagnosticWriter<'a> {
        DiagnosticWriter {
            source_files: self.source_files.read(),
            out: self.out.lock(),
            errors: &self.errors,
            data,
        }
    }
}

//TODO: Remove this deprecated API
impl Diagnostics {
    pub fn emit_error<'a>(&'a self, message: impl Into<Cow<'a, str>>) {
        self.error(message).emit();
    }

    pub fn emit_warning<'a>(&'a self, message: impl Into<Cow<'a, str>>) {
        self.warning(message).emit();
    }
}
