use std::borrow::Cow;
use std::sync::atomic::{AtomicUsize, Ordering};

use parking_lot::{MutexGuard, RwLockReadGuard};

use crate::span::Span;
use crate::source_files::SourceFiles;

use super::OutputStream;
use super::writer::DiagnosticsWriter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Level {
    Error,
    Warning,
    Info,
    Note,
    Help,
}

/// A labelled piece of text, prefixed and colored by its label
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Message<'a> {
    pub level: Level,
    pub label: Cow<'a, str>,
}

/// A fragment of code with any number of annotations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fragment<'a> {
    /// The span of code to annotate
    pub span: Span,
    /// The message to put at the annotated location
    pub message: Message<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Diagnostic<'a> {
    pub title: Message<'a>,
    pub fragments: Vec<Fragment<'a>>,
}

#[must_use]
pub struct DiagnosticWriter<'a> {
    pub(super) source_files: RwLockReadGuard<'a, SourceFiles>,
    pub(super) out: MutexGuard<'a, OutputStream>,
    pub(super) errors: &'a AtomicUsize,
    pub(super) data: Diagnostic<'a>,
}

impl<'a> DiagnosticWriter<'a> {
    pub fn span_error(self, span: Span, message: impl Into<Cow<'a, str>>) -> Self {
        self.span_level(Level::Error, span, message)
    }

    pub fn span_warning(self, span: Span, message: impl Into<Cow<'a, str>>) -> Self {
        self.span_level(Level::Warning, span, message)
    }

    pub fn span_info(self, span: Span, message: impl Into<Cow<'a, str>>) -> Self {
        self.span_level(Level::Info, span, message)
    }

    pub fn span_note(self, span: Span, message: impl Into<Cow<'a, str>>) -> Self {
        self.span_level(Level::Note, span, message)
    }

    pub fn span_help(self, span: Span, message: impl Into<Cow<'a, str>>) -> Self {
        self.span_level(Level::Help, span, message)
    }

    pub fn span_level(mut self, level: Level, span: Span, message: impl Into<Cow<'a, str>>) -> Self {
        self.data.fragments.push(Fragment {
            span,
            message: Message {
                level,
                label: message.into(),
            },
        });
        self
    }

    pub fn emit(self) {
        let Self {source_files, mut out, errors, data} = self;
        let Diagnostic {title, fragments} = &data;

        if title.level == Level::Error {
            errors.fetch_add(1, Ordering::SeqCst);
        }

        let frags = &fragments[..];
        let frags = match frags {
            // Skip the first fragment if it is the same as the title
            [frag] if frag.message == *title => &frags[1..],
            frags => frags,
        };

        emit_message(&source_files, &mut out, None, title);
        for frag in frags {
            let &Fragment {span, ref message} = frag;
            emit_message(&source_files, &mut out, Some(span), message);
        }
    }
}

fn emit_message(
    source_files: &SourceFiles,
    out: &mut OutputStream,
    span: Option<Span>,
    message: &Message,
) {
    let Message {level, label} = message;
    let label = match span {
        Some(span) => {
            let start_line = source_files.line(span.start);
            let end_line = source_files.line(span.end);
            if start_line == end_line {
                Cow::Owned(format!("L{}: {}", start_line, label))
            } else {
                Cow::Owned(format!("L{}-L{}: {}", start_line, end_line, label))
            }
        },

        None => label.clone(),
    };

    use Level::*;
    match level {
        Error => out.write_error(&label).expect("IO error"),
        Warning => out.write_warning(&label).expect("IO error"),
        Info => out.write_info(&label).expect("IO error"),
        Note => out.write_note(&label).expect("IO error"),
        Help => out.write_help(&label).expect("IO error"),
    }
}
