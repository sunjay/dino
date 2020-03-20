mod writer;

use std::borrow::Cow;
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use termcolor::ColorChoice;

use writer::DiagnosticsWriter;

pub struct Diagnostics {
    #[cfg(not(test))]
    out: Mutex<termcolor::StandardStream>,
    #[cfg(test)]
    out: Mutex<writer::NullWriter>,
    /// The number of errors that have been emitted
    errors: AtomicUsize,
    /// The number of warnings that have been emitted
    warnings: AtomicUsize,
}

impl Diagnostics {
    pub fn new(color_choice: ColorChoice) -> Self {
        Self {
            #[cfg(not(test))]
            out: Mutex::new(termcolor::StandardStream::stderr(color_choice)),
            #[cfg(test)]
            out: Mutex::new(writer::NullWriter::new(color_choice)),
            errors: AtomicUsize::default(),
            warnings: AtomicUsize::default(),
        }
    }

    /// Returns the number of errors that have been emitted
    pub fn emitted_errors(&self) -> usize {
        self.errors.load(Ordering::SeqCst)
    }

    /// Returns the number of warnings that have been emitted
    pub fn emitted_warnings(&self) -> usize {
        self.warnings.load(Ordering::SeqCst)
    }
}

//TODO: Create the real diagnostics API
impl Diagnostics {
    pub fn emit_error<'a>(&self, message: impl Into<Cow<'a, str>>) {
        let mut out = self.out.lock().expect("lock poisoned");
        out.write_error(message.into().as_ref()).expect("IO Error");

        self.errors.fetch_add(1, Ordering::SeqCst);
    }

    pub fn emit_warning<'a>(&self, message: impl Into<Cow<'a, str>>) {
        let mut out = self.out.lock().expect("lock poisoned");
        out.write_warning(message.into().as_ref()).expect("IO Error");

        self.warnings.fetch_add(1, Ordering::SeqCst);
    }
}
