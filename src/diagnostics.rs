use std::io::Write;
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use termcolor::{StandardStream, ColorChoice, ColorSpec, Color, WriteColor};

pub struct Diagnostics {
    out: Mutex<StandardStream>,
    errors: AtomicUsize,
    warnings: AtomicUsize,
}

impl Diagnostics {
    pub fn new(color_choice: ColorChoice) -> Self {
        Self {
            out: Mutex::new(StandardStream::stderr(color_choice)),
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
    pub fn emit_error(&self, message: impl Into<String>) {
        let mut out = self.out.lock().expect("lock poisoned");
        out.set_color(ColorSpec::new().set_fg(Some(Color::Red))).expect("IO Error");
        writeln!(out, "Error: {}", message.into()).expect("IO Error");
        out.reset().expect("IO Error");
        self.errors.fetch_add(1, Ordering::SeqCst);
    }

    pub fn emit_warning(&self, message: impl Into<String>) {
        let mut out = self.out.lock().expect("lock poisoned");
        out.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).expect("IO Error");
        writeln!(out, "Warning: {}", message.into()).expect("IO Error");
        out.reset().expect("IO Error");
        self.warnings.fetch_add(1, Ordering::SeqCst);
    }
}
