use std::io::{self, Write};
use std::borrow::Cow;
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};

use termcolor::{StandardStream, ColorChoice, ColorSpec, Color, WriteColor};

trait DiagnosticsWriter {
    fn write_error(&mut self, message: &str) -> io::Result<()>;
    fn write_warning(&mut self, message: &str) -> io::Result<()>;
}

impl DiagnosticsWriter for StandardStream {
    fn write_error(&mut self, message: &str) -> io::Result<()> {
        let mut out = self.lock();

        out.set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))?;
        write!(out, "error: ")?;
        out.reset()?;

        writeln!(out, "{}", message)
    }

    fn write_warning(&mut self, message: &str) -> io::Result<()> {
        let mut out = self.lock();

        out.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true))?;
        write!(out, "warning: ")?;
        out.reset()?;

        writeln!(out, "{}", message)
    }
}

#[cfg(test)]
struct NullWriter;

#[cfg(test)]
impl NullWriter {
    pub fn new(_color_choice: ColorChoice) -> Self {
        // This impl exists to silence an unused parameter warning
        NullWriter
    }
}

#[cfg(test)]
impl DiagnosticsWriter for NullWriter {
    fn write_error(&mut self, _message: &str) -> io::Result<()> {
        Ok(())
    }

    fn write_warning(&mut self, _message: &str) -> io::Result<()> {
        Ok(())
    }
}

pub struct Diagnostics {
    #[cfg(not(test))]
    out: Mutex<StandardStream>,
    #[cfg(test)]
    out: Mutex<NullWriter>,
    /// The number of errors that have been emitted
    errors: AtomicUsize,
    /// The number of warnings that have been emitted
    warnings: AtomicUsize,
}

impl Diagnostics {
    pub fn new(color_choice: ColorChoice) -> Self {
        Self {
            #[cfg(not(test))]
            out: Mutex::new(StandardStream::stderr(color_choice)),
            #[cfg(test)]
            out: Mutex::new(NullWriter::new(color_choice)),
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
