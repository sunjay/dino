use std::io::{self, Write};

use termcolor::{StandardStream, ColorSpec, Color, WriteColor};

pub trait DiagnosticsWriter {
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
pub struct NullWriter;

#[cfg(test)]
impl NullWriter {
    pub fn new(_color_choice: termcolor::ColorChoice) -> Self {
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
