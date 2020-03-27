use std::io::{self, Write};

use termcolor::{StandardStream, StandardStreamLock, ColorSpec, Color, WriteColor};

pub trait DiagnosticsWriter {
    fn write_error(&mut self, message: &str) -> io::Result<()>;
    fn write_warning(&mut self, message: &str) -> io::Result<()>;
    fn write_info(&mut self, message: &str) -> io::Result<()>;
    fn write_note(&mut self, message: &str) -> io::Result<()>;
    fn write_help(&mut self, message: &str) -> io::Result<()>;
    fn write_newline(&mut self) -> io::Result<()>;
}

impl DiagnosticsWriter for StandardStream {
    fn write_error(&mut self, message: &str) -> io::Result<()> {
        write_message(self.lock(), "error:", Color::Red, message)
    }

    fn write_warning(&mut self, message: &str) -> io::Result<()> {
        write_message(self.lock(), "warning:", Color::Yellow, message)
    }

    fn write_info(&mut self, message: &str) -> io::Result<()> {
        write_message(self.lock(), "info:", Color::Green, message)
    }

    fn write_note(&mut self, message: &str) -> io::Result<()> {
        write_message(self.lock(), "note:", Color::White, message)
    }

    fn write_help(&mut self, message: &str) -> io::Result<()> {
        write_message(self.lock(), "help:", Color::Blue, message)
    }

    fn write_newline(&mut self) -> io::Result<()> {
        writeln!(self.lock())
    }
}

fn write_message(
    mut out: StandardStreamLock,
    prefix: &str,
    prefix_color: Color,
    message: &str,
) -> io::Result<()> {
    out.set_color(ColorSpec::new().set_fg(Some(prefix_color)).set_bold(true))?;
    write!(out, "{} ", prefix)?;
    out.reset()?;

    writeln!(out, "{}", message)
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

    fn write_info(&mut self, _message: &str) -> io::Result<()> {
        Ok(())
    }

    fn write_note(&mut self, _message: &str) -> io::Result<()> {
        Ok(())
    }

    fn write_help(&mut self, _message: &str) -> io::Result<()> {
        Ok(())
    }

    fn write_newline(&mut self) -> io::Result<()> {
        Ok(())
    }
}
