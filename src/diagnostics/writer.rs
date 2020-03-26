use std::fmt;
use std::io::{self, Write};

use termcolor::{StandardStreamLock, ColorSpec, WriteColor};

pub use termcolor::Color;

pub trait DisplayStyled<W: io::Write> {
    fn fmt_styled(&self, out: &mut W) -> io::Result<()>;
}

impl<'a, W: io::Write> DisplayStyled<W> for &'a str {
    fn fmt_styled(&self, out: &mut W) -> io::Result<()> {
        write!(out, "{}", self)
    }
}

#[derive(Debug)]
pub struct WithStyle<T: fmt::Display> {
    pub color: Color,
    pub bold: bool,
    pub value: T,
}

impl<'a, T: fmt::Display> DisplayStyled<StandardStreamLock<'a>> for WithStyle<T> {
    fn fmt_styled(&self, out: &mut StandardStreamLock<'a>) -> io::Result<()> {
        let &Self {color, bold, ref value} = self;
        out.set_color(ColorSpec::new().set_fg(Some(color)).set_bold(bold))?;
        write!(out, "{}", value)?;
        out.reset()
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
