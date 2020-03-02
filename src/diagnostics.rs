use std::io::Write;

use termcolor::{StandardStream, ColorChoice, ColorSpec, Color, WriteColor};

pub struct Diagnostics {
    out: StandardStream,
    errors: usize,
    warnings: usize,
}

impl Diagnostics {
    pub fn new(color_choice: ColorChoice) -> Self {
        Self {
            out: StandardStream::stderr(color_choice),
            errors: 0,
            warnings: 0,
        }
    }

    /// Returns the number of errors that have been emitted
    pub fn emitted_errors(&self) -> usize {
        self.errors
    }

    /// Returns the number of warnings that have been emitted
    pub fn emitted_warnings(&self) -> usize {
        self.warnings
    }
}

//TODO: Create the real diagnostics API
impl Diagnostics {
    pub fn emit_error(&mut self, message: impl Into<String>) {
        self.out.set_color(ColorSpec::new().set_fg(Some(Color::Red))).expect("IO Error");
        writeln!(&mut self.out, "Error: {}", message.into()).expect("IO Error");
        self.out.reset().expect("IO Error");
        self.errors += 1;
    }

    pub fn emit_warning(&mut self, message: impl Into<String>) {
        self.out.set_color(ColorSpec::new().set_fg(Some(Color::Yellow))).expect("IO Error");
        writeln!(&mut self.out, "Warning: {}", message.into()).expect("IO Error");
        self.out.reset().expect("IO Error");
        self.warnings += 1;
    }
}
