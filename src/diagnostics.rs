use std::sync::atomic::{AtomicUsize, Ordering};

use annotate_snippets::formatter::DisplayListFormatter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ColorChoice {
    /// Colors are enabled
    Enabled,
    /// Colors are disabled
    Disabled,
}

impl ColorChoice {
    fn into_bool(self) -> bool {
        match self {
            ColorChoice::Enabled => true,
            ColorChoice::Disabled => false,
        }
    }
}

pub struct Diagnostics {
    formatter: DisplayListFormatter,
    /// The number of errors that have been emitted
    errors: AtomicUsize,
    /// The number of warnings that have been emitted
    warnings: AtomicUsize,
}

impl Diagnostics {
    pub fn new(color_choice: ColorChoice) -> Self {
        Self {
            formatter: DisplayListFormatter::new(color_choice.into_bool(), false),
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
