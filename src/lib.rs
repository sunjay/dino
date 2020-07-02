#![deny(unused_must_use)]

pub mod source_files;
pub mod ast;
pub mod parser;

#[salsa::database(parser::ParserStorage)]
#[derive(Default)]
pub struct Compiler {
    runtime: salsa::Runtime<Compiler>,
}

impl salsa::Database for Compiler {
    fn salsa_runtime(&self) -> &salsa::Runtime<Self> {
        &self.runtime
    }

    fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<Self> {
        &mut self.runtime
    }
}
