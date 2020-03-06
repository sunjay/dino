mod scanner;
mod lexer;

use crate::ast::*;
use crate::diagnostics::Diagnostics;

use scanner::Scanner;
use lexer::{Lexer, Token, TokenKind, Lit, Delim};

pub fn parse_module<'a, 'b>(source: &'a str, diag: &'b Diagnostics) -> Module<'a> {
    let scanner = Scanner::new(source.as_bytes());
    let lexer = Lexer::new(scanner, diag);
    let mut parser = ModuleParser {
        lexer,
        diag,
    };

    parser.module()
}

struct ModuleParser<'a, 'b> {
    lexer: Lexer<'a, 'b>,
    diag: &'b Diagnostics,
}

impl<'a, 'b> ModuleParser<'a, 'b> {
    fn module(&mut self) -> Module<'a> {
        todo!()
    }
}
