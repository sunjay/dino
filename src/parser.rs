mod scanner;
mod lexer;

use crate::ast::*;
use crate::diagnostics::Diagnostics;

use scanner::Scanner;
use lexer::{Lexer, Token, TokenKind, Lit, Delim};

use TokenKind::*;

pub fn parse_module<'a>(source: &'a str, diag: &'a Diagnostics) -> Module<'a> {
    let scanner = Scanner::new(source.as_bytes());
    let lexer = Lexer::new(scanner, diag);
    let mut parser = ModuleParser {
        lexer,
        diag,
    };

    parser.module()
}

struct ModuleParser<'a> {
    lexer: Lexer<'a>,
    diag: &'a Diagnostics,
}

impl<'a> ModuleParser<'a> {
    fn module(&mut self) -> Module<'a> {
        let mut decls = Vec::new();
        loop {
            let token = self.lexer.next();
            let decl = match &token.kind {
                Eof => break,
                _ => self.decl(token),
            };
            decls.push(decl);
        }

        Module {decls}
    }

    fn decl(&mut self, token: Token<'a>) -> Decl<'a> {
        todo!()
    }
}
