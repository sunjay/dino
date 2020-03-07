mod scanner;
mod lexer;

use crate::ast::*;
use crate::diagnostics::Diagnostics;

use scanner::Scanner;
use lexer::{Lexer, Token, TokenKind, Lit, Delim};

use TokenKind::*;

pub fn parse_module<'d, 's: 'd>(source: &'s str, diag: &'d Diagnostics) -> Module<'s> {
    let scanner = Scanner::new(source.as_bytes());
    let lexer = Lexer::new(scanner, diag);
    let mut parser = ModuleParser {
        lexer,
        diag,
    };

    parser.module()
}

struct ModuleParser<'d, 's: 'd> {
    lexer: Lexer<'s, 'd>,
    diag: &'d Diagnostics,
}

impl<'d, 's: 'd> ModuleParser<'d, 's> {
    fn module(&mut self) -> Module<'s> {
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

    fn decl(&mut self, token: Token<'s>) -> Decl<'s> {
        todo!()
    }
}
