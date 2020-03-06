mod scanner;
mod lexer;

use crate::ast::*;
use crate::diagnostics::Diagnostics;

use scanner::Scanner;
use lexer::{Lexer, TokenKind};

pub fn parse_module<'a>(source: &'a str, diag: &Diagnostics) -> Module<'a> {
    let scanner = Scanner::new(source.as_bytes());
    let mut lexer = Lexer::new(scanner, diag);
    loop {
        let token = lexer.next();
        println!("{:?} {:?}", token.kind, std::str::from_utf8(token.span).unwrap());
        if token.kind == TokenKind::Eof {
            break;
        }
    }
    todo!()
}
