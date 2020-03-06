mod scanner;
mod lexer;

use crate::ast::*;

use scanner::Scanner;
use lexer::Lexer;

pub fn parse_module(source: &str) -> Module {
    let scanner = Scanner::new(source);
    let lexer = Lexer::new(scanner);
    todo!()
}
