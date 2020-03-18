mod scanner;
mod token;
mod lexer;
mod combinators;

pub use scanner::*;
pub use token::*;
pub use lexer::*;

use smallvec::smallvec;

use crate::ast::*;
use crate::diagnostics::Diagnostics;

use combinators::*;

use TokenKind::*;

type Input<'a> = &'a [Token];
type ParseResult<'a, O> = IResult<Input<'a>, O, &'a Token>;

impl InputItem for Token {
    type Expected = TokenKind;
}

pub fn parse_module(source: &str, diag: &Diagnostics) -> Module {
    let scanner = Scanner::new(source.as_bytes());
    let lexer = Lexer::new(scanner, diag);
    let tokens = collect_tokens(lexer);
    let module = module(&tokens);
    match module {
        Ok((input, module)) => {
            assert!(input.is_empty(), "bug: parser did not consume all input");
            module
        },
        Err((input, err)) => {
            todo!()
        },
    }
}

fn collect_tokens(mut lexer: Lexer) -> Vec<Token> {
    let mut tokens = Vec::new();

    loop {
        let token = lexer.next();
        if token.kind == TokenKind::Eof {
            break;
        }
        tokens.push(token);
    }

    tokens
}

fn module(input: Input) -> ParseResult<Module> {
    map(
        many0(decl),
        |decls| Module {decls}
    )(input)
}

fn decl(input: Input) -> ParseResult<Decl> {
    alt((
        map(use_decl, Decl::Import),
        map(struct_decl, Decl::Struct),
        map(impl_decl, Decl::Impl),
        map(func_decl, Decl::Function),
    ))(input)
}

fn use_decl(input: Input) -> ParseResult<ImportPath> {
    todo!()
}

fn struct_decl(input: Input) -> ParseResult<Struct> {
    todo!()
}

fn impl_decl(input: Input) -> ParseResult<Impl> {
    todo!()
}

fn func_decl(input: Input) -> ParseResult<Function> {
    todo!()
}

fn tk(kind: TokenKind) -> impl FnMut(Input) -> ParseResult<&Token> {
    move |input| {
        let (input, token) = input.advance();
        if token.kind == kind {
            Ok((input, token))
        } else {
            Err((input, ParseError {
                expected: smallvec![kind],
                actual: token,
            }))
        }
    }
}
