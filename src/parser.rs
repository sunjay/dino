mod scanner;
mod token;
mod lexer;
mod combinators;

pub use scanner::*;
pub use token::*;
pub use lexer::*;

use smallvec::smallvec;

use crate::ast;
use crate::ast::*;
use crate::diagnostics::Diagnostics;

use combinators::*;

use TokenKind::*;
use Delim::*;
use token::Keyword as Kw;

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
        // Make sure module ends with EOF
        suffixed(many0(decl), tk(Eof)),
        |decls| Module {decls},
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
    surrounded(kw(Kw::Use), import_path, tk(Semicolon))(input)
}

fn import_path(input: Input) -> ParseResult<ImportPath> {
    map(
        tuple((
            many0(suffixed(path_component, tk(DoubleColon))),
            import_path_selection,
        )),
        |(path, selection)| ImportPath {path, selection},
    )(input)
}

fn import_path_selection(input: Input) -> ParseResult<ImportSelection> {
    alt((
        map(tk(Star), |_| ImportSelection::All),
        map(import_names, ImportSelection::Names),
        map(path_component, ImportSelection::Component),
    ))(input)
}

fn import_names(input: Input) -> ParseResult<Vec<ImportName>> {
    surrounded(
        tk(OpenDelim(Brace)),
        separated0(tk(Comma), import_name),
        tk(CloseDelim(Brace)),
    )(input)
}

fn import_name(input: Input) -> ParseResult<ImportName> {
    alt((
        map(
            tuple((ident, opt(prefixed(kw(Kw::As), ident)))),
            |(name, alias)| ImportName::Name {name, alias},
        ),
        map(
            prefixed(kw(Kw::SelfValue), opt(prefixed(kw(Kw::As), ident))),
            |alias| ImportName::SelfValue {alias},
        ),
    ))(input)
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

fn path_component(input: Input) -> ParseResult<PathComponent> {
    todo!()
}

fn ident(input: Input) -> ParseResult<ast::Ident> {
    map(
        tk(Ident),
        |token| token.unwrap_ident().clone(),
    )(input)
}

fn kw(keyword: token::Keyword) -> impl FnMut(Input) -> ParseResult<&Token> {
    tk(Keyword(keyword))
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
