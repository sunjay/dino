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
    braces(comma_separated(import_name))(input)
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
    map(
        tuple((
            kw(Kw::Struct),
            ident,
            braces(comma_separated(struct_field)),
        )),
        |(_, name, fields)| Struct {name, fields},
    )(input)
}

fn struct_field(input: Input) -> ParseResult<StructField> {
    map(
        tuple((ident, tk(Colon), ty)),
        |(name, _, ty)| StructField {name, ty},
    )(input)
}

fn impl_decl(input: Input) -> ParseResult<Impl> {
    map(
        tuple((kw(Kw::Impl), ty, braces(many0(func_decl)))),
        |(_, self_ty, methods)| Impl {self_ty, methods},
    )(input)
}

fn func_decl(input: Input) -> ParseResult<Function> {
    map(
        tuple((kw(Kw::Fn), ident, func_sig, block)),
        |(_, name, sig, body)| Function {name, sig, body},
    )(input)
}

fn func_sig(input: Input) -> ParseResult<FuncSig> {
    map(
        tuple((parens(comma_separated(func_param)), opt(prefixed(tk(RArrow), ty)))),
        |(params, return_type)| FuncSig {params, return_type},
    )(input)
}

fn func_param(input: Input) -> ParseResult<FuncParam> {
    alt((
        map(kw(Kw::SelfValue), |_| FuncParam::SelfValue),
        map(tuple((ident, tk(Colon), ty)), |(name, _, ty)| FuncParam::Named {name, ty}),
    ))(input)
}

fn block(input: Input) -> ParseResult<Block> {
    enum ParseState {
        Continue,
        FoundExpr,
        FoundBrace,
    }
    use ParseState::*;

    let (mut input, _) = tk(OpenDelim(Brace))(input)?;

    let mut decls = Vec::new();
    let mut stmts = Vec::new();
    let mut ret = None;

    // This loop is mostly equivalent to `tuple((many0(alt((decl, stmt))), opt(expr)))`
    // However, that wouldn't work because `many0` returns an error if its parser goes past the
    // start of its input. This would always happen with `stmt` because has `expr` + ';' as valid.
    loop {
        let (inp, state) = alt((
            map(decl, |decl| { decls.push(decl); Continue }),
            map(stmt, |stmt| { stmts.push(stmt); Continue }),
            map(expr, |expr| { ret = Some(expr); FoundExpr }),
            map(tk(CloseDelim(Brace)), |_| FoundBrace),
        ))(input)?;
        input = inp;

        match state {
            Continue => {},
            FoundExpr => {
                let (inp, _) = tk(CloseDelim(Brace))(input)?;
                input = inp;
                break;
            },
            FoundBrace => break,
        }
    }

    Ok((input, Block {decls, stmts, ret}))
}

fn stmt(input: Input) -> ParseResult<Stmt> {
    todo!()
}

fn expr(input: Input) -> ParseResult<Expr> {
    todo!()
}

fn path_component(input: Input) -> ParseResult<PathComponent> {
    todo!()
}

fn ty(input: Input) -> ParseResult<Ty> {
    todo!()
}

fn ident(input: Input) -> ParseResult<ast::Ident> {
    map(
        tk(Ident),
        |token| token.unwrap_ident().clone(),
    )(input)
}

fn comma_separated<'a, F, O>(f: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, Vec<O>>
    where F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    separated0(tk(Comma), f)
}

fn parens<'a, F, O>(f: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, O>
    where F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    surrounded(tk(OpenDelim(Paren)), f, tk(CloseDelim(Paren)))
}

fn braces<'a, F, O>(f: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, O>
    where F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    surrounded(tk(OpenDelim(Brace)), f, tk(CloseDelim(Brace)))
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
