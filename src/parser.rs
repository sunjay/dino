mod scanner;
mod token;
mod lexer;
mod combinators;

pub use scanner::*;
pub use token::*;
pub use lexer::*;

use std::sync::Arc;

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
    alt((
        map(cond, Stmt::Cond),
        map(while_loop, Stmt::WhileLoop),
        map(var_decl, Stmt::VarDecl),
        map(suffixed(expr, tk(Semicolon)), Stmt::Expr),
    ))(input)
}

fn while_loop(input: Input) -> ParseResult<WhileLoop> {
    map(
        tuple((kw(Kw::While), expr, block)),
        |(_, cond, body)| WhileLoop {cond, body},
    )(input)
}

fn var_decl(input: Input) -> ParseResult<VarDecl> {
    map(
        tuple((kw(Kw::Let), ident, opt(prefixed(tk(Colon), ty)), tk(Equals), expr, tk(Semicolon))),
        |(_, name, ty, _, expr, _)| VarDecl {name, ty, expr},
    )(input)
}

fn expr(input: Input) -> ParseResult<Expr> {
    prec0(input)
}

fn prec0(input: Input) -> ParseResult<Expr> {
    alt((
        map(kw(Kw::Break), |_| Expr::Break),
        map(kw(Kw::Continue), |_| Expr::Continue),
        map(prefixed(kw(Kw::Return), opt(expr)), |expr| Expr::Return(expr.map(Box::new))),
        prec1,
    ))(input)
}

fn prec1(input: Input) -> ParseResult<Expr> {
    alt((
        map(
            tuple((prec2, tk(Equals), expr)),
            |(lhs, _, rhs)| Expr::Assign(Box::new(Assign {lhs, rhs})),
        ),
        prec2,
    ))(input)
}

fn prec2(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec3,
        prefixed(tk(DoubleOr), prec3),
        |lhs, rhs| Expr::BoolOp(Box::new(lhs), BoolOp::Or, Box::new(rhs)),
    )(input)
}

fn prec3(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec4,
        prefixed(tk(DoubleAnd), prec4),
        |lhs, rhs| Expr::BoolOp(Box::new(lhs), BoolOp::And, Box::new(rhs)),
    )(input)
}

fn prec4(input: Input) -> ParseResult<Expr> {
    let compare_op = alt((
        map(tk(DoubleEquals), |_| CompareOp::Eq),
        map(tk(NotEqual), |_| CompareOp::Ne),
        map(tk(LessThan), |_| CompareOp::Lt),
        map(tk(LessThanEquals), |_| CompareOp::Le),
        map(tk(GreaterThan), |_| CompareOp::Gt),
        map(tk(GreaterThanEquals), |_| CompareOp::Ge),
    ));

    alt((
        map(
            tuple((prec5, compare_op, prec5)),
            |(lhs, op, rhs)| Expr::CompareOp(Box::new(lhs), op, Box::new(rhs)),
        ),
        prec5,
    ))(input)
}

fn prec5(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec6,
        prefixed(tk(Or), prec6),
        |lhs, rhs| Expr::BitwiseOp(Box::new(lhs), BitwiseOp::Or, Box::new(rhs)),
    )(input)
}

fn prec6(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec7,
        prefixed(tk(Tilde), prec7),
        |lhs, rhs| Expr::BitwiseOp(Box::new(lhs), BitwiseOp::Xor, Box::new(rhs)),
    )(input)
}

fn prec7(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec8,
        prefixed(tk(And), prec8),
        |lhs, rhs| Expr::BitwiseOp(Box::new(lhs), BitwiseOp::And, Box::new(rhs)),
    )(input)
}

fn prec8(input: Input) -> ParseResult<Expr> {
    let shift_op = alt((
        map(tk(Shl), |_| BitwiseOp::Shl),
        map(tk(Shr), |_| BitwiseOp::Shr),
    ));

    fold_many0(
        prec9,
        tuple((shift_op, prec9)),
        |lhs, (op, rhs)| Expr::BitwiseOp(Box::new(lhs), op, Box::new(rhs)),
    )(input)
}

fn prec9(input: Input) -> ParseResult<Expr> {
    let numeric_op = alt((
        map(tk(Plus), |_| NumericOp::Add),
        map(tk(Minus), |_| NumericOp::Sub),
    ));

    fold_many0(
        prec10,
        tuple((numeric_op, prec10)),
        |lhs, (op, rhs)| Expr::NumericOp(Box::new(lhs), op, Box::new(rhs)),
    )(input)
}

fn prec10(input: Input) -> ParseResult<Expr> {
    let numeric_op = alt((
        map(tk(Star), |_| NumericOp::Mul),
        map(tk(Slash), |_| NumericOp::Div),
        map(tk(Percent), |_| NumericOp::Rem),
    ));

    fold_many0(
        prec11,
        tuple((numeric_op, prec11)),
        |lhs, (op, rhs)| Expr::NumericOp(Box::new(lhs), op, Box::new(rhs)),
    )(input)
}

fn prec11(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec12,
        prefixed(kw(Kw::As), ty),
        |lhs, ty| Expr::CastAs(Box::new(lhs), ty),
    )(input)
}

fn prec12(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec13,
        prefixed(tk(Caret), prec13),
        |lhs, rhs| Expr::NumericOp(Box::new(lhs), NumericOp::Pow, Box::new(rhs)),
    )(input)
}

fn prec13(input: Input) -> ParseResult<Expr> {
    let unary_op = alt((
        map(tk(Plus), |_| UnaryOp::Pos),
        map(tk(Minus), |_| UnaryOp::Neg),
        map(tk(Not), |_| UnaryOp::Not),
    ));

    map(
        tuple((opt(unary_op), prec14)),
        |(op, expr)| match op {
            Some(op) => Expr::UnaryOp(op, Box::new(expr)),
            None => expr,
        },
    )(input)
}

fn prec14(input: Input) -> ParseResult<Expr> {
    enum Postfix {
        FuncCall(Vec<Expr>),
        Index(Expr),
    }

    let postfix_op = alt((
        map(parens(comma_separated(expr)), Postfix::FuncCall),
        map(brackets(expr), Postfix::Index),
    ));

    fold_many0(
        prec15,
        postfix_op,
        |value, op| match op {
            Postfix::FuncCall(args) => Expr::Call(Box::new(FuncCall {value, args})),
            Postfix::Index(expr) => Expr::Index(Box::new(Index {value, expr})),
        },
    )(input)
}

fn prec15(input: Input) -> ParseResult<Expr> {
    let postfix_op = tuple((tk(Period), ident, opt(parens(comma_separated(expr)))));

    fold_many0(
        prec16,
        postfix_op,
        |lhs, op| match op {
            (_, method_name, Some(args)) => {
                Expr::MethodCall(Box::new(MethodCall {lhs, method_name, args}))
            },
            (_, field, None) => {
                Expr::FieldAccess(Box::new(FieldAccess {lhs, field}))
            },
        },
    )(input)
}

fn prec16(input: Input) -> ParseResult<Expr> {
    alt((
        parens(expr),
        map(block, |block| Expr::Block(Box::new(block))),
        map(cond, |cond| Expr::Cond(Box::new(cond))),
        map(struct_lit, Expr::StructLiteral),
        map(bstr_lit, Expr::BStrLiteral),
        map(int_lit, Expr::IntegerLiteral),
        map(real_lit, Expr::RealLiteral),
        map(complex_lit, Expr::ComplexLiteral),
        map(bool_lit, Expr::BoolLiteral),
        map(unit_lit, |_| Expr::UnitLiteral),
        map(kw(Kw::SelfValue), |_| Expr::SelfValue),
        map(path, Expr::Path),
    ))(input)
}

fn cond(input: Input) -> ParseResult<Cond> {
    todo!()
}

fn struct_lit(input: Input) -> ParseResult<StructLiteral> {
    todo!()
}

fn bstr_lit(input: Input) -> ParseResult<Arc<[u8]>> {
    todo!()
}

fn int_lit(input: Input) -> ParseResult<IntegerLiteral> {
    todo!()
}

fn real_lit(input: Input) -> ParseResult<f64> {
    todo!()
}

fn complex_lit(input: Input) -> ParseResult<f64> {
    todo!()
}

fn bool_lit(input: Input) -> ParseResult<bool> {
    todo!()
}

fn unit_lit(input: Input) -> ParseResult<()> {
    todo!()
}

fn path(input: Input) -> ParseResult<Path> {
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

fn brackets<'a, F, O>(f: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, O>
    where F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    surrounded(tk(OpenDelim(Bracket)), f, tk(CloseDelim(Bracket)))
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
