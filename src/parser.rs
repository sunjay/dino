mod scanner;
mod token;
mod lexer;
mod combinators;

pub use scanner::*;
pub use token::*;
pub use lexer::*;

use std::sync::Arc;
use std::fmt::Write;

use smallvec::smallvec;

use crate::ast;
use crate::ast::*;
use crate::span::Span;
use crate::diagnostics::Diagnostics;
use crate::source_files::FileSource;

use combinators::*;

use TokenKind::*;
use Delim::*;
use LitKind::*;
use token::Keyword as Kw;

type Input<'a> = &'a [Token];
type ParseResult<'a, O> = IResult<Input<'a>, O, &'a Token>;

impl InputItem for Token {
    type Expected = TokenKind;
}

pub fn parse_module(source: FileSource, diag: &Diagnostics) -> Module {
    let scanner = Scanner::new(source);
    let lexer = Lexer::new(scanner, diag);
    let tokens = collect_tokens(lexer);
    let module = module(&tokens);
    match module {
        Ok((input, module)) => {
            assert!(input.is_empty(), "bug: parser did not consume all input");
            module
        },
        Err((_, err)) => {
            let ParseError {mut expected, actual} = err;
            expected.sort_unstable();

            let mut message = String::new();
            match &expected[..] {
                [] => unreachable!("bug: no parser should produce zero expected tokens"),
                [tk] => write!(message, "expected {}", tk).unwrap(),
                [tk1, tk2] => write!(message, "expected {} or {}", tk1, tk2).unwrap(),
                kinds => {
                    write!(message, "expected one of ").unwrap();
                    for kind in &kinds[..kinds.len()-1] {
                        write!(message, "{}, ", kind).unwrap();
                    }
                    write!(message, "or {}", kinds[kinds.len()-1]).unwrap();
                },
            }
            write!(message, ", found: {}", actual.kind).unwrap();
            diag.span_error(actual.span, message).emit();

            // Error recovery: return an empty module
            //TODO: Do a better job at error recovery!
            Module {
                decls: Vec::new(),
            }
        },
    }
}

fn collect_tokens(mut lexer: Lexer) -> Vec<Token> {
    let mut tokens = Vec::new();

    loop {
        let token = lexer.next();
        if token.kind == TokenKind::Eof {
            tokens.push(token);
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
            opt(suffixed(path_prefix, tk(DoubleColon))),
            many0(suffixed(ident, tk(DoubleColon))),
            import_path_selection,
        )),
        |(prefix, path, selection)| ImportPath {prefix, path, selection},
    )(input)
}

fn import_path_selection(input: Input) -> ParseResult<ImportSelection> {
    alt((
        map(tk(Star), |token| ImportSelection::All(token.span)),
        map(import_names, ImportSelection::Names),
        map(import_name, |name| ImportSelection::Names(vec![name])),
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
        map(kw(Kw::SelfValue), |token| FuncParam::SelfValue(token.span)),
        map(tuple((ident, tk(Colon), ty)), |(name, _, ty)| FuncParam::Named {name, ty}),
    ))(input)
}

fn block(input: Input) -> ParseResult<Block> {
    enum ParseState {
        Continue,
        FoundExpr,
        FoundBrace(Span),
    }
    use ParseState::*;

    let (mut input, open_brace) = tk(OpenDelim(Brace))(input)?;
    let span_start = open_brace.span;

    let mut decls = Vec::new();
    let mut stmts = Vec::new();
    let mut ret = None;

    // This loop is mostly equivalent to `tuple((many0(alt((decl, stmt))), opt(expr)))`
    // However, that wouldn't work because `many0` returns an error if its parser goes past the
    // start of its input. This would always happen with `stmt` because has `expr` + ';' as valid.
    let span_end;
    loop {
        let (inp, state) = alt((
            map(decl, |decl| { decls.push(decl); Continue }),
            map(stmt, |stmt| { stmts.push(stmt); Continue }),
            map(expr, |expr| { ret = Some(expr); FoundExpr }),
            map(tk(CloseDelim(Brace)), |token| FoundBrace(token.span)),
        ))(input)?;
        input = inp;

        match state {
            Continue => {},
            FoundExpr => {
                let (inp, close_brace) = tk(CloseDelim(Brace))(input)?;
                span_end = close_brace.span;
                input = inp;
                break;
            },
            FoundBrace(span) => {
                span_end = span;
                break;
            },
        }
    }

    let span = span_start.to(span_end);
    Ok((input, Block {decls, stmts, ret, span}))
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
        map(kw(Kw::Break), |token| Expr::Break(token.span)),
        map(kw(Kw::Continue), |token| Expr::Continue(token.span)),
        map(tuple((kw(Kw::Return), opt(expr))), |(ret_token, expr)| {
            Expr::Return(Box::new(Return {
                return_span: ret_token.span,
                expr,
            }))
        }),
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
    let range_op = alt((
        map(tk(DoublePeriod), |token| RangeOp::Exclusive(token.span)),
        map(tk(DoublePeriodEquals), |token| RangeOp::Inclusive(token.span)),
    ));

    alt((
        map(
            tuple((opt(prec3), range_op, opt(prec3))),
            |(lhs, op, rhs)| Expr::Range(Box::new(Range {lhs, op, rhs})),
        ),
        prec3,
    ))(input)
}

fn prec3(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec4,
        tuple((tk(DoubleOr), prec4)),
        |lhs, (op, rhs)| Expr::BoolOp(Box::new(Binary {lhs, op: BoolOp::Or(op.span), rhs})),
    )(input)
}

fn prec4(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec5,
        tuple((tk(DoubleAnd), prec5)),
        |lhs, (op, rhs)| Expr::BoolOp(Box::new(Binary {lhs, op: BoolOp::And(op.span), rhs})),
    )(input)
}

fn prec5(input: Input) -> ParseResult<Expr> {
    let compare_op = alt((
        map(tk(DoubleEquals), |token| CompareOp::Eq(token.span)),
        map(tk(NotEquals), |token| CompareOp::Ne(token.span)),
        map(tk(LessThan), |token| CompareOp::Lt(token.span)),
        map(tk(LessThanEquals), |token| CompareOp::Le(token.span)),
        map(tk(GreaterThan), |token| CompareOp::Gt(token.span)),
        map(tk(GreaterThanEquals), |token| CompareOp::Ge(token.span)),
    ));

    alt((
        map(
            tuple((prec6, compare_op, prec6)),
            |(lhs, op, rhs)| Expr::CompareOp(Box::new(Binary {lhs, op, rhs})),
        ),
        prec6,
    ))(input)
}

fn prec6(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec7,
        tuple((tk(Or), prec7)),
        |lhs, (op, rhs)| Expr::BitwiseOp(Box::new(Binary {lhs, op: BitwiseOp::Or(op.span), rhs})),
    )(input)
}

fn prec7(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec8,
        tuple((tk(Tilde), prec8)),
        |lhs, (op, rhs)| Expr::BitwiseOp(Box::new(Binary {lhs, op: BitwiseOp::Xor(op.span), rhs})),
    )(input)
}

fn prec8(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec9,
        tuple((tk(And), prec9)),
        |lhs, (op, rhs)| Expr::BitwiseOp(Box::new(Binary {lhs, op: BitwiseOp::And(op.span), rhs})),
    )(input)
}

fn prec9(input: Input) -> ParseResult<Expr> {
    let shift_op = alt((
        map(tk(Shl), |token| BitwiseOp::Shl(token.span)),
        map(tk(Shr), |token| BitwiseOp::Shr(token.span)),
    ));

    fold_many0(
        prec10,
        tuple((shift_op, prec10)),
        |lhs, (op, rhs)| Expr::BitwiseOp(Box::new(Binary {lhs, op, rhs})),
    )(input)
}

fn prec10(input: Input) -> ParseResult<Expr> {
    let numeric_op = alt((
        map(tk(Plus), |token| NumericOp::Add(token.span)),
        map(tk(Minus), |token| NumericOp::Sub(token.span)),
    ));

    fold_many0(
        prec11,
        tuple((numeric_op, prec11)),
        |lhs, (op, rhs)| Expr::NumericOp(Box::new(Binary {lhs, op, rhs})),
    )(input)
}

fn prec11(input: Input) -> ParseResult<Expr> {
    let numeric_op = alt((
        map(tk(Star), |token| NumericOp::Mul(token.span)),
        map(tk(Slash), |token| NumericOp::Div(token.span)),
        map(tk(Percent), |token| NumericOp::Rem(token.span)),
    ));

    fold_many0(
        prec12,
        tuple((numeric_op, prec12)),
        |lhs, (op, rhs)| Expr::NumericOp(Box::new(Binary {lhs, op, rhs})),
    )(input)
}

fn prec12(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec13,
        prefixed(kw(Kw::As), ty),
        |expr, ty| Expr::CastAs(Box::new(CastAs {expr, ty})),
    )(input)
}

fn prec13(input: Input) -> ParseResult<Expr> {
    fold_many0(
        prec14,
        tuple((tk(Caret), prec14)),
        |lhs, (op, rhs)| Expr::NumericOp(Box::new(Binary {lhs, op: NumericOp::Pow(op.span), rhs})),
    )(input)
}

fn prec14(input: Input) -> ParseResult<Expr> {
    let unary_op = alt((
        map(tk(Plus), |token| UnaryOp::Pos(token.span)),
        map(tk(Minus), |token| UnaryOp::Neg(token.span)),
        map(tk(Not), |token| UnaryOp::Not(token.span)),
    ));

    map(
        tuple((opt(unary_op), prec15)),
        |(op, expr)| match op {
            Some(op) => Expr::UnaryOp(Box::new(Unary {op, expr})),
            None => expr,
        },
    )(input)
}

fn prec15(input: Input) -> ParseResult<Expr> {
    enum Postfix {
        FuncCall(Vec<Expr>),
        Index(Expr),
    }

    let postfix_op = spanned(alt((
        map(parens(comma_separated(expr)), Postfix::FuncCall),
        map(brackets(expr), Postfix::Index),
    )));

    fold_many0(
        prec16,
        postfix_op,
        |value, (op, rhs_span)| match op {
            Postfix::FuncCall(args) => {
                let span = value.span().to(rhs_span);
                Expr::Call(Box::new(FuncCall {value, args, span}))
            },
            Postfix::Index(expr) => {
                let span = value.span().to(rhs_span);
                Expr::Index(Box::new(Index {value, expr, span}))
            },
        },
    )(input)
}

fn prec16(input: Input) -> ParseResult<Expr> {
    let postfix_op = tuple((tk(Period), ident, opt(spanned(parens(comma_separated(expr))))));

    fold_many0(
        prec17,
        postfix_op,
        |lhs, op| match op {
            (_, method_name, Some((args, args_span))) => {
                let span = lhs.span().to(args_span);
                Expr::MethodCall(Box::new(MethodCall {lhs, method_name, args, span}))
            },
            (_, field, None) => {
                Expr::FieldAccess(Box::new(FieldAccess {lhs, field}))
            },
        },
    )(input)
}

fn prec17(input: Input) -> ParseResult<Expr> {
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
        map(unit_lit, Expr::UnitLiteral),
        map(kw(Kw::SelfValue), |token| Expr::SelfValue(token.span)),
        map(path, Expr::Path),
    ))(input)
}

fn cond(input: Input) -> ParseResult<Cond> {
    // Start with a single `if`, since if that isn't found we can quit right away
    let (mut input, (if_token, cond, (body, if_body_span))) = tuple((
        kw(Kw::If),
        expr,
        spanned(block),
    ))(input)?;
    let span_start = if_token.span;

    // This is essentially equivalent to:
    // tuple((
    //      many0(tuple((kw(Kw::Else), kw(Kw::If), expr, block))),
    //      opt(tuple((kw(Kw::Else), block))),
    // ))
    // Need to hand-roll this loop because we need 2 tokens to detect `else if` and `many0` will
    // fail if we advance further than 1 token.
    let mut conds = vec![(cond, body)];
    let mut else_body = None;
    let mut span_end = if_body_span;
    loop {
        match opt(kw(Kw::Else))(input)? {
            // Advance in the input past the else
            (inp, Some(_)) => input = inp,
            // If something other than else is found, stop, but do NOT advance the input
            // This ensures that the next token is available for the next parser after this
            _ => break,
        }

        // One of these must succeed or we produce an error
        let (inp, (stop, body_span)) = alt((
            // an `else if` block
            map(tuple((kw(Kw::If), expr, spanned(block))), |(_, cond, (body, body_span))| {
                conds.push((cond, body));
                // keep looping
                (false, body_span)
            }),
            // just an `else` block
            map(spanned(block), |(body, body_span)| {
                else_body = Some(body);
                // stop
                (true, body_span)
            }),
        ))(input)?;
        input = inp;
        span_end = body_span;

        if stop {
            break;
        }
    }

    let span = span_start.to(span_end);
    Ok((input, Cond {conds, else_body, span}))
}

fn struct_lit(input: Input) -> ParseResult<StructLiteral> {
    map(
        spanned(tuple((named_ty, braces(comma_separated(struct_field_value))))),
        |((name, field_values), span)| StructLiteral {name, field_values, span},
    )(input)
}

fn struct_field_value(input: Input) -> ParseResult<StructFieldValue> {
    map(
        tuple((ident, opt(prefixed(tk(Colon), expr)))),
        |(name, value)| StructFieldValue {name, value},
    )(input)
}

fn bstr_lit(input: Input) -> ParseResult<ast::Literal<Arc<[u8]>>> {
    map(lit(BStr), |token| ast::Literal {
        value: token.unwrap_bstr().clone(),
        span: token.span,
    })(input)
}

fn int_lit(input: Input) -> ParseResult<IntegerLiteral> {
    map(lit(Integer), |token| {
        let (value, suffix) = token.unwrap_integer();
        let suffix = suffix.map(|suffix| match suffix {
            Suffix::Int => LiteralSuffix::Int,
            Suffix::Real => LiteralSuffix::Real,
        });
        let span = token.span;
        IntegerLiteral {value, suffix, span}
    })(input)
}

fn real_lit(input: Input) -> ParseResult<ast::Literal<f64>> {
    map(lit(Real), |token| ast::Literal {
        value: token.unwrap_real(),
        span: token.span,
    })(input)
}

fn complex_lit(input: Input) -> ParseResult<ast::Literal<f64>> {
    map(lit(Complex), |token| ast::Literal {
        value: token.unwrap_complex(),
        span: token.span,
    })(input)
}

fn bool_lit(input: Input) -> ParseResult<ast::Literal<bool>> {
    alt((
        map(kw(Kw::True), |token| ast::Literal {value: true, span: token.span}),
        map(kw(Kw::False), |token| ast::Literal {value: false, span: token.span}),
    ))(input)
}

fn unit_lit(input: Input) -> ParseResult<Span> {
    map(
        tuple((tk(OpenDelim(Paren)), tk(CloseDelim(Paren)))),
        |(open_token, close_token)| open_token.span.to(close_token.span),
    )(input)
}

fn ty(input: Input) -> ParseResult<Ty> {
    alt((
        map(
            tuple((tk(OpenDelim(Paren)), tk(CloseDelim(Paren)))),
            |(open_token, close_token)| Ty::Unit(open_token.span.to(close_token.span)),
        ),
        map(named_ty, |ty| ty.into()),
    ))(input)
}

fn named_ty(input: Input) -> ParseResult<NamedTy> {
    alt((
        map(path, NamedTy::Named),
        map(kw(Kw::SelfType), |token| NamedTy::SelfType(token.span)),
    ))(input)
}

fn path(input: Input) -> ParseResult<Path> {
    alt((
        map(
            tuple((
                path_prefix,
                opt(prefixed(
                    tk(DoubleColon),
                    separated0(tk(DoubleColon), ident),
                )),
            )),
            |(prefix, components)| Path {
                prefix: Some(prefix),
                components: components.unwrap_or_default(),
            },
        ),

        map(
            separated1(tk(DoubleColon), ident),
            |components| Path {prefix: None, components},
        ),
    ))(input)
}

fn path_prefix(input: Input) -> ParseResult<PathPrefix> {
    alt((
        map(kw(Kw::Package), |token| PathPrefix::Package(token.span)),
        map(kw(Kw::SelfType), |token| PathPrefix::SelfType(token.span)),
        map(kw(Kw::SelfValue), |token| PathPrefix::SelfValue(token.span)),
        //TODO: Support super::super::super::...
        map(kw(Kw::Super), |token| PathPrefix::Super(token.span)),
    ))(input)
}

fn ident(input: Input) -> ParseResult<ast::Ident> {
    map(
        tk(Ident),
        |token| ast::Ident {
            value: token.unwrap_ident().clone(),
            span: token.span,
        },
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

/// Returns the span surrounding the result of the given parser
fn spanned<'a, F, O>(mut f: F) -> impl FnMut(Input<'a>) -> ParseResult<'a, (O, Span)>
    where F: FnMut(Input<'a>) -> ParseResult<'a, O>,
{
    move |input| {
        let (_, init_token) = input.advance();
        let (input, value) = f(input)?;
        let (_, final_token) = input.advance();
        let span = Span {
            start: init_token.span.start,
            // The final token is one *past* the end token
            end: final_token.span.start,
        };

        Ok((input, (value, span)))
    }
}

fn kw(keyword: token::Keyword) -> impl FnMut(Input) -> ParseResult<&Token> {
    tk(Keyword(keyword))
}

fn lit(kind: LitKind) -> impl FnMut(Input) -> ParseResult<&Token> {
    tk(Literal(kind))
}

fn tk(kind: TokenKind) -> impl FnMut(Input) -> ParseResult<&Token> {
    move |input| {
        let (next_input, token) = input.advance();
        if token.kind == kind {
            // Only proceed with the next input if this succeeds
            Ok((next_input, token))
        } else {
            Err((input, ParseError {
                expected: smallvec![kind],
                actual: token,
            }))
        }
    }
}
