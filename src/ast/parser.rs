use std::iter::once;

use snafu::Snafu;
use nom::{
    error::VerboseError,
    branch::alt,
    number::complete::double,
    character::complete::{char, digit1, one_of},
    combinator::{all_consuming, map, map_res, recognize, opt, not},
    bytes::complete::{tag, take_while1, take_while, take_till},
    sequence::{tuple, pair, delimited, terminated, preceded},
    multi::{many0, many1, separated_list},
};

use super::*;

type Input<'a> = &'a str;
type IResult<'a, O> = nom::IResult<Input<'a>, O, VerboseError<Input<'a>>>;

/// Represents errors that can occur during parsing
#[derive(Debug, Snafu)]
pub enum Error {
    //TODO: Figure out what different error cases we can return here
}

impl From<nom::Err<VerboseError<Input<'_>>>> for Error {
    fn from(err: nom::Err<VerboseError<Input>>) -> Self {
        panic!("Error: {:?}", err);
    }
}

/// Attempts to parse the given input module
pub fn parse_module(input: &str) -> Result<Module, Error> {
    let (inp, module) = match all_consuming(module)(input) {
        Ok((inp, module)) => (inp, module),
        Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => {
            //TODO: Convert to Error type above
            panic!("Error: {}", nom::error::convert_error(input, err));
        },
        // This should not be reachable because we are using the 'complete' versions of all parsers
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    };

    Ok(match inp {
        "" => module,
        // Should never reach this case because the parser should ensure that all input is consumed
        _ => unreachable!(),
    })
}

fn module(input: Input) -> IResult<Module> {
    map(
        preceded(wsc0, many0(terminated(decl, wsc0))),
        |decls| Module {decls},
    )(input)
}

fn decl(input: Input) -> IResult<Decl> {
    map(function, Decl::Function)(input)
}

fn function(input: Input) -> IResult<Function> {
    map(tuple((
        tag("fn"),
        wsc1,
        ident,
        wsc0,
        function_params,
        opt(tuple((wsc0, tag("->"), wsc0, ty))),
        wsc0,
        block,
    )), |(_, _, name, _, params, return_ty, _, body)| Function {
        name,
        sig: FuncSig {
            // The default return type is unit
            return_type: return_ty.map(|(_, _, _, ty)| ty).unwrap_or(Ty::Unit),
            params,
        },
        body,
        is_extern: false,
    })(input)
}

fn function_params(input: Input) -> IResult<Vec<FuncParam>> {
    delimited(
        char('('),
        comma_separated(map(
            tuple((ident, wsc0, char(':'), wsc0, ty)),
            |(name, _, _, _, ty)| FuncParam {name, ty},
        )),
        char(')'),
    )(input)
}

fn block(input: Input) -> IResult<Block> {
    map(
        delimited(
            char('{'),
            tuple((
                preceded(wsc0, many0(terminated(stmt, wsc0))),
                opt(tuple((expr, wsc0))),
            )),
            char('}'),
        ),
        |(mut stmts, expr)| {
            let ret = expr.map(|(expr, _)| expr);

            // There is an ambiguity here because certain expressions can also be written in
            // statment position. When that is the case, we need to be sure to pull those
            // statements into the return expression instead of leaving them in the statements
            // list.
            let ret = if ret.is_none() && !stmts.is_empty() {
                match stmts.pop().unwrap() {
                    // Since semi-colons after conditionals are optional even in statement
                    // position, they may sometimes be seen as statements when the user actually
                    // intended them to be the return expression
                    Stmt::Cond(cond) => Some(Expr::Cond(Box::new(cond))),
                    // Cannot currently promote a while loop to an expression
                    stmt@Stmt::WhileLoop(_) |
                    // Cannot promote var declaration to expression
                    stmt@Stmt::VarDecl(_) |
                    // Expressions cannot be promoted because for them to be parsed as statements
                    // they must have ended in a semi-colon. That means that the user explicitly
                    // indended the expression's value to *not* be returned.
                    stmt@Stmt::Expr(_) => {
                        // Need to push statement back. Likely won't cause any re-allocation, since
                        // the collection shouldn't have freed the space we just pop'd this from.
                        // At most we pay a bit for a move, but that's not too big of a price to
                        // pay for simpler code.
                        stmts.push(stmt);
                        None
                    },
                }
            } else {
                ret
            };

            Block {stmts, ret}
        },
    )(input)
}

fn stmt(input: Input) -> IResult<Stmt> {
    alt((
        // Need to explicitly ensure that there is so semi-colon following this since that means it
        // was intended as an expression, not a statement
        map(tuple((cond, not(char(';')))), |(cond, _)| Stmt::Cond(cond)),
        map(while_loop, Stmt::WhileLoop),
        map(var_decl, Stmt::VarDecl),
        map(tuple((expr, wsc0, char(';'))), |(expr, _, _)| Stmt::Expr(expr)),
    ))(input)
}

fn while_loop(input: Input) -> IResult<WhileLoop> {
    map(
        tuple((tag("while"), wsc1, expr, wsc0, block)),
        |(_, _, cond, _, body)| WhileLoop {cond, body},
    )(input)
}

fn var_decl(input: Input) -> IResult<VarDecl> {
    map(
        tuple((
            tag("let"),
            wsc1,
            ident,
            wsc0,
            opt(tuple((
                char(':'),
                wsc0,
                ty,
                wsc0,
            ))),
            char('='),
            wsc0,
            expr,
            wsc0,
            char(';'),
        )),
        |(_, _, ident, _, ty, _, _, expr, _, _)| VarDecl {
            ident,
            ty: ty.map(|(_, _, ty, _)| ty),
            expr,
        },
    )(input)
}

fn expr(input: Input) -> IResult<Expr> {
    alt((
        map(cond, |cond| Expr::Cond(Box::new(cond))),
        map(func_call, Expr::Call),
        // Integer literal must be parsed before real_literal because that parser also accepts all
        // valid integer literals
        map(integer_literal, Expr::IntegerLiteral),
        real_or_complex_literal,
        map(bool_literal, Expr::BoolLiteral),
        map(unit_literal, |_| Expr::UnitLiteral),
        map(ident, Expr::Var),
    ))(input)
}

fn cond(input: Input) -> IResult<Cond> {
    map(
        tuple((
            tuple((tag("if"), wsc1, expr, wsc0, block)),
            many0(tuple((wsc0, tag("else"), wsc1, tag("if"), wsc1, expr, wsc0, block))),
            opt(tuple((wsc0, tag("else"), wsc0, block))),
        )),
        |(top_if, else_ifs, else_body)| {
            let (_, _, top_if_cond, _, top_if_body) = top_if;
            let else_ifs = else_ifs.into_iter()
                .map(|(_, _, _, _, _, else_if_cond, _, else_if_body)| (else_if_cond, else_if_body));
            let else_body = else_body.map(|(_, _, _, else_body)| else_body);

            let conds = once((top_if_cond, top_if_body)).chain(else_ifs).collect();
            Cond {conds, else_body}
        },
    )(input)
}

fn func_call(input: Input) -> IResult<CallExpr> {
    map(
        tuple((ident, wsc0, func_args)),
        |(func_name, _, args)| CallExpr {func_name, args},
    )(input)
}

fn func_args(input: Input) -> IResult<Vec<Expr>> {
    delimited(char('('), comma_separated(expr), char(')'))(input)
}

fn comma_separated<'r, T: Clone + 'r, F>(parser: F) -> impl Fn(Input<'r>) -> IResult<Vec<T>>
    where F: Fn(Input<'r>) -> IResult<T> {
    separated_list(tuple((wsc0, char(','), wsc0)), parser)
}

fn ty(input: Input) -> IResult<Ty> {
    alt((
        map(tag("()"), |_| Ty::Unit),
        map(ident, |name| Ty::Named(name)),
    ))(input)
}

fn ident(input: Input) -> IResult<&str> {
    recognize(pair(
        // Must be at least non-number
        take_while1(|c: char| c.is_alphabetic() || c == '_'),
        // Followed by any other identifier characters
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

fn integer_literal(input: Input) -> IResult<IntegerLiteral> {
    use nom::ParseTo;
    use nom::error::{ParseError, ErrorKind};

    map_res(
        tuple((
            recognize(tuple((
                opt(alt((char('+'), char('-')))),
                digit1,
                // Cannot end in something that would result in a real number literal
                not(one_of(".eEjJI")),
                // 'i' is a special case because 'i' isn't allowed, but "int" is fine
                //
                // This reads as "you're not allowed i when it isn't followed by 'nt'"
                not(pair(char('i'), not(tag("nt")))),
            ))),
            opt(alt((tag("int"), tag("real")))),
        )),
        |(val, type_hint): (Input, _)| match val.parse_to() {
            Some(value) => Ok(IntegerLiteral {value, type_hint}),
            None => Err(nom::Err::Error(VerboseError::from_error_kind(input, ErrorKind::ParseTo))),
        },
    )(input)
}

fn real_or_complex_literal(input: Input) -> IResult<Expr> {
    map(
        tuple((double, opt(alt((char('j'), char('J'), char('i'), char('I')))))),
        |(value, complex)| if complex.is_some() {
            Expr::ComplexLiteral(value)
        } else {
            Expr::RealLiteral(value)
        }
    )(input)
}

fn bool_literal(input: Input) -> IResult<bool> {
    alt((
        map(tag("true"), |_| true),
        map(tag("false"), |_| false),
    ))(input)
}

fn unit_literal(input: Input) -> IResult<()> {
    map(tag("()"), |_| ())(input)
}

/// Parses at least one whitespace character or comment
fn wsc1(input: Input) -> IResult<()> {
    map(many1(alt((comment, ws::ws1))), |_| ())(input)
}

/// Parses any amount of whitespace or comment
fn wsc0(input: Input) -> IResult<()> {
    map(many0(alt((comment, ws::ws1))), |_| ())(input)
}

/// Parses a comment
fn comment(input: Input) -> IResult<()> {
    map(
        tuple((tag("//"), take_till(|c| c == '\n'), char('\n'))),
        |_| (),
    )(input)
}

// Keeping in a separate module so it's easier to use wsc0 and wsc1 instead of these
mod ws {
    use super::*;

    /// Parses at least one whitespace character
    pub fn ws1(input: Input) -> IResult<()> {
        map(take_while1(|c: char| c.is_whitespace()), |_| ())(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::convert_error;

    macro_rules! test_parser {
        ($parser:ident($input:expr) -> ok) => {
            {
                let input = $input;
                match $parser(input) {
                    Ok((inp, _)) => if !inp.is_empty() {
                        panic!("Parser did not consume entire input")
                    },
                    Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => {
                        panic!("Parse error: {}", convert_error(input, err));
                    },
                    Err(nom::Err::Incomplete(_)) => unreachable!("Parser returned incomplete"),
                }
            }
        };
        ($parser:ident($input:expr) -> err) => {
            {
                let input = $input;
                match $parser(input) {
                    Ok((inp, val)) => panic!("Parser should not have succeeded.\n\tRemaining Input: `{}`\n\tValue: `{:?}`", inp, val),
                    Err(_) => {},
                }
            }
        };
    }

    #[test]
    fn function_parser() {
        // Valid cases with different whitespace
        test_parser!(function("fn foo() {}") -> ok);
        test_parser!(function("fn foo(){}") -> ok);
        test_parser!(function("fn foo (){}") -> ok);
        test_parser!(function("fn
            foo () {}") -> ok);

        // No space between `fn` and `foo`
        test_parser!(function("fnfoo(){}") -> err);
    }
}
