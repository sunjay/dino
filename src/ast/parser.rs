use snafu::Snafu;
use nom::{
    error::VerboseError,
    branch::alt,
    character::complete::{char, digit1},
    combinator::{all_consuming, map, map_res, recognize, opt},
    bytes::complete::{tag, take_while1, take_while},
    sequence::{tuple, pair, delimited, terminated, preceded},
    multi::{many0, separated_list},
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
        preceded(ws0, many0(terminated(decl, ws0))),
        |decls| Module {decls},
    )(input)
}

fn decl(input: Input) -> IResult<Decl> {
    map(function, Decl::Function)(input)
}

fn function(input: Input) -> IResult<Function> {
    map(tuple((
        tag("fn"),
        ws1,
        ident,
        ws0,
        function_params,
        ws0,
        block,
    )), |(_, _, name, _, _params, _, body)| Function {
        name,
        sig: FuncSig {return_type: Ty::Unit, params: Vec::new()}, //TODO
        body,
    })(input)
}

fn function_params(input: Input) -> IResult<()> {
    //TODO: Actually parse params
    delimited(char('('), |inp| Ok((inp, ())), char(')'))(input)
}

fn block(input: Input) -> IResult<Block> {
    map(
        delimited(
            char('{'),
            preceded(ws0, many0(terminated(stmt, ws0))),
            char('}'),
        ),
        |stmts| Block {stmts},
    )(input)
}

fn stmt(input: Input) -> IResult<Stmt> {
    alt((
        map(var_decl, Stmt::VarDecl),
        map(tuple((expr, ws0, char(';'))), |(expr, _, _)| Stmt::Expr(expr)),
    ))(input)
}

fn var_decl(input: Input) -> IResult<VarDecl> {
    map(
        tuple((
            tag("let"),
            ws1,
            ident,
            ws0,
            char(':'),
            ws0,
            ident,
            ws0,
            char('='),
            ws0,
            expr,
            ws0,
            char(';'),
        )),
        |(_, _, ident, _, _, _, ty, _, _, _, expr, _, _)| VarDecl {ident, ty, expr},
    )(input)
}

fn expr(input: Input) -> IResult<Expr> {
    alt((
        map(func_call, Expr::CallExpr),
        map(integer_literal, Expr::IntegerLiteral),
        map(ident, Expr::Var),
    ))(input)
}

fn func_call(input: Input) -> IResult<CallExpr> {
    map(
        tuple((ident, ws0, func_args)),
        |(func_name, _, args)| CallExpr {func_name, args},
    )(input)
}

fn func_args(input: Input) -> IResult<Vec<Expr>> {
    delimited(char('('), comma_separated(expr), char(')'))(input)
}

fn comma_separated<'r, T: Clone + 'r, F>(parser: F) -> impl Fn(Input<'r>) -> IResult<Vec<T>>
    where F: Fn(Input<'r>) -> IResult<T> {
    separated_list(tuple((ws0, char(','), ws0)), parser)
}

fn ident(input: Input) -> IResult<&str> {
    recognize(pair(
        // Must be at least non-number
        take_while1(|c: char| c.is_alphabetic() || c == '_'),
        // Followed by any other identifier characters
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
}

fn integer_literal(input: Input) -> IResult<i64> {
    use nom::ParseTo;
    use nom::error::{ParseError, ErrorKind};

    map_res(
        recognize(tuple((
            opt(alt((char('+'), char('-')))),
            digit1,
        ))),
        |val: Input| match val.parse_to() {
            Some(n) => Ok(n),
            None => Err(nom::Err::Error(VerboseError::from_error_kind(input, ErrorKind::ParseTo))),
        },
    )(input)
}

/// Parses at least one whitespace character
fn ws1(input: Input) -> IResult<()> {
    //TODO: Support comments
    map(take_while1(|c: char| c.is_whitespace()), |_| ())(input)
}

/// Parses any amount of whitespace
fn ws0(input: Input) -> IResult<()> {
    //TODO: Support comments
    map(take_while(|c: char| c.is_whitespace()), |_| ())(input)
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
