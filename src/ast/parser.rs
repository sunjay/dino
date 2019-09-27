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
        wsc0,
        block,
    )), |(_, _, name, _, _params, _, body)| Function {
        name,
        sig: FuncSig {return_type: Ty::Unit, params: Vec::new()}, //TODO
        body,
        is_extern: false,
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
            preceded(wsc0, many0(terminated(stmt, wsc0))),
            char('}'),
        ),
        |stmts| Block {stmts},
    )(input)
}

fn stmt(input: Input) -> IResult<Stmt> {
    alt((
        map(var_decl, Stmt::VarDecl),
        map(tuple((expr, wsc0, char(';'))), |(expr, _, _)| Stmt::Expr(expr)),
    ))(input)
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
                ident,
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
        map(func_call, Expr::Call),
        // Integer literal must be parsed before real_literal because that parser also accepts all
        // valid integer literals
        map(integer_literal, Expr::IntegerLiteral),
        map(real_literal, Expr::RealLiteral),
        map(ident, Expr::Var),
    ))(input)
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
            // Cannot end in something that would result in a real number literal
            not(one_of(".eE")),
        ))),
        |val: Input| match val.parse_to() {
            Some(n) => Ok(n),
            None => Err(nom::Err::Error(VerboseError::from_error_kind(input, ErrorKind::ParseTo))),
        },
    )(input)
}

fn real_literal(input: Input) -> IResult<f64> {
    double(input)
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
