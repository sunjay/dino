use snafu::Snafu;
use nom::{
    error::VerboseError,
    character::complete::char,
    combinator::{all_consuming, map, recognize},
    bytes::complete::{tag, take_while1, take_while},
    sequence::{tuple, pair, delimited},
    multi::many0,
};

use crate::ast::*;

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

/// Attempts to parse the given input program
pub fn parse_program(input: &str) -> Result<Program, Error> {
    Ok(program(input).map(|(inp, prog)| match inp {
        "" => prog,
        // Should never reach this case because the parser should ensure that all input is consumed
        _ => unreachable!(),
    })?)
}

fn program(input: Input) -> IResult<Program> {
    let (inp, decls) = all_consuming(many0(delimited(ws0, decl, ws0)))(input)?;

    Ok((inp, Program {decls}))
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
    )), |(_, _, name, _, _params, _, _block)| Function {
        name,
    })(input)
}

fn function_params(input: Input) -> IResult<()> {
    //TODO: Actually parse params
    delimited(char('('), |inp| Ok((inp, ())), char(')'))(input)
}

fn block(input: Input) -> IResult<()> {
    //TODO: Actually parse statements
    delimited(char('{'), |inp| Ok((inp, ())), char('}'))(input)
}

fn ident(input: Input) -> IResult<&str> {
    recognize(pair(
        // Must be at least non-number
        take_while1(|c: char| c.is_alphabetic() || c == '_'),
        // Followed by any other identifier characters
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    ))(input)
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
