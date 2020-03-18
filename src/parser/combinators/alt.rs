use smallvec::SmallVec;

use super::{ParserInput, IResult, ParseError};

pub trait Alt<I: ParserInput, O> {
    fn parse(&mut self, input: I) -> IResult<I, O, <I as ParserInput>::Item>;
}

pub fn alt<I: ParserInput, O, P: Alt<I, O>>(mut parsers: P) -> impl FnMut(I) -> IResult<I, O, <I as ParserInput>::Item> {
    move |input| parsers.parse(input)
}

macro_rules! impl_alt {
    ($f_1:ident, $($f_n:ident,)*) => {
        impl_alt!($($f_n,)*);

        impl<I: ParserInput, O, $f_1, $($f_n),*> Alt<I, O> for ($f_1, $($f_n),*)
            where $f_1: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
                  $($f_n: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>),*
        {
            fn parse(&mut self, input: I) -> IResult<I, O, <I as ParserInput>::Item> {
                #![allow(non_snake_case)] // makes code generation easier

                use super::RelativePosition::*;

                // Algorithm is roughly:
                // 1. run each parser until one succeeds
                // 2. If one succeeds, return the first success
                // 3. If all fail, compute how far each got in the input
                // 4. Merge the errors of the ones that got the furthest (actual token of all errors should be same because their input position is the same)
                // 5. Return the merged error

                let ($f_1, $($f_n),*) = self;

                let mut error_input = input.clone();
                let mut total_error = ParseError {
                    expected: SmallVec::default(),
                    actual: input.advance().1,
                };

                match $f_1(input.clone()) {
                    Ok(value) => return Ok(value),
                    Err((input, err)) => match input.relative_position_to(&error_input) {
                        Behind => {},
                        Same => total_error.expected.extend(err.expected),
                        Ahead => {
                            error_input = input;
                            total_error = err
                        },
                    },
                }
                $(match $f_n(input.clone()) {
                    Ok(value) => return Ok(value),
                    Err((input, err)) => match input.relative_position_to(&error_input) {
                        Behind => {},
                        Same => total_error.expected.extend(err.expected),
                        Ahead => {
                            error_input = input;
                            total_error = err
                        },
                    },
                })*

                Err((error_input, total_error))
            }
        }
    };
    () => {};
}

impl_alt! {
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    F25,
    F26,
    F27,
    F28,
    F29,
    F30,
    F31,
    F32,
    F33,
    F34,
    F35,
    F36,
    F37,
    F38,
    F39,
    F40,
    F41,
    F42,
    F43,
    F44,
    F45,
    F46,
    F47,
    F48,
    F49,
    F50,
    F51,
    F52,
    F53,
    F54,
    F55,
    F56,
    F57,
    F58,
    F59,
    F60,
    F61,
    F62,
    F63,
    F64,
}
