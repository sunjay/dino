mod input;
mod alt;
mod tuple;

pub use input::*;
pub use alt::*;
pub use tuple::*;

use smallvec::SmallVec;

/// An approximation of the max number of expected values in an error
///
/// Should be set based on the expected max number of failed branches in an `alt`
const MAX_EXPECTED: usize = 3;

#[derive(Debug)]
pub struct ParseError<T: InputItem> {
    /// The values expected to be found
    pub expected: SmallVec<[<T as InputItem>::Expected; MAX_EXPECTED]>,
    /// The value that was actually found
    pub actual: T,
}

/// On success, this represents the output and next input position after the output
///
/// On error, this represents what was expected and the actual item found, as well
/// as the input position of the actual item found
pub type IResult<I, O, Item> = Result<(I, O), (I, ParseError<Item>)>;

/// Repeats the given parser until it fails and returns the results in a Vec
///
/// If the given parser produces an error after advancing past the start of its input, that error
/// will be returned.
pub fn many0<I: ParserInput, O, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>
{
    move |mut input| {
        let mut outputs = Vec::new();
        loop {
            match f(input.clone()) {
                Ok((inp, output)) => {
                    outputs.push(output);
                    if input.relative_position_to(&inp) == RelativePosition::Same {
                        panic!("bug: infinite loop detected. Do not pass a parser that accepts empty input to many0");
                    }
                    input = inp;
                },

                Err((inp, err)) => {
                    // propagate the error if the input advanced past the start
                    if inp.has_advanced_past(&input) {
                        return Err((inp, err));
                    }
                    break;
                }
            }
        }
        Ok((input, outputs))
    }
}

/// Repeats the given parser until it fails and returns the results in a Vec. The parser must
/// succeed at least once. If it fails the first time, the error from that failure will be returned.
///
/// If the given parser produces an error after advancing past the start of its input, that error
/// will be returned.
pub fn many1<I: ParserInput, O, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>
{
    move |input| {
        let (mut input, output) = f(input)?;
        let mut outputs = vec![output];
        loop {
            match f(input.clone()) {
                Ok((inp, output)) => {
                    outputs.push(output);
                    if input.relative_position_to(&inp) == RelativePosition::Same {
                        panic!("bug: infinite loop detected. Do not pass a parser that accepts empty input to many1");
                    }
                    input = inp;
                },

                Err((inp, err)) => {
                    // propagate the error if the input advanced past the start
                    if inp.has_advanced_past(&input) {
                        return Err((inp, err));
                    }
                    break;
                }
            }
        }
        Ok((input, outputs))
    }
}

/// Creates an optional parser. Returns `None` if the given parser produces an error.
pub fn opt<I: ParserInput, O, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Option<O>, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>
{
    move |input| match f(input) {
        Ok((input, output)) => Ok((input, Some(output))),
        Err((input, _)) => Ok((input, None)),
    }
}

/// Maps a function on the result of the parser
pub fn map<I: ParserInput, O1, O2, F, G>(mut f: F, mut mapper: G) -> impl FnMut(I) -> IResult<I, O2, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O1, <I as ParserInput>::Item>,
          G: FnMut(O1) -> O2,
{
    move |input| f(input).map(|(input, output)| (input, mapper(output)))
}
