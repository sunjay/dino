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
const MAX_EXPECTED: usize = 5;

#[derive(Debug)]
pub struct ParseError<T: InputItem> {
    /// The values expected to be found
    pub expected: SmallVec<[<T as InputItem>::Expected; MAX_EXPECTED]>,
    /// The value that was actually found
    pub actual: T,
}

impl<T: InputItem> ParseError<T> {
    pub fn merge(self, other: Self) -> Self {
        let Self {mut expected, actual} = self;
        let Self {expected: other_expected, actual: other_actual} = other;

        assert!(actual == other_actual,
            "bug: cannot merge errors where `actual` item is different");

        for item in other_expected {
            if !expected.contains(&item) {
                expected.push(item);
            }
        }

        Self {
            expected,
            actual,
        }
    }
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
pub fn many0<I: ParserInput, O, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Vec<O>, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
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

/// Applies the initial parser, then another parser until it fails. The results are accumulated
/// using the given function which takes the result so far, and each result produced.
pub fn fold_many0<I: ParserInput, O, R, F, G, H>(
    mut init: G,
    mut f: F,
    mut folder: H,
) -> impl FnMut(I) -> IResult<I, R, <I as ParserInput>::Item>
where G: FnMut(I) -> IResult<I, R, <I as ParserInput>::Item>,
      F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
      H: FnMut(R, O) -> R,
{
    move |input| {
        let (mut input, mut final_output) = init(input)?;
        loop {
            match f(input.clone()) {
                Ok((inp, output)) => {
                    final_output = folder(final_output, output);
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
        Ok((input, final_output))
    }
}

/// Creates an optional parser. Returns `None` if the given parser produces an error.
pub fn opt<I: ParserInput, O, F>(
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Option<O>, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
{
    move |input| match f(input.clone()) {
        Ok((input, output)) => Ok((input, Some(output))),
        Err(_) => Ok((input, None)),
    }
}

/// Maps a function on the result of the parser
pub fn map<I: ParserInput, O1, O2, F, G>(mut f: F, mut mapper: G) -> impl FnMut(I) -> IResult<I, O2, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O1, <I as ParserInput>::Item>,
          G: FnMut(O1) -> O2,
{
    move |input| f(input).map(|(input, output)| (input, mapper(output)))
}

/// Alternates between two parsers to produce a list
///
/// Allows a trailing separator
pub fn separated0<I: ParserInput, O, O2, F, S>(
    mut sep: S,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Vec<O>, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
          S: FnMut(I) -> IResult<I, O2, <I as ParserInput>::Item>,
{
    move |mut input| {
        let mut items = Vec::new();

        loop {
            match f(input.clone()) {
                Ok((inp, item)) => {
                    items.push(item);
                    if input.relative_position_to(&inp) == RelativePosition::Same {
                        panic!("bug: infinite loop detected. Do not pass a parser that accepts empty input to separated0");
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

            // Match the entire separator or stop
            match opt(&mut sep)(input.clone())? {
                (inp, Some(_)) => input = inp,
                // Ignore the input because we don't want to advance past the last item
                (_, None) => break,
            }
        }

        Ok((input, items))
    }
}

/// Alternates between two parsers to produce a non-empty list
///
/// This does NOT allow a trailing separator
pub fn separated1<I: ParserInput, O, O2, F, S>(
    mut sep: S,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Vec<O>, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
          S: FnMut(I) -> IResult<I, O2, <I as ParserInput>::Item>,
{
    move |mut input| {
        let mut items = Vec::new();

        loop {
            let (inp, item) = f(input.clone())?;
            if input.relative_position_to(&inp) == RelativePosition::Same {
                panic!("bug: infinite loop detected. Do not pass a parser that accepts empty input to separated1");
            }
            input = inp;
            items.push(item);

            // Match the entire separator or stop
            match opt(&mut sep)(input.clone())? {
                (inp, Some(_)) => input = inp,
                // Ignore the input because we don't want to advance past the last item
                (_, None) => break,
            }
        }

        Ok((input, items))
    }
}

/// Executes both parsers and only returns the first
pub fn suffixed<I: ParserInput, O, O2, F, S>(
    f: F,
    suffix: S,
) -> impl FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
          S: FnMut(I) -> IResult<I, O2, <I as ParserInput>::Item>,
{
    map(
        tuple((f, suffix)),
        |(out, _)| out,
    )
}

/// Executes both parsers and only returns the second
pub fn prefixed<I: ParserInput, O, O2, F, P>(
    prefix: P,
    f: F,
) -> impl FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
          P: FnMut(I) -> IResult<I, O2, <I as ParserInput>::Item>,
{
    map(
        tuple((prefix, f)),
        |(_, out)| out,
    )
}

/// Executes all three parsers in order and only returns the middle
pub fn surrounded<I: ParserInput, O, O2, O3, F, P, S>(
    prefix: P,
    f: F,
    suffix: S,
) -> impl FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>
    where F: FnMut(I) -> IResult<I, O, <I as ParserInput>::Item>,
          P: FnMut(I) -> IResult<I, O2, <I as ParserInput>::Item>,
          S: FnMut(I) -> IResult<I, O3, <I as ParserInput>::Item>,
{
    map(
        tuple((prefix, f, suffix)),
        |(_, out, _)| out,
    )
}
