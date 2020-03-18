mod alt;
mod tuple;

pub use alt::*;
pub use tuple::*;

use std::fmt;

use smallvec::SmallVec;

/// An approximation of the max number of expected values in an error
///
/// Should be set based on the expected max number of failed branches in an `alt`
const MAX_EXPECTED: usize = 3;

pub trait InputItem {
    /// The type that represents an expected value of this item (for use in errors)
    type Expected: fmt::Debug;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RelativePosition {
    /// The left input is behind the right input (left has advanced less than right)
    Behind,
    /// The left input is at the same position as the right input
    Same,
    /// The left input is ahead of the right input (left has advanced more than right)
    Ahead,
}

pub trait Input: Clone {
    type Item: InputItem;

    /// Advances the input and returns the next item
    ///
    /// If no remaining items are present, this method should panic, as that is considered a bug.
    /// Inputs should have a natural end item (e.g. EOF) that signals the end of input.
    fn advance(&self) -> (Self, Self::Item);

    /// Compares the input position of this input with another input
    ///
    /// Note: for most inputs, it doesn't make sense to ask for the current position, since that
    /// can be meaningless for inputs where each item has its own concept of position. However,
    /// it can make sense to compare how far different inputs have advanced. That is the
    /// information that this method provides.
    fn relative_position_to(&self, other: &Self) -> RelativePosition;
}

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
pub type IResult<I, O> = Result<(I, O), (I, ParseError<<I as Input>::Item>)>;

pub fn many0<I: Input, O, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>>
    where F: FnMut(I) -> IResult<I, O>
{
    move |mut input| {
        let mut outputs = Vec::new();
        while let Ok((inp, output)) = f(input.clone()) {
            outputs.push(output);
            if input.relative_position_to(&inp) == RelativePosition::Same {
                panic!("bug: infinite loop detected. Do not pass a parser that accepts empty input to many0");
            }
            input = inp;
        }
        Ok((input, outputs))
    }
}

pub fn many1<I: Input, O, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>>
    where F: FnMut(I) -> IResult<I, O>
{
    move |input| {
        let (mut input, output) = f(input)?;
        let mut outputs = vec![output];
        while let Ok((inp, output)) = f(input.clone()) {
            outputs.push(output);
            if input.relative_position_to(&inp) == RelativePosition::Same {
                panic!("bug: infinite loop detected. Do not pass a parser that accepts empty input to many1");
            }
            input = inp;
        }
        Ok((input, outputs))
    }
}

pub fn opt<I: Input, O, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Option<O>>
    where F: FnMut(I) -> IResult<I, O>
{
    move |input| match f(input) {
        Ok((input, output)) => Ok((input, Some(output))),
        Err((input, _)) => Ok((input, None)),
    }
}

pub fn map<I: Input, O1, O2, F, G>(mut f: F, mut mapper: G) -> impl FnMut(I) -> IResult<I, O2>
    where F: FnMut(I) -> IResult<I, O1>,
          G: FnMut(O1) -> O2,
{
    move |input| f(input).map(|(input, output)| (input, mapper(output)))
}
