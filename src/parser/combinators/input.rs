use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RelativePosition {
    /// The left input is behind the right input (left has advanced less than right)
    Behind,
    /// The left input is at the same position as the right input
    Same,
    /// The left input is ahead of the right input (left has advanced more than right)
    Ahead,
}

pub trait InputItem {
    /// The type that represents an expected value of this item (for use in errors)
    type Expected: fmt::Debug;
}

impl<'a, T: InputItem> InputItem for &'a T {
    type Expected = <T as InputItem>::Expected;
}

pub trait ParserInput: Clone {
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

    /// Returns true if this input has advanced past the given input
    fn has_advanced_past(&self, other: &Self) -> bool {
        self.relative_position_to(other) == RelativePosition::Ahead
    }
}

impl<'a, T: InputItem> ParserInput for &'a [T] {
    type Item = &'a T;

    fn advance(&self) -> (Self, Self::Item) {
        (&self[1..], &self[0])
    }

    fn relative_position_to(&self, other: &Self) -> RelativePosition {
        let self_ptr = self.as_ptr();
        let other_ptr = other.as_ptr();

        use std::cmp::Ordering::*;
        match self_ptr.cmp(&other_ptr) {
            Less => RelativePosition::Behind,
            Equal => RelativePosition::Same,
            Greater => RelativePosition::Ahead,
        }
    }
}
