use super::{ParserInput, IResult};

pub trait Tuple<I: ParserInput, O> {
    fn parse(&mut self, input: I) -> IResult<I, O>;
}

pub fn tuple<I: ParserInput, O, P: Tuple<I, O>>(mut parsers: P) -> impl FnMut(I) -> IResult<I, O> {
    move |input| parsers.parse(input)
}

macro_rules! impl_tuple {
    ($f_1:ident => $o_1:ident, $($f_n:ident => $o_n:ident,)*) => {
        impl_tuple!($($f_n => $o_n,)*);

        impl<I: ParserInput, $f_1, $o_1, $($f_n, $o_n),*> Tuple<I, ($o_1, $($o_n),*)> for ($f_1, $($f_n),*)
            where $f_1: FnMut(I) -> IResult<I, $o_1>,
                  $($f_n: FnMut(I) -> IResult<I, $o_n>),*
        {
            fn parse(&mut self, input: I) -> IResult<I, ($o_1, $($o_n),*)> {
                #![allow(non_snake_case)] // makes code generation easier

                let ($f_1, $($f_n),*) = self;
                let (input, $o_1) = $f_1(input)?;
                $(let (input, $o_n) = $f_n(input)?;)*

                Ok((input, ($o_1, $($o_n),*)))
            }
        }
    };
    () => {};
}

impl_tuple! {
    F1 => O1,
    F2 => O2,
    F3 => O3,
    F4 => O4,
    F5 => O5,
    F6 => O6,
    F7 => O7,
    F8 => O8,
    F9 => O9,
    F10 => O10,
    F11 => O11,
    F12 => O12,
    F13 => O13,
    F14 => O14,
    F15 => O15,
    F16 => O16,
    F17 => O17,
    F18 => O18,
    F19 => O19,
    F20 => O20,
    F21 => O21,
    F22 => O22,
    F23 => O23,
    F24 => O24,
    F25 => O25,
    F26 => O26,
    F27 => O27,
    F28 => O28,
    F29 => O29,
    F30 => O30,
    F31 => O31,
    F32 => O32,
    F33 => O33,
    F34 => O34,
    F35 => O35,
    F36 => O36,
    F37 => O37,
    F38 => O38,
    F39 => O39,
    F40 => O40,
    F41 => O41,
    F42 => O42,
    F43 => O43,
    F44 => O44,
    F45 => O45,
    F46 => O46,
    F47 => O47,
    F48 => O48,
    F49 => O49,
    F50 => O50,
    F51 => O51,
    F52 => O52,
    F53 => O53,
    F54 => O54,
    F55 => O55,
    F56 => O56,
    F57 => O57,
    F58 => O58,
    F59 => O59,
    F60 => O60,
    F61 => O61,
    F62 => O62,
    F63 => O63,
    F64 => O64,
}
