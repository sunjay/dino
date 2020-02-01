/// A token and the span of the data that was matched
#[derive(Debug)]
pub struct TokenSlice<'a> {
    pub token: Token,
    pub slice: &'a [u8],
}

#[derive(Debug)]
pub enum Token {
    Keyword(Keyword),
}

#[derive(Debug)]
pub enum Keyword {
    Fn,
    Let,
}

pub fn token(text: &[u8]) -> Option<Result<TokenSlice, LexerError>> {
    let first = text.get(0)?;
    let rest = &text[1..];
    Ok(match first {
        b'0' ..= b'9' => number(text),

    })
}

fn number(text: &[u8]) -> Result<TokenSlice, LexerError> {

}
