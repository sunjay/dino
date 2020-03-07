use std::collections::VecDeque;

use super::lexer::{Lexer, Token};

/// Abstraction over Lexer to provide access to tokens in a way that is convenient for the parser
pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    /// The next queued tokens, used to peek ahead at what's next
    look_ahead: VecDeque<Token<'a>>,
}

impl<'a> From<Lexer<'a>> for TokenStream<'a> {
    fn from(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            // capacity = 2 since that's the max supported lookahead
            look_ahead: VecDeque::with_capacity(2),
        }
    }
}

impl<'a> TokenStream<'a> {
    /// Returns the next token in the input, but does not consume it
    pub fn peek(&mut self) -> &Token<'a> {
        self.peek_nth(1)
    }

    /// Returns the next token after the next token in the input, but does not consume it
    pub fn peek2(&mut self) -> &Token<'a> {
        self.peek_nth(2)
    }

    /// Returns the next nth token in the input, but does not consume it
    ///
    /// Not public until we decide whether we want arbitrary lookahead
    fn peek_nth(&mut self, n: usize) -> &Token<'a> {
        debug_assert!(n > 0);

        // Read just enough to be able to get this token
        while self.look_ahead.len() < n {
            let next_token = self.lexer.next();
            self.look_ahead.push_back(next_token);
        }

        // This unwrap is safe because we just checked if there are enough next tokens
        self.look_ahead.get(n - 1).unwrap()
    }

    /// Returns the next token in the input
    pub fn next(&mut self) -> Token<'a> {
        // Return the next queued token, if any
        if let Some(token) = self.look_ahead.pop_front() {
            return token;
        }

        self.lexer.next()
    }
}
