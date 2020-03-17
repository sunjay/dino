use std::collections::VecDeque;

use super::lexer::{Lexer, Token, TokenKind};

/// Abstraction over Lexer to provide access to tokens in a way that is convenient for the parser
pub struct TokenStream {
    tokens: VecDeque<Token>,
}

impl<'a> From<Lexer<'a>> for TokenStream {
    fn from(mut lexer: Lexer<'a>) -> Self {
        // Read all tokens from lexer so that any lexing errors are reported even if the entire
        // token stream isn't consumed
        let mut tokens = VecDeque::new();
        loop {
            let token = lexer.next();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push_back(token);
        }

        Self {tokens}
    }
}

impl TokenStream {
    /// Returns the next token in the input, but does not consume it
    pub fn peek(&mut self) -> &Token {
        self.peek_nth(1)
    }

    /// Returns the next token after the next token in the input, but does not consume it
    pub fn peek2(&mut self) -> &Token {
        self.peek_nth(2)
    }

    /// Returns the next nth token in the input, but does not consume it
    fn peek_nth(&mut self, n: usize) -> &Token {
        debug_assert!(n > 0);

        &self.tokens[n - 1]
    }

    /// Returns the next token in the input
    pub fn next(&mut self) -> Token {
        self.tokens.pop_front()
            .expect("bug: popped past the end of token stream")
    }
}
