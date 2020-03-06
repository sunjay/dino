use super::scanner::Scanner;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delim {
    /// A round parenthesis (i.e., `(` or `)`).
    Paren,
    /// A square bracket (i.e., `[` or `]`).
    Bracket,
    /// A curly brace (i.e., `{` or `}`).
    Brace,
}

/// Some literals may end with a suffix that is meant as hint for type inference
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Suffix {
    /// The suffix `int`
    Int,
    /// The suffix `real`
    Real,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    /// An integer literal, e.g. `1`, `31`, `49928`
    Integer(i64, Option<Suffix>),
    /// A real number literal, e.g. `1.0`, `2.5`, `0.5`
    Real(f64, Option<Suffix>),
    /// A complex number literal, e.g. `1i`, `2.5j`, `.5J`
    Complex(f64),
    /// A byte string literal (e.g. `b"abc"`)
    ///
    /// Any escaped characters will be unescaped
    BStr {
        unescaped_text: Vec<u8>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    /// An identifier
    ///
    /// Keywords are just identifiers at this point
    Ident,
    /// A literal of some kind
    Literal(Lit),

    /// An opening delimiter (e.g., `{`).
    OpenDelim(Delim),
    /// A closing delimiter (e.g., `}`).
    CloseDelim(Delim),

    /// The `.` symbol
    Period,
    /// The `,` symbol
    Comma,
    /// The `;` symbol
    Semicolon,
    /// The `->` symbol
    RArrow,
    /// The `::` symbol
    DoubleColon,
    /// The `:` symbol
    Colon,

    /// The `=` symbol
    Equals,

    /// The `<` symbol
    LessThan,
    /// The `<=` symbol
    LessThanEquals,
    /// The `==` symbol
    DoubleEquals,
    /// The `!=` symbol
    NotEqual,
    /// The `>=` symbol
    GreaterThanEquals,
    /// The `>` symbol
    GreaterThan,

    /// The `&&` symbol
    DoubleAnd,
    /// The `||` symbol
    DoubleOr,
    /// The `!` symbol
    Not,

    /// The `&` symbol
    And,
    /// The `|` symbol
    Or,
    /// The `+` symbol
    Plus,
    /// The `-` symbol
    Minus,
    /// The `*` symbol
    Star,
    /// The `/` symbol
    Slash,
    /// The `%` symbol
    Percent,
    /// The `^` symbol
    Caret,
    /// The `<<` symbol
    Shl,
    /// The `>>` symbol
    Shr,

    /// A totally unknown token that should be ignored
    Unknown,

    /// End of file/input
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub span: &'a [u8],
}

#[derive(Debug)]
pub struct Lexer<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        Self {
            scanner,
        }
    }

    /// Returns the next token in the input
    pub fn next(&mut self) -> Token {
        use TokenKind::*;
        use Delim::*;

        self.ignore_whitespace_comments();

        let start = self.scanner.current_pos();
        let next = match self.scanner.next() {
            Some(next) => next,
            None => return self.empty_token(start, Eof),
        };

        // Allows the compiler to help us a bit here
        #[deny(unreachable_patterns)]
        match (next, self.scanner.peek()) {
            (b'(', _) => self.byte_token(start, OpenDelim(Paren)),
            (b')', _) => self.byte_token(start, CloseDelim(Paren)),
            (b'[', _) => self.byte_token(start, OpenDelim(Bracket)),
            (b']', _) => self.byte_token(start, CloseDelim(Bracket)),
            (b'{', _) => self.byte_token(start, OpenDelim(Brace)),
            (b'}', _) => self.byte_token(start, CloseDelim(Brace)),

            (b',', _) => self.byte_token(start, Comma),
            (b';', _) => self.byte_token(start, Semicolon),

            (b'-', Some(b'>')) => self.next_token(start, RArrow),

            (b':', Some(b':')) => self.next_token(start, DoubleColon),
            (b':', _) => self.byte_token(start, Colon),

            (b'=', Some(b'=')) => self.next_token(start, DoubleEquals),
            (b'=', _) => self.byte_token(start, Equals),
            (b'!', Some(b'=')) => self.next_token(start, NotEqual),
            (b'!', _) => self.byte_token(start, Not),

            (b'<', Some(b'=')) => self.next_token(start, LessThanEquals),
            (b'<', Some(b'<')) => self.next_token(start, Shl),
            (b'<', _) => self.byte_token(start, LessThan),
            (b'>', Some(b'=')) => self.next_token(start, GreaterThanEquals),
            (b'>', Some(b'>')) => self.next_token(start, Shr),
            (b'>', _) => self.byte_token(start, GreaterThan),

            (b'&', Some(b'&')) => self.next_token(start, DoubleAnd),
            (b'&', _) => self.byte_token(start, And),
            (b'|', Some(b'|')) => self.next_token(start, DoubleOr),
            (b'|', _) => self.byte_token(start, Or),

            (b'+', _) => self.byte_token(start, Plus),
            (b'-', _) => self.byte_token(start, Minus),
            (b'*', _) => self.byte_token(start, Star),
            // Note that we don't need to check for `//` or `/*` because comments have already been
            // ignored
            (b'/', _) => self.byte_token(start, Slash),
            (b'%', _) => self.byte_token(start, Percent),
            (b'^', _) => self.byte_token(start, Caret),

            (b'b', Some(b'"')) => {
                // Skip the quote
                self.scanner.next();
                self.bstr_lit(start)
            },

            (ch@b'0' ..= b'9', _) |
            (ch@b'.', Some(b'0' ..= b'9')) => self.num_lit(start, ch),
            (b'.', _) => self.byte_token(start, Period),

            (b'a' ..= b'z', _) |
            (b'A' ..= b'Z', _) |
            (b'_', _) => self.ident(start),

            //TODO: Produce an error: "unknown start of token"
            (ch, ch2) => todo!("{} {:?}", ch as char, ch2),
        }
    }

    fn ignore_whitespace_comments(&mut self) {
        while self.ignore_whitespace() || self.ignore_comments() {
            // Keep going until nothing is ignored anymore
        }
    }

    /// Returns true if any whitespace was ignored
    fn ignore_whitespace(&mut self) -> bool {
        let mut ignored = false;
        while let Some(ch) = self.scanner.peek() {
            if ch.is_ascii_whitespace() {
                self.scanner.next();
                ignored = true;
            } else {
                break;
            }
        }

        ignored
    }

    /// Returns true if any comments were ignored
    fn ignore_comments(&mut self) -> bool {
        let mut ignored = false;

        while let Some(ch) = self.scanner.peek() {
            match (ch, self.scanner.peek2()) {
                (b'/', Some(b'*')) => {
                    self.scanner.next2();
                    self.ignore_block_comment();
                },
                (b'/', Some(b'/')) => {
                    self.scanner.next2();
                    self.ignore_until_eol();
                },
                // Keep going until nothing is ignored anymore
                _ => break,
            }
            ignored = true;
        }

        ignored
    }

    /// Assuming the start of a block comment has been seen, ignores until the end is found
    ///
    /// Block comments may be nested, e.g. /* /* foo */ bar */
    fn ignore_block_comment(&mut self) {
        // The count of unmatched `/*` seen
        let mut count = 1;
        while count > 0 {
            match (self.scanner.next(), self.scanner.next()) {
                (Some(b'/'), Some(b'*')) => count += 1,
                (Some(b'*'), Some(b'/')) => count -= 1,
                // If we reach EOF, stop iterating
                (None, _) | (_, None) => break,
                // Ignore all characters between the comment delimiters
                _ => {},
            }
        }

        //TODO: Produce an error: "unterminated block comment"
        if count > 0 {
            todo!()
        }
    }

    /// Ignores until the end of the line
    fn ignore_until_eol(&mut self) {
        while let Some(ch) = self.scanner.next() {
            if ch == b'\n' {
                break;
            }
        }
    }

    // Parses the remaining byte string literal after `b"`
    fn bstr_lit(&mut self, start: usize) -> Token {
        let mut unescaped_text = Vec::new();
        while let Some(ch) = self.scanner.next() {
            if ch == b'"' {
                break;

            } else if ch == b'\\' {
                let unescaped_char = match self.scanner.next() {
                    Some(b'\\') => b'\\',
                    Some(b'"') => b'\"',
                    Some(b'\'') => b'\'',
                    Some(b'n') => b'\n',
                    Some(b'r') => b'\r',
                    Some(b't') => b'\t',
                    Some(b'0') => b'\0',
                    //TODO: Support more kinds of escapes:
                    // ASCII/Byte strings: https://doc.rust-lang.org/reference/tokens.html#ascii-escapes
                    // Unicode strings: https://doc.rust-lang.org/reference/tokens.html#unicode-escapes
                    //TODO: Produce an error: "unknown character escape: `{}`"
                    Some(_) => todo!(),
                    //TODO: Produce an error: "unterminated double quote byte string"
                    None => todo!(),
                };
                unescaped_text.push(unescaped_char);

            } else {
                unescaped_text.push(ch);
            }
        }

        self.token_to_current(start, TokenKind::Literal(Lit::BStr {unescaped_text}))
    }

    /// Parses a numeric literal, given the starting character which must be either a digit or '.'
    ///
    /// If the literal is immediately followed by an ident, we will attempt to parse that as a
    /// literal suffix (e.g. `123int` or `3.4j`).
    fn num_lit(&mut self, start: usize, start_ch: u8) -> Token {
        // true if this is a real number literal
        let mut real = false;
        // Starting at either a digit or '.', so try to get as many additional digits as possible
        let found_digits = self.digits();
        if start_ch == b'.' {
            if found_digits == 0 {
                //TODO: Produce an error: "expected at least one digit in real number literal"
                todo!()
            }

            real = true;
        } else if self.scanner.peek() == Some(b'.') {
            // Check for digits after the '.'
            self.scanner.next();
            // Zero digits are allowed after '.'
            self.digits();
            real = true;
        }

        // Check for exponent part (optional)
        match self.scanner.peek() {
            Some(b'e') | Some(b'E') => {
                self.scanner.next();
                // Check for a sign, `+` or `-`
                match self.scanner.peek() {
                    Some(b'+') | Some(b'-') => {
                        self.scanner.next();
                    },
                    _ => {},
                }
                // Find remaining digits
                let found_digits = self.digits();
                if found_digits == 0 {
                    //TODO: Produce an error: "expected at least one digit in exponent"
                    todo!()
                }

                real = true;
            },
            _ => {},
        }

        // One past the end index of the literal
        let lit_end = self.scanner.current_pos();

        // Check for suffix
        let suffix = match self.scanner.peek() {
            Some(b'a' ..= b'z') |
            Some(b'A' ..= b'Z') |
            Some(b'_') => {
                let start = self.scanner.current_pos();
                self.scanner.next();
                // Find an ident, but ignore the token
                self.ident(start);

                // Parse the suffix
                match self.scanner.slice(start, self.scanner.current_pos()) {
                    "int" => Some(Suffix::Int),
                    "real" => Some(Suffix::Real),
                    "j" | "J" | "i" | "I" => {
                        let value = self.scanner.slice(start, lit_end).parse()
                            .expect("bug: should have been a valid `f64` literal");
                        return self.token_to_current(start, TokenKind::Literal(Lit::Complex(value)));
                    },
                    _ => {
                        //TODO: Produce an error: "invalid suffix `{}` for float literal"
                        todo!()
                    }
                }
            },
            _ => None,
        };

        if real {
            let value = self.scanner.slice(start, lit_end).parse()
                .expect("bug: should have been a valid `f64` literal");
            self.token_to_current(start, TokenKind::Literal(Lit::Real(value, suffix)))
        } else {
            let value = self.scanner.slice(start, lit_end).parse()
                .expect("bug: should have been a valid `i64` literal");
            self.token_to_current(start, TokenKind::Literal(Lit::Integer(value, suffix)))
        }
    }

    /// Advances the scanner until no more digits are found. Returns the number of digits found.
    ///
    /// The final, non-digit character is NOT consumed
    fn digits(&mut self) -> usize {
        let mut digits = 0;
        while let Some(ch) = self.scanner.peek() {
            if ch.is_ascii_digit() {
                digits += 1;
                self.scanner.next();
            } else {
                break;
            }
        }
        digits
    }

    /// Parses an identifier, assuming that the first character has already been parsed
    fn ident(&mut self, start: usize) -> Token {
        // We've already got a valid start character, so let's just look for further characters
        while let Some(ch) = self.scanner.peek() {
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.scanner.next();
            } else {
                break;
            }
        }

        self.token_to_current(start, TokenKind::Ident)
    }

    fn empty_token(&self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.empty_span(start);
        Token {kind, span}
    }

    fn byte_token(&self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.byte_span(start);
        Token {kind, span}
    }

    fn next_token(&mut self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.next_span(start);
        Token {kind, span}
    }

    fn token_to_current(&self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.span(start, self.scanner.current_pos());
        Token {kind, span}
    }
}
