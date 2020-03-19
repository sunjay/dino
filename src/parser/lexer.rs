use std::sync::Arc;
use std::collections::HashMap;

use crate::diagnostics::Diagnostics;

use super::scanner::Scanner;
use super::token::*;

use TokenKind::*;

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
    diag: &'a Diagnostics,
    interned_strings: HashMap<String, Arc<str>>,
}

impl<'a> Lexer<'a> {
    pub fn new(scanner: Scanner<'a>, diag: &'a Diagnostics) -> Self {
        Self {
            scanner,
            diag,
            interned_strings: HashMap::new(),
        }
    }

    /// Returns the next token in the input
    pub fn next(&mut self) -> Token {
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

            (b'.', _) => self.byte_token(start, Period),
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

            (b'~', _) => self.byte_token(start, Tilde),
            (b'^', _) => self.byte_token(start, Caret),

            (b'+', _) => self.byte_token(start, Plus),
            (b'-', _) => self.byte_token(start, Minus),
            (b'*', _) => self.byte_token(start, Star),
            // Note that we don't need to check for `//` or `/*` because comments have already been
            // ignored
            (b'/', _) => self.byte_token(start, Slash),
            (b'%', _) => self.byte_token(start, Percent),

            (b'b', Some(b'"')) => {
                // Skip the quote
                self.scanner.next();
                self.bstr_lit(start)
            },

            (b'0' ..= b'9', _) => self.num_lit(start),

            (b'a' ..= b'z', _) |
            (b'A' ..= b'Z', _) |
            (b'_', _) => self.ident(start),

            (ch, _) => {
                self.diag.emit_error(format!("unknown start of token `{}`", ch as char));
                self.byte_token(start, TokenKind::Error)
            },
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

        if count > 0 {
            //TODO: Emit the span of the block comment (from current pos at the start of this function)
            self.diag.emit_error("unterminated block comment");
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

                    Some(ch) => {
                        self.diag.emit_error(format!("unknown character escape: `\\{}`", ch as char));
                        return self.byte_token(start, TokenKind::Error);
                    },
                    None => {
                        //TODO: Emit span for the start of the byte string literal
                        self.diag.emit_error("unterminated double quote byte string");
                        return self.byte_token(start, TokenKind::Error);
                    },
                };
                unescaped_text.push(unescaped_char);

            } else {
                unescaped_text.push(ch);
            }
        }

        let data = TokenData::BStr {unescaped_text: unescaped_text.into()};
        self.token_to_current(start, TokenKind::Literal(LitKind::BStr), data)
    }

    /// Parses a numeric literal, given a starting digit
    ///
    /// If the literal is immediately followed by an ident, we will attempt to parse that as a
    /// literal suffix (e.g. `123int` or `3.4j`).
    fn num_lit(&mut self, start: usize) -> Token {
        // true if this is a real number literal
        let mut real = false;
        // Try to get as many additional digits as possible. Totally fine if we get zero more
        // digits, because we already have one.
        self.digits();

        // Check for floating-point part (optional)
        // Only a floating point part if followed by a digit since otherwise this could be a period
        // followed by an ident (i.e. a field access) or something
        match (self.scanner.peek(), self.scanner.peek2()) {
            (Some(b'.'), Some(b'0' ..= b'9')) => {
                // Skip the '.'
                self.scanner.next();
                // Scan for one or more digits
                self.digits();
                real = true;
            },

            _ => {},
        }

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
                    self.diag.emit_error("expected at least one digit in exponent");
                    return self.byte_token(start, TokenKind::Error);
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
                let suffix_start = self.scanner.current_pos();
                self.scanner.next();
                // Find an ident, but ignore the token
                self.ident(suffix_start);

                // Parse the suffix
                match self.scanner.slice(suffix_start, self.scanner.current_pos()) {
                    "int" => Some(Suffix::Int),
                    "real" => Some(Suffix::Real),
                    "j" | "J" | "i" | "I" => {
                        let value = self.scanner.slice(start, lit_end).parse()
                            .expect("bug: should have been a valid `f64` literal");
                        let data = TokenData::Complex(value);
                        return self.token_to_current(start, TokenKind::Literal(LitKind::Complex), data);
                    },
                    suffix => {
                        self.diag.emit_error(format!("invalid suffix `{}` for numeric literal", suffix));
                        return self.token_to_current(suffix_start, TokenKind::Error, None);
                    },
                }
            },
            _ => None,
        };

        if real {
            if suffix.is_some() {
                self.diag.emit_error("real number literals may not have a suffix");
                return self.token_to_current(start, TokenKind::Error, None);
            }

            let value = self.scanner.slice(start, lit_end).parse()
                .expect("bug: should have been a valid `f64` literal");
            self.token_to_current(start, TokenKind::Literal(LitKind::Real), TokenData::Real(value))
        } else {
            let value = self.scanner.slice(start, lit_end).parse()
                .expect("bug: should have been a valid `i64` literal");
            let data = TokenData::Integer(value, suffix);
            self.token_to_current(start, TokenKind::Literal(LitKind::Integer), data)
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

        let value = self.scanner.slice(start, self.scanner.current_pos());
        match match_keyword(value) {
            Some(kw) => self.token_to_current(start, TokenKind::Keyword(kw), None),
            None => {
                let data = TokenData::Ident(self.intern_str(value));
                self.token_to_current(start, TokenKind::Ident, data)
            },
        }
    }

    fn empty_token(&self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.empty_span(start);
        Token {kind, span, data: None}
    }

    fn byte_token(&self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.byte_span(start);
        Token {kind, span, data: None}
    }

    fn next_token(&mut self, start: usize, kind: TokenKind) -> Token {
        let span = self.scanner.next_span(start);
        Token {kind, span, data: None}
    }

    fn token_to_current(&self, start: usize, kind: TokenKind, data: impl Into<Option<TokenData>>) -> Token {
        let span = self.scanner.span(start, self.scanner.current_pos());
        Token {kind, span, data: data.into()}
    }

    fn intern_str(&mut self, value: &str) -> Arc<str> {
        match self.interned_strings.get(value) {
            Some(interned) => interned.clone(),
            None => {
                let interned: Arc<str> = value.into();
                self.interned_strings.insert(value.to_string(), interned.clone());
                interned
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::span::Span;

    macro_rules! t {
        ($kind:expr) => (
            Token {
                kind: $kind,
                span: Span {start: 0, end: 0},
                data: None,
            }
        );
        ($kind:expr, $data:expr) => (
            Token {
                kind: $kind,
                span: Span {start: 0, end: 0},
                data: Some($data),
            }
        );
    }

    macro_rules! ident {
        ($value:expr) => (
            t!(Ident, TokenData::Ident($value.into()))
        );
    }

    macro_rules! int {
        ($value:expr) => (
            t!(Literal(LitKind::Integer), TokenData::Integer($value, None))
        );
        ($value:expr, $suffix:expr) => (
            t!(Literal(LitKind::Integer), TokenData::Integer($value, Some($suffix)))
        );
    }

    macro_rules! real {
        ($value:expr) => (
            t!(Literal(LitKind::Real), TokenData::Real($value))
        );
    }

    macro_rules! complex {
        ($value:expr) => (
            t!(Literal(LitKind::Complex), TokenData::Complex($value))
        );
    }

    macro_rules! expect_token {
        ($source:literal, $expected:expr) => {
            let diag = Diagnostics::new(termcolor::ColorChoice::Always);
            let scanner = Scanner::new($source);
            let mut lexer = Lexer::new(scanner, &diag);
            let token = lexer.next();
            let expected = $expected;
            assert_eq!(token.kind, expected.kind);
            assert_eq!(token.data, expected.data);
            let token = lexer.next();
            assert_eq!(token.kind, Eof);
        };
    }

    macro_rules! expect_tokens {
        ($source:literal, $expected:expr) => {
            let diag = Diagnostics::new(termcolor::ColorChoice::Always);
            let scanner = Scanner::new($source);
            let mut lexer = Lexer::new(scanner, &diag);
            let expected_tokens: &[Token] = $expected;
            for expected_token in expected_tokens {
                let token = lexer.next();
                assert_eq!(token.kind, expected_token.kind);
                assert_eq!(token.data, expected_token.data);
            }
            // Ensure that the input is exhausted
            let token = lexer.next();
            assert_eq!(token.kind, Eof);
        };
    }

    macro_rules! expect_error {
        ($source:literal) => {
            expect_token!($source, t!(Error));
        };
    }

    #[test]
    fn integer_literals() {
        expect_token!(b"0", int!(0));
        expect_token!(b"000", int!(0));
        expect_token!(b"013", int!(013));
        expect_token!(b"123", int!(123));
        expect_token!(b"9999", int!(9999));

        expect_token!(b"0int", int!(0, Suffix::Int));
        expect_token!(b"9301int", int!(9301, Suffix::Int));
        expect_token!(b"256real", int!(256, Suffix::Real));
    }

    #[test]
    fn integer_literals_invalid_suffix() {
        expect_error!(b"0foo");
    }

    #[test]
    fn real_literals() {
        expect_token!(b"0.0", real!(0.0));
        expect_token!(b"0.00", real!(0.0));
        expect_token!(b"0.13", real!(0.13));
        expect_token!(b"123.0", real!(123.));
        expect_token!(b"123.0000", real!(123.0000));
        expect_token!(b"999e9", real!(999e9));
        expect_token!(b"99.9e-9", real!(99.9e-9));
        expect_token!(b"99.9e+9", real!(99.9e+9));
        expect_token!(b"99.9E-10", real!(99.9e-10));
        expect_token!(b"99.9E+9", real!(99.9e+9));
    }

    #[test]
    fn real_literals_invalid() {
        expect_tokens!(b"..0", &[t!(Period), t!(Period), int!(0)]);
        expect_tokens!(b"0.e", &[int!(0), t!(Period), ident!("e")]);
        expect_error!(b"0.0e");
        expect_error!(b"0.0e+");
        expect_error!(b"0.0e-");
        expect_tokens!(b".0foo", &[t!(Period), t!(Error)]);
        // Real number literals may not have a suffix
        expect_error!(b"0.0int");
        expect_error!(b"0.0real");
    }

    #[test]
    fn complex_literals() {
        expect_token!(b"0j", complex!(0.0));
        expect_token!(b"0J", complex!(0.0));
        expect_token!(b"0i", complex!(0.0));
        expect_token!(b"0I", complex!(0.0));
        expect_token!(b"000j", complex!(0.0));
        expect_token!(b"013j", complex!(013.0));
        expect_token!(b"123j", complex!(123.0));
        expect_token!(b"9999j", complex!(9999.0));
        expect_token!(b"0.0j", complex!(0.0));
        expect_token!(b"0.00j", complex!(0.0));
        expect_token!(b"0.13j", complex!(0.13));
        expect_token!(b"123.0j", complex!(123.));
        expect_token!(b"999e9j", complex!(999e9));
        expect_token!(b"99.9e-9j", complex!(99.9e-9));
        expect_token!(b"99.9e+9j", complex!(99.9e+9));
        expect_token!(b"99.9E-10j", complex!(99.9e-10));
        expect_token!(b"99.9E+9j", complex!(99.9e+9));
        expect_token!(b"0.9E-10j", complex!(0.9e-10));
        expect_token!(b"0.9E+9j", complex!(0.9e+9));
    }

    #[test]
    fn complex_literals_invalid() {
        expect_tokens!(b".0jfoo", &[t!(Period), t!(Error)]);
    }

    #[test]
    fn integer_field_access() {
        expect_tokens!(b"123.foo", &[int!(123), t!(Period), ident!("foo")]);
    }

    #[test]
    fn unknown_token_start() {
        expect_tokens!(b"123\0456", &[int!(123), t!(Error), int!(456)]);
    }

    #[test]
    fn nested_block_comment() {
        expect_tokens!(b"/**/", &[]);
        expect_tokens!(b"/*/**/*/", &[]);
        expect_tokens!(b"/* foo

        /* test 123
                */ cool
    *//* okokok */
        ", &[]);
    }
}
