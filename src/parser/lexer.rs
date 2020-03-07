use std::str;
use std::collections::VecDeque;

use crate::diagnostics::Diagnostics;

use super::scanner::Scanner;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
    /// A round parenthesis (i.e., `(` or `)`).
    Paren,
    /// A square bracket (i.e., `[` or `]`).
    Bracket,
    /// A curly brace (i.e., `{` or `}`).
    Brace,
}

/// Some literals may end with a suffix that is meant as hint for type inference
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Suffix {
    /// The suffix `int`
    Int,
    /// The suffix `real`
    Real,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    /// An integer literal, e.g. `1`, `31`, `49928int`, `391real`
    Integer(i64, Option<Suffix>),
    /// A real number literal, e.g. `1.0`, `2.5`, `0.5`
    Real(f64),
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
    Ident,
    /// A keyword
    Keyword(Keyword),
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

    /// A placeholder for a def which could not be computed; this is
    /// propagated to avoid useless error messages.
    Error,

    /// End of file/input
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub span: &'a [u8],
}

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
    diag: &'a Diagnostics,
    /// The next queued tokens, used to implement `peek`
    look_ahead: VecDeque<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(scanner: Scanner<'a>, diag: &'a Diagnostics) -> Self {
        Self {
            scanner,
            diag,
            // capacity = 2 since that's the max supported lookahead
            look_ahead: VecDeque::with_capacity(2),
        }
    }

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
            // If we didn't use `next_impl` here, we would queue and remove tokens infinitely
            let next_token = self.next_impl();
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

        self.next_impl()
    }

    /// The implementation of next (unaware of any look ahead)
    fn next_impl(&mut self) -> Token<'a> {
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
    fn bstr_lit(&mut self, start: usize) -> Token<'a> {
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

        self.token_to_current(start, TokenKind::Literal(Lit::BStr {unescaped_text}))
    }

    /// Parses a numeric literal, given a starting digit
    ///
    /// If the literal is immediately followed by an ident, we will attempt to parse that as a
    /// literal suffix (e.g. `123int` or `3.4j`).
    fn num_lit(&mut self, start: usize) -> Token<'a> {
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
                        return self.token_to_current(start, TokenKind::Literal(Lit::Complex(value)));
                    },
                    suffix => {
                        self.diag.emit_error(format!("invalid suffix `{}` for numeric literal", suffix));
                        return self.token_to_current(suffix_start, TokenKind::Error);
                    },
                }
            },
            _ => None,
        };

        if real {
            if suffix.is_some() {
                //TODO: Produce an error: "real number literals may not have a suffix"
                self.diag.emit_error("real number literals may not have a suffix");
                return self.token_to_current(start, TokenKind::Error);
            }

            let value = self.scanner.slice(start, lit_end).parse()
                .expect("bug: should have been a valid `f64` literal");
            self.token_to_current(start, TokenKind::Literal(Lit::Real(value)))
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
    fn ident(&mut self, start: usize) -> Token<'a> {
        // We've already got a valid start character, so let's just look for further characters
        while let Some(ch) = self.scanner.peek() {
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.scanner.next();
            } else {
                break;
            }
        }

        let kind = match match_keyword(self.scanner.slice(start, self.scanner.current_pos())) {
            Some(kw) => TokenKind::Keyword(kw),
            None => TokenKind::Ident,
        };

        self.token_to_current(start, kind)
    }

    fn empty_token(&self, start: usize, kind: TokenKind) -> Token<'a> {
        let span = self.scanner.empty_span(start);
        Token {kind, span}
    }

    fn byte_token(&self, start: usize, kind: TokenKind) -> Token<'a> {
        let span = self.scanner.byte_span(start);
        Token {kind, span}
    }

    fn next_token(&mut self, start: usize, kind: TokenKind) -> Token<'a> {
        let span = self.scanner.next_span(start);
        Token {kind, span}
    }

    fn token_to_current(&self, start: usize, kind: TokenKind) -> Token<'a> {
        let span = self.scanner.span(start, self.scanner.current_pos());
        Token {kind, span}
    }
}

macro_rules! keywords {
    ($($variant:ident : $kw:literal)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Keyword {
            $($variant),*
        }

        fn match_keyword(ident: &str) -> Option<Keyword> {
            use Keyword::*;
            match ident {
                $($kw => Some($variant),)*
                _ => None,
            }
        }
    };
}

keywords! {
    Abstract : "abstract"
    As : "as"
    Async : "async"
    Await : "await"
    Become : "become"
    Box : "box"
    Break : "break"
    Const : "const"
    Continue : "continue"
    Do : "do"
    Dyn : "dyn"
    Else : "else"
    Enum : "enum"
    Extern : "extern"
    False : "false"
    Final : "final"
    Fn : "fn"
    For : "for"
    If : "if"
    Impl : "impl"
    In : "in"
    Let : "let"
    Loop : "loop"
    Macro : "macro"
    Match : "match"
    Mod : "mod"
    Move : "move"
    Mut : "mut"
    Override : "override"
    Package : "package"
    Priv : "priv"
    Pub : "pub"
    Ref : "ref"
    Return : "return"
    SelfType : "Self"
    SelfValue : "self"
    Static : "static"
    Struct : "struct"
    Super : "super"
    Trait : "trait"
    True : "true"
    Try : "try"
    Type : "type"
    Typeof : "typeof"
    Union : "union"
    Unsafe : "unsafe"
    Unsized : "unsized"
    Use : "use"
    Virtual : "virtual"
    Where : "where"
    While : "while"
    Yield : "yield"
}

#[cfg(test)]
mod tests {
    use super::*;

    use TokenKind::*;

    macro_rules! expect_token {
        ($source:literal, $expected:expr) => {
            let diag = Diagnostics::new(termcolor::ColorChoice::Always);
            let scanner = Scanner::new($source);
            let mut lexer = Lexer::new(scanner, &diag);
            let token = lexer.next();
            assert_eq!(token.kind, $expected);
            let token = lexer.next();
            assert_eq!(token.kind, Eof);
        };
    }

    macro_rules! expect_tokens {
        ($source:literal, $expected:expr) => {
            let diag = Diagnostics::new(termcolor::ColorChoice::Always);
            let scanner = Scanner::new($source);
            let mut lexer = Lexer::new(scanner, &diag);
            for expected_token in $expected {
                let token = lexer.next();
                assert_eq!(&token.kind, expected_token);
            }
            // Ensure that the input is exhausted
            let token = lexer.next();
            assert_eq!(token.kind, Eof);
        };
    }

    macro_rules! expect_error {
        ($source:literal) => {
            expect_token!($source, Error);
        };
    }

    #[test]
    fn integer_literals() {
        expect_token!(b"0", Literal(Lit::Integer(0, None)));
        expect_token!(b"000", Literal(Lit::Integer(0, None)));
        expect_token!(b"013", Literal(Lit::Integer(013, None)));
        expect_token!(b"123", Literal(Lit::Integer(123, None)));
        expect_token!(b"9999", Literal(Lit::Integer(9999, None)));

        expect_token!(b"0int", Literal(Lit::Integer(0, Some(Suffix::Int))));
        expect_token!(b"9301int", Literal(Lit::Integer(9301, Some(Suffix::Int))));
        expect_token!(b"256real", Literal(Lit::Integer(256, Some(Suffix::Real))));
    }

    #[test]
    fn integer_literals_invalid_suffix() {
        expect_error!(b"0foo");
    }

    #[test]
    fn real_literals() {
        expect_token!(b"0.0", Literal(Lit::Real(0.0)));
        expect_token!(b"0.00", Literal(Lit::Real(0.0)));
        expect_token!(b"0.13", Literal(Lit::Real(0.13)));
        expect_token!(b"123.0", Literal(Lit::Real(123.)));
        expect_token!(b"123.0000", Literal(Lit::Real(123.0000)));
        expect_token!(b"999e9", Literal(Lit::Real(999e9)));
        expect_token!(b"99.9e-9", Literal(Lit::Real(99.9e-9)));
        expect_token!(b"99.9e+9", Literal(Lit::Real(99.9e+9)));
        expect_token!(b"99.9E-10", Literal(Lit::Real(99.9e-10)));
        expect_token!(b"99.9E+9", Literal(Lit::Real(99.9e+9)));
    }

    #[test]
    fn real_literals_invalid() {
        expect_tokens!(b"..0", &[Period, Period, Literal(Lit::Integer(0, None))]);
        expect_tokens!(b"0.e", &[Literal(Lit::Integer(0, None)), Period, Ident]);
        expect_error!(b"0.0e");
        expect_error!(b"0.0e+");
        expect_error!(b"0.0e-");
        expect_tokens!(b".0foo", &[Period, Error]);
        // Real number literals may not have a suffix
        expect_tokens!(b"0.0int", &[Error]);
        expect_tokens!(b"0.0real", &[Error]);
    }

    #[test]
    fn complex_literals() {
        expect_token!(b"0j", Literal(Lit::Complex(0.0)));
        expect_token!(b"0J", Literal(Lit::Complex(0.0)));
        expect_token!(b"0i", Literal(Lit::Complex(0.0)));
        expect_token!(b"0I", Literal(Lit::Complex(0.0)));
        expect_token!(b"000j", Literal(Lit::Complex(0.0)));
        expect_token!(b"013j", Literal(Lit::Complex(013.0)));
        expect_token!(b"123j", Literal(Lit::Complex(123.0)));
        expect_token!(b"9999j", Literal(Lit::Complex(9999.0)));
        expect_token!(b"0.0j", Literal(Lit::Complex(0.0)));
        expect_token!(b"0.00j", Literal(Lit::Complex(0.0)));
        expect_token!(b"0.13j", Literal(Lit::Complex(0.13)));
        expect_token!(b"123.0j", Literal(Lit::Complex(123.)));
        expect_token!(b"999e9j", Literal(Lit::Complex(999e9)));
        expect_token!(b"99.9e-9j", Literal(Lit::Complex(99.9e-9)));
        expect_token!(b"99.9e+9j", Literal(Lit::Complex(99.9e+9)));
        expect_token!(b"99.9E-10j", Literal(Lit::Complex(99.9e-10)));
        expect_token!(b"99.9E+9j", Literal(Lit::Complex(99.9e+9)));
        expect_token!(b"0.9E-10j", Literal(Lit::Complex(0.9e-10)));
        expect_token!(b"0.9E+9j", Literal(Lit::Complex(0.9e+9)));
    }

    #[test]
    fn complex_literals_invalid() {
        expect_tokens!(b".0jfoo", &[Period, Error]);
    }

    #[test]
    fn integer_field_access() {
        expect_tokens!(b"123.foo", &[Literal(Lit::Integer(123, None)), Period, Ident]);
    }

    #[test]
    fn unknown_token_start() {
        expect_tokens!(b"123\0456", &[Literal(Lit::Integer(123, None)), Error, Literal(Lit::Integer(456, None))]);
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
