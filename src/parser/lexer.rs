use super::scanner::Scanner;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DelimToken {
    /// A round parenthesis (i.e., `(` or `)`).
    Paren,
    /// A square bracket (i.e., `[` or `]`).
    Bracket,
    /// A curly brace (i.e., `{` or `}`).
    Brace,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    /// An integer literal
    Integer(i64),
    /// A real number literal
    Real(f64),
    /// A complex number literal
    Complex(f64),
    /// A byte string literal (e.g. `b"abc"`)
    ///
    /// Any escaped characters will be unescaped
    BStr {
        unescaped_text: String,
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
    OpenDelim(DelimToken),
    /// A closing delimiter (e.g., `}`).
    CloseDelim(DelimToken),

    /// The `.` symbol
    Period,
    /// The `::` symbol
    DoubleColon,
    /// The `:` symbol
    Colon,
    /// The `,` symbol
    Comma,
    /// The `;` symbol
    Semicolon,
    /// The `->` symbol
    RArrow,

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
}

pub struct Token<'a> {
    kind: TokenKind,
    span: &'a str,
}

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        Self {
            scanner,
        }
    }
}
