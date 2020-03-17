use std::sync::Arc;

use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Delim {
    /// A round parenthesis (i.e., `(` or `)`).
    Paren,
    /// A square bracket (i.e., `[` or `]`).
    Bracket,
    /// A curly brace (i.e., `{` or `}`).
    Brace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LitKind {
    /// An integer literal, e.g. `1`, `31`, `49928int`, `391real`
    Integer,
    /// A real number literal, e.g. `1.0`, `2.5`, `0.5`
    Real,
    /// A complex number literal, e.g. `1i`, `2.5j`, `.5J`
    Complex,
    /// A byte string literal (e.g. `b"abc"`)
    BStr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// An identifier
    Ident,
    /// A keyword
    Keyword(Keyword),
    /// A literal of some kind
    Literal(LitKind),

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

#[derive(Debug, Clone, PartialEq)]
pub enum TokenData {
    Ident(Arc<str>),
    /// An integer literal, e.g. `1`, `31`, `49928int`, `391real`
    Integer(i64, Option<Suffix>),
    /// A real number literal, e.g. `1.0`, `2.5`, `0.5`
    Real(f64),
    /// A complex number literal, e.g. `1i`, `2.5j`, `.5J`
    Complex(f64),
    /// A byte string literal (e.g. `b"abc"`)
    BStr {
        /// The unescaped value of the byte string (e.g. "a\\n" => "a\n")
        unescaped_text: Arc<[u8]>,
    },
}

/// Some literals may end with a suffix that is meant as hint for type inference
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Suffix {
    /// The suffix `int`
    Int,
    /// The suffix `real`
    Real,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    /// Some token kinds have associated data
    pub data: Option<TokenData>,
}

macro_rules! keywords {
    ($($variant:ident : $kw:literal)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Keyword {
            $($variant),*
        }

        pub(super) fn match_keyword(ident: &str) -> Option<Keyword> {
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
