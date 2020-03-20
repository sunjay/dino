use std::fmt;
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

impl fmt::Display for LitKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LitKind::*;
        match self {
            Integer => write!(f, "an integer"),
            Real => write!(f, "a real number"),
            Complex => write!(f, "a complex number"),
            BStr => write!(f, "a byte string"),
        }
    }
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

    /// The `..`
    DoublePeriod,
    /// The `..=`
    DoublePeriodEquals,

    /// The `=` symbol
    Equals,

    /// The `<` symbol
    LessThan,
    /// The `<=` symbol
    LessThanEquals,
    /// The `==` symbol
    DoubleEquals,
    /// The `!=` symbol
    NotEquals,
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
    /// The `~` symbol
    Tilde,
    /// The `|` symbol
    Or,
    /// The `^` symbol
    Caret,
    /// The `<<` symbol
    Shl,
    /// The `>>` symbol
    Shr,
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

    /// A placeholder for a def which could not be computed; this is
    /// propagated to avoid useless error messages.
    Error,

    /// End of file/input
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenKind::*;
        match self {
            Ident => write!(f, "an identifer"),
            Keyword(keyword) => write!(f, "{}", keyword),
            Literal(lit) => write!(f, "{}", lit),

            OpenDelim(Delim::Paren) => write!(f, "`(`"),
            CloseDelim(Delim::Paren) => write!(f, "`)`"),

            OpenDelim(Delim::Bracket) => write!(f, "`[`"),
            CloseDelim(Delim::Bracket) => write!(f, "`]`"),

            OpenDelim(Delim::Brace) => write!(f, "`{{`"),
            CloseDelim(Delim::Brace) => write!(f, "`}}`"),

            Period => write!(f, "`.`"),
            Comma => write!(f, "`,`"),
            Semicolon => write!(f, "`;`"),
            RArrow => write!(f, "`->`"),
            DoubleColon => write!(f, "`::`"),
            Colon => write!(f, "`:`"),

            DoublePeriod => write!(f, "`..`"),
            DoublePeriodEquals => write!(f, "`..=`"),

            Equals => write!(f, "`=`"),

            LessThan => write!(f, "`<`"),
            LessThanEquals => write!(f, "`<=`"),
            DoubleEquals => write!(f, "`==`"),
            NotEquals => write!(f, "`!=`"),
            GreaterThanEquals => write!(f, "`>=`"),
            GreaterThan => write!(f, "`>`"),

            DoubleAnd => write!(f, "`&&`"),
            DoubleOr => write!(f, "`||`"),
            Not => write!(f, "`!`"),

            And => write!(f, "`&`"),
            Tilde => write!(f, "`~`"),
            Or => write!(f, "`|`"),
            Caret => write!(f, "`^`"),
            Shl => write!(f, "`<<`"),
            Shr => write!(f, "`>>`"),
            Plus => write!(f, "`+`"),
            Minus => write!(f, "`-`"),
            Star => write!(f, "`*`"),
            Slash => write!(f, "`/`"),
            Percent => write!(f, "`%`"),

            Error => panic!("The Error token kind should not be formatted"),

            Eof => write!(f, "end of file"),
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    /// Some token kinds have associated data
    pub data: Option<TokenData>,
}

impl Token {
    /// Returns the data as an identifier or panics
    pub fn unwrap_ident(&self) -> &Arc<str> {
        match &self.data {
            Some(TokenData::Ident(ident)) => ident,
            _ => panic!("bug: expected an identifier"),
        }
    }

    /// Returns the data as an integer and its suffix or panics
    pub fn unwrap_integer(&self) -> (i64, Option<Suffix>) {
        match &self.data {
            &Some(TokenData::Integer(value, suffix)) => (value, suffix),
            _ => panic!("bug: expected an integer"),
        }
    }

    /// Returns the data as a real number or panics
    pub fn unwrap_real(&self) -> f64 {
        match &self.data {
            &Some(TokenData::Real(value)) => value,
            _ => panic!("bug: expected a real number"),
        }
    }

    /// Returns the data as a complex number or panics
    pub fn unwrap_complex(&self) -> f64 {
        match &self.data {
            &Some(TokenData::Complex(value)) => value,
            _ => panic!("bug: expected a complex number"),
        }
    }

    /// Returns the data as the unescaped text of a byte string or panics
    pub fn unwrap_bstr(&self) -> &Arc<[u8]> {
        match &self.data {
            Some(TokenData::BStr {unescaped_text}) => unescaped_text,
            _ => panic!("bug: expected a byte string"),
        }
    }
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

        impl fmt::Display for Keyword {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                use Keyword::*;
                write!(f, "{}", match self {
                    $($variant => concat!("`", $kw, "`"),)*
                })
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
