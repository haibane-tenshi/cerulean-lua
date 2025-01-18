use std::borrow::Cow;

use thiserror::Error;

pub use literal::{Number, UnescapeError, UnknownNumberFormatError};
pub use logos::Logos;

pub type Lexer<'source> = logos::Lexer<'source, Token<'source>>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Logos)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(error = UnrecognizedTokenError)]
pub enum Token<'s> {
    #[token("nil")]
    Nil,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("not")]
    Not,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("elseif")]
    ElseIf,

    #[token("for")]
    For,

    #[token("in")]
    In,

    #[token("while")]
    While,

    #[token("then")]
    Then,

    #[token("repeat")]
    Repeat,

    #[token("until")]
    Until,

    #[token("break")]
    Break,

    #[token("do")]
    Do,

    #[token("end")]
    End,

    #[token("local")]
    Local,

    #[token("function")]
    Function,

    #[token("return")]
    Return,

    #[token("goto")]
    Goto,

    #[token("+")]
    PlusSign,

    #[token("-")]
    MinusSign,

    #[token("*")]
    Asterisk,

    #[token("/")]
    Slash,

    #[token("%")]
    PercentSign,

    #[token("^")]
    Circumflex,

    #[token("#")]
    Hash,

    #[token("&")]
    Ampersand,

    #[token("~")]
    Tilde,

    #[token("|")]
    Pipe,

    #[token("<<")]
    DoubleAngleL,

    #[token(">>")]
    DoubleAngleR,

    #[token("//")]
    DoubleSlash,

    #[token("==")]
    DoubleEqualsSign,

    #[token("~=")]
    TildeEqualsSign,

    #[token("<=")]
    AngleLEqualsSign,

    #[token(">=")]
    AngleREqualsSign,

    #[token("<")]
    AngleL,

    #[token(">")]
    AngleR,

    #[token("=")]
    EqualsSign,

    #[token("(")]
    ParL,

    #[token(")")]
    ParR,

    #[token("{")]
    CurlyL,

    #[token("}")]
    CurlyR,

    #[token("[")]
    BracketL,

    #[token("]")]
    BracketR,

    #[token("::")]
    DoubleColon,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("..")]
    DoubleDot,

    #[token("...")]
    TripleDot,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'s str),

    #[regex(r#""((\\((\r\n)|.))|[^"\\])*""#, |lex| RawLiteralString(lex.slice()))]
    #[regex(r#"'((\\((\r\n)|.))|[^'\\])*'"#, |lex| RawLiteralString(lex.slice()))]
    ShortLiteralString(RawLiteralString<'s>),

    #[regex("[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?", |lex| RawNumber(lex.slice()))]
    #[regex("0[xX][0-9a-fA-F]+([.][0-9a-fA-F]+)?([eEpP][+-]?[0-9a-fA-F]+)?", |lex| RawNumber(lex.slice()))]
    Numeral(RawNumber<'s>),

    #[regex("--.*\n?", logos::skip)]
    Comment,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawLiteralString<'s>(&'s str);

impl<'s> RawLiteralString<'s> {
    pub fn full(&self) -> &'s str {
        self.0
    }

    pub fn raw_value(&self) -> &'s str {
        &self.0[1..self.0.len() - 1]
    }

    pub fn unescape(&self) -> Result<Cow<'s, str>, UnescapeError> {
        literal::unescape(self.raw_value())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawNumber<'s>(&'s str);

impl<'s> RawNumber<'s> {
    pub fn raw_value(&self) -> &'s str {
        self.0
    }

    pub fn parse(&self) -> Result<Number, UnknownNumberFormatError> {
        literal::parse(self.raw_value())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default, Error)]
#[error("unrecognized character sequence")]
pub struct UnrecognizedTokenError;
