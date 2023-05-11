use std::borrow::Cow;
use std::str::FromStr;

use decorum::Finite;
use logos::Logos;
use thiserror::Error;

pub type Lexer<'source> = logos::Lexer<'source, Token<'source>>;

#[derive(Debug, Clone, Eq, PartialEq, Logos)]
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
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Asterisk,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token("^")]
    Caret,

    #[token("#")]
    Hash,

    #[token("&")]
    Ampersand,

    #[token("~")]
    Tilde,

    #[token("|")]
    Pipe,

    #[token("<<")]
    DoubleAngL,

    #[token(">>")]
    DoubleAngR,

    #[token("//")]
    DoubleSlash,

    #[token("==")]
    DoubleEqual,

    #[token("~=")]
    TildeEqual,

    #[token("<=")]
    AngLEqual,

    #[token(">=")]
    AngREqual,

    #[token("<")]
    AngL,

    #[token(">")]
    AngR,

    #[token("=")]
    Assign,

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

    #[regex("\"(([\\\\](\r\n|.))|[^\"\\\\\r\n])*\"", |lex| RawLiteralString(lex.slice()))]
    #[regex("'(([\\\\](\r\n|.))|[^'\\\\\r\n])*'", |lex| RawLiteralString(lex.slice()))]
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

    pub fn unescape(&self) -> Result<Cow<'s, str>, UnknownEscapeSequenceError> {
        let mut s = self.raw_value();

        if !s.contains('\\') {
            return Ok(s.into());
        }

        let mut r = String::with_capacity(s.len());

        while let Some(i) = s.find('\\') {
            r += &s[..i];
            s = &s[i..];

            let mut iter = s.char_indices();
            let _ = iter.next();
            let (_, c) = iter.next().unwrap();

            let unescaped = {
                let code = match c {
                    'a' => Some(0x07),
                    'b' => Some(0x08),
                    'f' => Some(0x0c),
                    'r' => Some(0x0d),
                    't' => Some(0x09),
                    'v' => Some(0x0b),
                    'n' => Some(0x0a),
                    '\\' => Some(0x5c),
                    '"' => Some(0x22),
                    '\'' => Some(0x27),
                    '\n' => Some(0x0a),
                    '\r' => {
                        let r = (&mut iter)
                            .peekable()
                            .next_if(|&(_, c)| c == '\n')
                            .map(|_| 0x0a)
                            .ok_or(UnknownEscapeSequenceError)?;

                        Some(r)
                    }
                    'z' => {
                        while (&mut iter)
                            .peekable()
                            .next_if(|(_, c)| c.is_whitespace())
                            .is_some()
                        {}

                        None
                    }
                    _ => return Err(UnknownEscapeSequenceError),
                };

                code.map(|code| char::from_u32(code).unwrap())
            };

            if let Some(c) = unescaped {
                r.push(c);
            }

            s = if let Some((i, _)) = iter.next() {
                &s[i..]
            } else {
                ""
            };
        }

        r += s;

        Ok(r.into())
    }
}

#[derive(Debug, Error)]
#[error("encountered unknown escape sequence")]
pub struct UnknownEscapeSequenceError;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RawNumber<'s>(&'s str);

impl<'s> RawNumber<'s> {
    pub fn raw_value(&self) -> &'s str {
        self.0
    }

    pub fn parse(&self) -> Result<Number, UnknownNumberFormatError> {
        self.0.parse()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(Finite<f64>),
}

impl FromStr for Number {
    type Err = UnknownNumberFormatError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use lexical::{
            parse_with_options, NumberFormatBuilder, ParseFloatOptions, ParseIntegerOptions,
        };
        use static_assertions::const_assert;
        use std::num::NonZeroU8;

        // Option::unwrap is not const-usable yet, so here goes much unneeded unsafe :(

        // SAFETY: trivial
        const TWO: NonZeroU8 = unsafe { NonZeroU8::new_unchecked(2) };
        const TEN: NonZeroU8 = unsafe { NonZeroU8::new_unchecked(10) };
        const SIXTEEN: NonZeroU8 = unsafe { NonZeroU8::new_unchecked(16) };

        const BASE_10: u128 = NumberFormatBuilder::new()
            .mantissa_radix(10)
            .exponent_base(Some(TEN))
            .exponent_radix(Some(TEN))
            .required_integer_digits(true)
            .required_fraction_digits(true)
            .required_exponent_digits(true)
            .no_special(true)
            .build();

        const BASE_16_EXPBASE_10: u128 = NumberFormatBuilder::new()
            .mantissa_radix(16)
            .exponent_base(Some(TEN))
            .exponent_radix(Some(SIXTEEN))
            .required_integer_digits(true)
            .required_fraction_digits(true)
            .required_exponent_digits(true)
            .no_special(true)
            .build();

        const BASE_16_EXPBASE_2: u128 = NumberFormatBuilder::new()
            .mantissa_radix(16)
            .exponent_base(Some(TWO))
            .exponent_radix(Some(SIXTEEN))
            .required_integer_digits(true)
            .required_fraction_digits(true)
            .required_exponent_digits(true)
            .no_special(true)
            .build();

        const OPTIONS_INT: ParseIntegerOptions = ParseIntegerOptions::new();

        // SAFETY: call to .build_unchecked is always safe in compatible versions of lexical.
        // https://docs.rs/lexical/latest/lexical/parse_float_options/struct.OptionsBuilder.html#safety
        const OPTIONS_FLOAT_E: ParseFloatOptions = unsafe {
            ParseFloatOptions::builder()
                .decimal_point(b'.')
                .exponent(b'e')
                .build_unchecked()
        };

        const OPTIONS_FLOAT_E2: ParseFloatOptions = unsafe {
            ParseFloatOptions::builder()
                .decimal_point(b'.')
                .exponent(b'E')
                .build_unchecked()
        };

        const OPTIONS_FLOAT_P: ParseFloatOptions = unsafe {
            ParseFloatOptions::builder()
                .decimal_point(b'.')
                .exponent(b'p')
                .build_unchecked()
        };

        const OPTIONS_FLOAT_P2: ParseFloatOptions = unsafe {
            ParseFloatOptions::builder()
                .decimal_point(b'.')
                .exponent(b'P')
                .build_unchecked()
        };

        const_assert!(OPTIONS_INT.is_valid());
        const_assert!(OPTIONS_FLOAT_E.is_valid());
        const_assert!(OPTIONS_FLOAT_E2.is_valid());
        const_assert!(OPTIONS_FLOAT_P.is_valid());
        const_assert!(OPTIONS_FLOAT_P2.is_valid());

        let hex = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X"));

        let r = match hex {
            Some(s) => {
                if let Ok(value) = parse_with_options::<_, _, BASE_16_EXPBASE_10>(s, &OPTIONS_INT) {
                    Number::Int(value)
                } else if let Ok(value) =
                    parse_with_options::<f64, _, BASE_16_EXPBASE_10>(s, &OPTIONS_FLOAT_E)
                {
                    Number::Float(value.try_into().unwrap())
                } else if let Ok(value) =
                    parse_with_options::<f64, _, BASE_16_EXPBASE_10>(s, &OPTIONS_FLOAT_E2)
                {
                    Number::Float(value.try_into().unwrap())
                } else if let Ok(value) =
                    parse_with_options::<f64, _, BASE_16_EXPBASE_2>(s, &OPTIONS_FLOAT_P)
                {
                    Number::Float(value.try_into().unwrap())
                } else if let Ok(value) =
                    parse_with_options::<f64, _, BASE_16_EXPBASE_2>(s, &OPTIONS_FLOAT_P2)
                {
                    Number::Float(value.try_into().unwrap())
                } else {
                    return Err(UnknownNumberFormatError);
                }
            }
            None => {
                if let Ok(value) = parse_with_options::<_, _, BASE_10>(s, &OPTIONS_INT) {
                    Number::Int(value)
                } else if let Ok(value) = parse_with_options::<f64, _, BASE_10>(s, &OPTIONS_FLOAT_E)
                {
                    Number::Float(value.try_into().unwrap())
                } else if let Ok(value) =
                    parse_with_options::<f64, _, BASE_10>(s, &OPTIONS_FLOAT_E2)
                {
                    Number::Float(value.try_into().unwrap())
                } else {
                    return Err(UnknownNumberFormatError);
                }
            }
        };

        Ok(r)
    }
}

#[derive(Debug, Copy, Clone, Error)]
#[error("unrecognized number format")]
pub struct UnknownNumberFormatError;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default, Error)]
#[error("unrecognized character sequence")]
pub struct UnrecognizedTokenError;
