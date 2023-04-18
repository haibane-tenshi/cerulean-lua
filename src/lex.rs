use std::str::FromStr;

use decorum::Finite;
use logos::Logos;

#[derive(Debug, Copy, Clone, PartialEq, Logos)]
#[logos(skip r"[ \t\n\f]+")]
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
    LeftShift,

    #[token(">>")]
    RightShift,

    #[token("//")]
    DoubleSlash,

    #[token("==")]
    Equal,

    #[token("~=")]
    NotEqual,

    #[token("<=")]
    LessThanOrEqual,

    #[token(">=")]
    GreaterThanOrEqual,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

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

    #[regex(r#""(([\\].)|.)*""#)]
    #[regex(r#"'(([\\].)|.)*'"#)]
    ShortLiteralString(&'s str),

    #[regex("[0-9]+(.[0-9]+)?([eE][+-]?[0-9]+)?", |lex| lex.slice().parse())]
    #[regex("0[xX][0-9a-fA-F]+([.][0-9a-fA-F]+)?(([eE]|[pP])[+-]?[0-9a-fA-F]+)?", |lex| lex.slice().parse())]
    Numeral(Number),

    #[regex("--.*\n?")]
    Comment(&'s str),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Number {
    Uint(u64),
    Float(Finite<f64>),
}

impl FromStr for Number {
    type Err = ();

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
                    Number::Uint(value)
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
                    return Err(());
                }
            }
            None => {
                if let Ok(value) = parse_with_options::<_, _, BASE_10>(s, &OPTIONS_INT) {
                    Number::Uint(value)
                } else if let Ok(value) = parse_with_options::<f64, _, BASE_10>(s, &OPTIONS_FLOAT_E)
                {
                    Number::Float(value.try_into().unwrap())
                } else if let Ok(value) =
                    parse_with_options::<f64, _, BASE_10>(s, &OPTIONS_FLOAT_E2)
                {
                    Number::Float(value.try_into().unwrap())
                } else {
                    return Err(());
                }
            }
        };

        Ok(r)
    }
}
