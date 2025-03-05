use std::ops::Range;

use logos::{Lexer, Logos};

use super::{ByteWidth, Endianness, PackOption};

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
#[logos(error = LexError)]
pub(crate) enum Token {
    #[token("<")]
    LeftAngleBracket,

    #[token(">")]
    RightAngleBracket,

    #[token("=")]
    EqualsSign,

    #[token("!")]
    ExclamationMark,

    #[token("b")]
    b,

    #[token("B")]
    B,

    #[token("h")]
    h,

    #[token("H")]
    H,

    #[token("l")]
    l,

    #[token("L")]
    L,

    #[token("j")]
    j,

    #[token("J")]
    J,

    #[token("T")]
    T,

    #[token("i")]
    i,

    #[token("I")]
    I,

    #[token("f")]
    f,

    #[token("d")]
    d,

    #[token("n")]
    n,

    #[token("c")]
    c,

    #[token("z")]
    z,

    #[token("s")]
    s,

    #[token("x")]
    x,

    #[token("X")]
    X,

    #[regex("[0-9]+", parse_usize)]
    Num(usize),

    #[token(" ")]
    Space,
}

fn parse_usize(s: &mut Lexer<'_, Token>) -> Result<usize, LexError> {
    s.slice().parse().map_err(|_| LexError::NumberTooBig)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum LexError {
    #[default]
    UnrecognizedToken,
    NumberTooBig,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackOptionError {
    /// Encountered unrecognized packing option.
    UknownOption {
        /// Offset into option's string.
        offset: usize,
    },
    /// Parsed number is too bit to fit into `usize`.
    NumberTooBig {
        /// Range into option's string denoting the number.
        range: Range<usize>,
    },
    /// Suggested integer width is out of bounds.
    ///
    /// Lua only permits widths in `1..=16` range.
    InvalidIntWidth {
        /// Range into option's string denoting the number.
        range: Range<usize>,

        /// Parsed number.
        width: usize,
    },
    /// Fixed-sized string (option `c`) requires to specify its length.
    ExpectedStrLength {
        /// Offset into option's string where number is expected.
        offset: usize,
    },
    /// Only entry options are permitted as argument to align-to (option `X`).
    InvalidAlignTo { range: usize },
}

pub fn parse_options(
    s: &str,
) -> impl Iterator<Item = Result<PackOption, PackOptionError>> + use<'_> {
    use logos::Lexer;

    use super::{ControlOption, ValueOption};

    let mut lexer = Lexer::new(s).spanned().peekable();

    std::iter::from_fn(move || {
        // Skip spaces between options.
        while lexer.next_if(|t| matches!(t.0, Ok(Token::Space))).is_some() {}

        let (token, span) = lexer.peek().cloned()?;

        let mut inner = || {
            let token = token.map_err(|err| match err {
                LexError::UnrecognizedToken => PackOptionError::UknownOption { offset: span.start },
                LexError::NumberTooBig => PackOptionError::NumberTooBig {
                    range: span.clone(),
                },
            })?;

            let parse_byte_width = |lexer: &mut std::iter::Peekable<_>| {
                if let Some((Ok(Token::Num(len)), span)) = lexer.peek().cloned() {
                    lexer.next();
                    ByteWidth::new(len).ok_or(PackOptionError::InvalidIntWidth {
                        range: span,
                        width: len,
                    })
                } else {
                    let align = std::mem::align_of::<usize>();
                    Ok(ByteWidth::new(align).unwrap())
                }
            };

            let parse_value = |lexer: &mut std::iter::Peekable<_>| {
                let (token, span): (Result<Token, _>, Range<usize>) = lexer.next()?;

                let mut inner = || {
                    let token = token.map_err(|err| match err {
                        LexError::UnrecognizedToken => {
                            PackOptionError::UknownOption { offset: span.start }
                        }
                        LexError::NumberTooBig => PackOptionError::NumberTooBig {
                            range: span.clone(),
                        },
                    })?;

                    let r = match token {
                        Token::b => ValueOption::I8,
                        Token::B => ValueOption::U8,
                        Token::h => ValueOption::I16,
                        Token::H => ValueOption::U16,
                        Token::l => ValueOption::I32,
                        Token::L => ValueOption::U32,
                        Token::T => ValueOption::Usize,
                        Token::f => ValueOption::F32,
                        Token::d => ValueOption::F64,
                        Token::j => ValueOption::I64,
                        Token::J => ValueOption::U64,
                        Token::n => ValueOption::Number,
                        Token::z => ValueOption::StrC,
                        Token::i => {
                            let len = parse_byte_width(lexer)?;
                            ValueOption::Signed(len)
                        }
                        Token::I => {
                            let len = parse_byte_width(lexer)?;
                            ValueOption::Unsigned(len)
                        }
                        Token::c => {
                            let len = if let Some((Ok(Token::Num(len)), _)) = lexer.peek().cloned()
                            {
                                lexer.next();
                                len
                            } else {
                                let err = PackOptionError::ExpectedStrLength { offset: span.end };
                                return Err(err);
                            };

                            ValueOption::StrFixed { len }
                        }
                        Token::s => {
                            let len = parse_byte_width(lexer)?;
                            ValueOption::StrDyn { len_width: len }
                        }
                        _ => {
                            let err = PackOptionError::UknownOption { offset: span.start };
                            return Err(err);
                        }
                    };

                    Ok(r)
                };

                Some(inner())
            };

            let r = match token {
                Token::LeftAngleBracket => {
                    lexer.next();
                    ControlOption::SetEndianness(Endianness::Little).into()
                }
                Token::RightAngleBracket => {
                    lexer.next();
                    ControlOption::SetEndianness(Endianness::Big).into()
                }
                Token::EqualsSign => {
                    lexer.next();
                    ControlOption::SetEndianness(Endianness::Native).into()
                }
                Token::x => {
                    lexer.next();
                    ControlOption::PadByte.into()
                }
                Token::ExclamationMark => {
                    lexer.next();
                    let len = parse_byte_width(&mut lexer)?;
                    ControlOption::MaxAlignment(len.to_align()).into()
                }
                Token::X => {
                    lexer.next();
                    let next = parse_value(&mut lexer)
                        .ok_or(PackOptionError::InvalidAlignTo { range: span.end })??;

                    ControlOption::AlignTo(next.align()).into()
                }
                _ => parse_value(&mut lexer).unwrap()?.into(),
            };

            Ok(r)
        };

        Some(inner())
    })
}
