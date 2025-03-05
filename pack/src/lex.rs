use std::ops::Range;

use logos::{Lexer, Logos};

use super::{ByteWidth, Endianness, PackSpec};

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

/// Parse format specifier string.
///
/// See [description of the format](crate#format-string) in crate-level documentation.
pub fn parse_format(s: &str) -> impl Iterator<Item = Result<PackSpec, PackOptionError>> + use<'_> {
    use logos::Lexer;

    use super::{ControlSpec, ValueSpec};

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
                        Token::b => ValueSpec::I8,
                        Token::B => ValueSpec::U8,
                        Token::h => ValueSpec::I16,
                        Token::H => ValueSpec::U16,
                        Token::l => ValueSpec::I32,
                        Token::L => ValueSpec::U32,
                        Token::T => ValueSpec::Usize,
                        Token::f => ValueSpec::F32,
                        Token::d => ValueSpec::F64,
                        Token::j => ValueSpec::I64,
                        Token::J => ValueSpec::U64,
                        Token::n => ValueSpec::Number,
                        Token::z => ValueSpec::StrC,
                        Token::i => {
                            let len = parse_byte_width(lexer)?;
                            ValueSpec::Signed(len)
                        }
                        Token::I => {
                            let len = parse_byte_width(lexer)?;
                            ValueSpec::Unsigned(len)
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

                            ValueSpec::StrFixed { len }
                        }
                        Token::s => {
                            let len = parse_byte_width(lexer)?;
                            ValueSpec::StrDyn { len_width: len }
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
                    ControlSpec::SetEndianness(Endianness::Little).into()
                }
                Token::RightAngleBracket => {
                    lexer.next();
                    ControlSpec::SetEndianness(Endianness::Big).into()
                }
                Token::EqualsSign => {
                    lexer.next();
                    ControlSpec::SetEndianness(Endianness::Native).into()
                }
                Token::x => {
                    lexer.next();
                    ControlSpec::PadByte.into()
                }
                Token::ExclamationMark => {
                    lexer.next();
                    let len = parse_byte_width(&mut lexer)?;
                    ControlSpec::MaxAlignment(len.to_align()).into()
                }
                Token::X => {
                    lexer.next();
                    let next = parse_value(&mut lexer)
                        .ok_or(PackOptionError::InvalidAlignTo { range: span.end })??;

                    ControlSpec::AlignTo(next.align()).into()
                }
                _ => parse_value(&mut lexer).unwrap()?.into(),
            };

            Ok(r)
        };

        Some(inner())
    })
}
