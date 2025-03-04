use std::ops::Range;

use logos::{Lexer, Logos};

use super::{ByteLength, Endianness, PackOption};

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

        let (token, span) = lexer.next()?;

        let mut inner = || {
            let token = token.map_err(|err| match err {
                LexError::UnrecognizedToken => PackOptionError::UknownOption { offset: span.start },
                LexError::NumberTooBig => PackOptionError::NumberTooBig {
                    range: span.clone(),
                },
            })?;

            let parse_byte_length = |lexer: &mut std::iter::Peekable<_>| {
                if let Some((Ok(Token::Num(len)), span)) = lexer.peek().cloned() {
                    lexer.next();
                    ByteLength::new(len).ok_or(PackOptionError::InvalidIntWidth {
                        range: span,
                        width: len,
                    })
                } else {
                    let align = std::mem::align_of::<usize>();
                    Ok(ByteLength::new(align).unwrap())
                }
            };

            let r = match token {
                Token::LeftAngleBracket => ControlOption::SetEndianness(Endianness::Little).into(),
                Token::RightAngleBracket => ControlOption::SetEndianness(Endianness::Big).into(),
                Token::EqualsSign => ControlOption::SetEndianness(Endianness::Native).into(),
                Token::b => ValueOption::I8.into(),
                Token::B => ValueOption::U8.into(),
                Token::h => ValueOption::I16.into(),
                Token::H => ValueOption::U16.into(),
                Token::l => ValueOption::I32.into(),
                Token::L => ValueOption::U32.into(),
                Token::T => ValueOption::Usize.into(),
                Token::f => ValueOption::F32.into(),
                Token::d => ValueOption::F64.into(),
                Token::j => ValueOption::I64.into(),
                Token::J => ValueOption::U64.into(),
                Token::n => ValueOption::Number.into(),
                Token::z => ValueOption::StrC.into(),
                Token::x => ControlOption::PadByte.into(),
                Token::ExclamationMark => {
                    let len = parse_byte_length(&mut lexer)?;
                    ControlOption::MaxAlignment(len).into()
                }
                Token::i => {
                    let len = parse_byte_length(&mut lexer)?;
                    ValueOption::Signed(len).into()
                }
                Token::I => {
                    let len = parse_byte_length(&mut lexer)?;
                    ValueOption::Unsigned(len).into()
                }
                Token::c => {
                    let len = if let Some((Ok(Token::Num(len)), _)) = lexer.peek().cloned() {
                        lexer.next();
                        len
                    } else {
                        let err = PackOptionError::ExpectedStrLength { offset: span.end };
                        return Err(err);
                    };

                    let r = ValueOption::StrFixed { len };
                    r.into()
                }
                Token::s => {
                    let len = parse_byte_length(&mut lexer)?;
                    let r = ValueOption::StrDyn { len_width: len };
                    r.into()
                }
                Token::X => {
                    let (token, span) = lexer
                        .next()
                        .ok_or(PackOptionError::InvalidAlignTo { range: span.end })?;

                    let token = token.map_err(|err| match err {
                        LexError::UnrecognizedToken => {
                            PackOptionError::UknownOption { offset: span.start }
                        }
                        LexError::NumberTooBig => PackOptionError::NumberTooBig {
                            range: span.clone(),
                        },
                    })?;

                    let width = match token {
                        Token::b | Token::B | Token::z => ByteLength::new(1).unwrap(),
                        Token::h | Token::H => ByteLength::new(2).unwrap(),
                        Token::l | Token::L | Token::f => ByteLength::new(4).unwrap(),
                        Token::j | Token::J | Token::d | Token::n => ByteLength::new(8).unwrap(),
                        Token::T => ByteLength::new(std::mem::size_of::<usize>()).unwrap(),
                        Token::i | Token::I | Token::s => parse_byte_length(&mut lexer)?,
                        Token::c => {
                            let _ = if let Some((Ok(Token::Num(len)), _)) = lexer.peek().cloned() {
                                lexer.next();
                                len
                            } else {
                                let err = PackOptionError::ExpectedStrLength { offset: span.end };
                                return Err(err);
                            };

                            ByteLength::new(1).unwrap()
                        }
                        Token::Space | Token::Num(_) => {
                            let err = PackOptionError::UknownOption { offset: span.start };
                            return Err(err);
                        }
                        Token::LeftAngleBracket
                        | Token::RightAngleBracket
                        | Token::EqualsSign
                        | Token::ExclamationMark
                        | Token::x
                        | Token::X => {
                            let err = PackOptionError::InvalidAlignTo { range: span.start };
                            return Err(err);
                        }
                    };

                    ControlOption::AlignTo(width).into()
                }
                _ => return Err(PackOptionError::UknownOption { offset: span.start }),
            };

            Ok(r)
        };

        Some(inner())
    })
}
