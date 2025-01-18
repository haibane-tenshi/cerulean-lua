use std::borrow::Cow;
use std::error::Error;
use std::fmt::Display;
use std::ops::Range;

use lexical::NumberFormatBuilder;

const INT_BASE10: u128 = NumberFormatBuilder::new()
    .digit_separator(None)
    .mantissa_radix(10)
    .base_prefix(None)
    .base_suffix(None)
    .no_positive_mantissa_sign(true)
    .required_mantissa_sign(false)
    .no_integer_leading_zeros(false)
    .build();

const INT_BASE16: u128 = NumberFormatBuilder::new()
    .digit_separator(None)
    .mantissa_radix(16)
    .base_prefix(None)
    .base_suffix(None)
    .no_positive_mantissa_sign(true)
    .required_mantissa_sign(false)
    .no_integer_leading_zeros(false)
    .build();

fn parse_hex(s: &str) -> Option<u32> {
    use lexical::{parse_with_options, ParseIntegerOptions};

    parse_with_options::<_, _, INT_BASE16>(s, &ParseIntegerOptions::new()).ok()
}

fn parse_hex_byte(s: &[u8]) -> Option<u8> {
    use lexical::{parse_with_options, ParseIntegerOptions};

    parse_with_options::<_, _, INT_BASE16>(s, &ParseIntegerOptions::new()).ok()
}

fn parse_dec_byte(s: &[u8]) -> Option<(u32, usize)> {
    use lexical::{parse_partial_with_options, ParseIntegerOptions};

    parse_partial_with_options::<_, _, INT_BASE10>(s, &ParseIntegerOptions::new()).ok()
}

enum Part<'a> {
    Slice(&'a str),
    Char(char),
    CodePoint(u32),
    Byte(u8),
}

fn unescaped_parts(
    mut s: &str,
) -> impl Iterator<Item = Result<(Range<usize>, Part<'_>), MalformedEscapeError>> {
    let mut offset = 0;
    std::iter::from_fn(move || {
        if s.is_empty() {
            return None;
        }

        let mut body = || {
            let Some(body) = s.strip_prefix('\\') else {
                let (end, part, leftover) = if let Some(index) = s.find('\\') {
                    let (part, leftover) = s.split_at(index);
                    (index, part, leftover)
                } else {
                    (s.len(), s, "")
                };

                let range = offset..offset + end;
                offset += end;
                s = leftover;

                return Ok((range, Part::Slice(part)));
            };

            let unknown = MalformedEscapeError::Unknown { index: offset };

            let mut iter = body.char_indices().peekable();

            let Some((_, ch)) = iter.next() else {
                return Err(unknown);
            };

            let part = match ch {
                'a' => Part::Char('\u{07}'),
                'b' => Part::Char('\u{08}'),
                'f' => Part::Char('\u{0c}'),
                'r' => Part::Char('\u{0d}'),
                't' => Part::Char('\u{09}'),
                'v' => Part::Char('\u{0b}'),
                'n' => Part::Char('\u{0a}'),
                '\\' => Part::Char('\u{5c}'),
                '"' => Part::Char('\u{22}'),
                '\'' => Part::Char('\u{27}'),
                '\n' => Part::Char('\u{0a}'),
                '\r' => match iter.peek() {
                    Some((_, '\n')) => Part::Slice(""),
                    _ => return Err(unknown),
                },
                'z' => {
                    while iter.next_if(|(_, ch)| ch.is_whitespace()).is_some() {}

                    Part::Slice("")
                }
                'u' => {
                    if !matches!(iter.next(), Some((_, '{'))) {
                        return Err(unknown);
                    }

                    let Some((start, _)) = iter.next() else {
                        return Err(unknown);
                    };

                    let Some(end) = s[start..].find('}') else {
                        return Err(unknown);
                    };

                    assert_eq!('}'.len_utf8(), 1);
                    let outer_end = end + 1;

                    let err = MalformedEscapeError::Unicode {
                        range: offset..offset + outer_end,
                    };

                    if (start..end).len() > 8 {
                        return Err(err);
                    }

                    let Some(code) = parse_hex(&s[start..end]) else {
                        return Err(err);
                    };

                    iter = s[outer_end..].char_indices().peekable();

                    Part::CodePoint(code)
                }
                'x' => {
                    let err = MalformedEscapeError::HexByte {
                        range: offset..offset + 4.min(s.len()),
                    };

                    if s.len() < 4 {
                        return Err(err);
                    }

                    // Convert to byte slice.
                    // Second boundary is not guaranteed to be on codepoint boundary.
                    let inner = &s.as_bytes()[2..4];
                    let byte = parse_hex_byte(inner).ok_or(err)?;

                    iter = s[4..].char_indices().peekable();

                    Part::Byte(byte)
                }
                '0'..='9' => {
                    let end = 4.min(s.len());

                    // Convert to byte slice.
                    // Second boundary is not guaranteed to be on codepoint boundary.
                    let inner = &s.as_bytes()[1..end];
                    let (value, end) =
                        parse_dec_byte(inner).expect("input should contain at least 1 digit");
                    // +1 to account for skipped backslash byte.
                    let end = end + 1;

                    let byte = value
                        .try_into()
                        .map_err(|_| MalformedEscapeError::DecByte {
                            range: offset..offset + end,
                            value,
                        })?;

                    iter = s[end..].char_indices().peekable();

                    Part::Byte(byte)
                }
                _ => return Err(unknown),
            };

            let end = iter.next().map(|(offset, _)| offset).unwrap_or(s.len());

            let range = offset..offset + end;
            offset += end;
            s = &s[end..];

            Ok((range, part))
        };

        Some(body())
    })
}

fn utf8_byte_count(byte: u8) -> Option<usize> {
    match byte.leading_ones() {
        0 => Some(1),
        2 => Some(2),
        3 => Some(3),
        4 => Some(4),
        _ => None,
    }
}

enum Utf8Part<'a> {
    Slice(&'a str),
    Char(char),
}

fn convert<'a>(
    iter: impl Iterator<Item = Result<(Range<usize>, Part<'a>), MalformedEscapeError>>,
) -> impl Iterator<Item = Result<Utf8Part<'a>, UnescapeError>> {
    let mut iter = iter.peekable();

    std::iter::from_fn(move || {
        let item = iter.next()?;
        let body = || match item? {
            (_, Part::Slice(t)) => Ok(Utf8Part::Slice(t)),
            (_, Part::Char(t)) => Ok(Utf8Part::Char(t)),
            (range, Part::CodePoint(code)) => match char::from_u32(code) {
                Some(ch) => Ok(Utf8Part::Char(ch)),
                None => {
                    let err = InvalidUnicodeError {
                        range,
                        code_point: Some(code),
                    };

                    Err(err.into())
                }
            },
            (range, Part::Byte(byte)) => {
                let count = utf8_byte_count(byte).ok_or_else(|| InvalidUnicodeError {
                    range: range.clone(),
                    code_point: Some(byte.into()),
                })?;

                let mut buf = [byte, 0, 0, 0];
                let start = range.start;
                let mut end = range.end;

                for place in buf[1..count].iter_mut() {
                    let next = iter.next_if(|item| matches!(item, Ok((_, Part::Byte(_)))));
                    let Some(Ok((range, Part::Byte(next)))) = next else {
                        let err = InvalidUnicodeError {
                            range: start..end,
                            code_point: None,
                        };
                        return Err(err.into());
                    };
                    *place = next;
                    end = range.end;
                }

                let s = std::str::from_utf8(&buf[..count]).map_err(|_| InvalidUnicodeError {
                    range: start..end,
                    code_point: None,
                })?;
                let ch = s.chars().next().unwrap();

                Ok(Utf8Part::Char(ch))
            }
        };

        Some(body())
    })
}

fn convert_lossy<'a>(
    iter: impl Iterator<Item = Result<(Range<usize>, Part<'a>), MalformedEscapeError>>,
) -> impl Iterator<Item = Result<Utf8Part<'a>, MalformedEscapeError>> {
    let mut iter = iter.map(|item| item.map(|(_, part)| part)).peekable();

    std::iter::from_fn(move || {
        let item = iter.next()?;
        let body = || match item? {
            Part::Slice(t) => Ok(Utf8Part::Slice(t)),
            Part::Char(t) => Ok(Utf8Part::Char(t)),
            Part::CodePoint(code) => match char::from_u32(code) {
                Some(ch) => Ok(Utf8Part::Char(ch)),
                None => Ok(Utf8Part::Char(char::REPLACEMENT_CHARACTER)),
            },
            Part::Byte(byte) => {
                let Some(count) = utf8_byte_count(byte) else {
                    return Ok(Utf8Part::Char(char::REPLACEMENT_CHARACTER));
                };

                const REPLACEMENT: &str = "����";

                let mut buf = [byte, 0, 0, 0];

                for i in 1..count {
                    let next = iter.next_if(|item| matches!(item, Ok(Part::Byte(_))));
                    let Some(Ok(Part::Byte(next))) = next else {
                        return Ok(Utf8Part::Slice(&REPLACEMENT[..i]));
                    };
                    buf[i] = next;
                }

                let Ok(s) = std::str::from_utf8(&buf[..count]) else {
                    return Ok(Utf8Part::Slice(&REPLACEMENT[..count]));
                };
                let ch = s.chars().next().unwrap();

                Ok(Utf8Part::Char(ch))
            }
        };

        Some(body())
    })
}

/// Replace escape sequences in a Lua string.
pub fn unescape(s: &str) -> Result<Cow<'_, str>, UnescapeError> {
    if !s.contains('\\') {
        return Ok(s.into());
    }

    let mut r = String::with_capacity(s.len());

    for part in convert(unescaped_parts(s)) {
        match part? {
            Utf8Part::Slice(s) => r += s,
            Utf8Part::Char(s) => r.push(s),
        }
    }

    Ok(r.into())
}

pub fn unescape_lossy(s: &str) -> Result<Cow<'_, str>, MalformedEscapeError> {
    if !s.contains('\\') {
        return Ok(s.into());
    }

    let mut r = String::with_capacity(s.len());

    for part in convert_lossy(unescaped_parts(s)) {
        match part? {
            Utf8Part::Slice(s) => r += s,
            Utf8Part::Char(s) => r.push(s),
        }
    }

    Ok(r.into())
}

#[derive(Debug, Clone)]
pub enum UnescapeError {
    MalformedEscape(MalformedEscapeError),
    InvalidUnicode(InvalidUnicodeError),
}

impl Display for UnescapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnescapeError::MalformedEscape(err) => write!(f, "{err}"),
            UnescapeError::InvalidUnicode(err) => write!(f, "{err}"),
        }
    }
}

impl Error for UnescapeError {}

impl From<MalformedEscapeError> for UnescapeError {
    fn from(value: MalformedEscapeError) -> Self {
        UnescapeError::MalformedEscape(value)
    }
}

impl From<InvalidUnicodeError> for UnescapeError {
    fn from(value: InvalidUnicodeError) -> Self {
        UnescapeError::InvalidUnicode(value)
    }
}

/// Escape sequence does not denote valid Unicode code point.
#[derive(Debug, Clone)]
pub struct InvalidUnicodeError {
    /// Byte range that denotes escape sequence.
    pub range: Range<usize>,

    /// Evaluated code point.
    ///
    /// This value will be `None` for bad byte sequences.
    pub code_point: Option<u32>,
}

impl Display for InvalidUnicodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let InvalidUnicodeError { range, code_point } = self;

        if let Some(code_point) = code_point {
            write!(f, "utf8 escape sequence at range {range:?} was recognized, but does not contain valid unicode code point (evaluated to {code_point})")
        } else {
            write!(f, "utf8 escape sequence at range {range:?} was recognized, but does not contain valid unicode code point")
        }
    }
}

impl Error for InvalidUnicodeError {}

#[derive(Debug, Clone)]
pub enum MalformedEscapeError {
    /// Backslash is not followed by any recognized escape sequence.
    Unknown { index: usize },
    /// Malformed unicode escape sequence.
    Unicode { range: Range<usize> },
    /// Malformed hex byte escape sequence.
    HexByte { range: Range<usize> },
    /// Value in decimal byte escape sequence is too big to fit into a byte.
    DecByte { range: Range<usize>, value: u32 },
}

impl Display for MalformedEscapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalformedEscapeError::Unknown { index } => {
                write!(f, "encountered backslash `\\` at index {index}, which is not followed by any recognized escape sequence")
            }
            MalformedEscapeError::Unicode { range } => {
                write!(f, "malformed unicode escape sequence at range {range:?}")
            }
            MalformedEscapeError::HexByte { range } => {
                write!(f, "malfromed hex byte escape sequence at range {range:?}")
            }
            MalformedEscapeError::DecByte { range, value } => {
                write!(f, "decimal byte escape sequence at range {range:?} evaluated to {value}, which doesn fit into single byte (u8::MAX = {})", u8::MAX)
            }
        }
    }
}

impl Error for MalformedEscapeError {}
