use std::borrow::Cow;
// use std::error::Error;
// use std::fmt::Display;
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

fn parse_dec_byte(s: &[u8]) -> Option<(u8, usize)> {
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
) -> impl Iterator<Item = Result<(Range<usize>, Part<'_>), UnknownEscape>> {
    let mut offset = 0;
    std::iter::from_fn(move || {
        if s.is_empty() {
            return None;
        }

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

            return Some(Ok((range, Part::Slice(part))));
        };

        let unknown = UnknownEscape { index: offset };

        let mut iter = body.char_indices().peekable();

        let Some((_, ch)) = iter.next() else {
            return Some(Err(unknown));
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
                _ => return Some(Err(unknown)),
            },
            'z' => {
                while iter.next_if(|(_, ch)| ch.is_whitespace()).is_some() {}

                Part::Slice("")
            }
            'u' => {
                if !matches!(iter.next(), Some((_, '{'))) {
                    return Some(Err(unknown));
                }

                let Some((start, _)) = iter.next() else {
                    return Some(Err(unknown));
                };

                let Some(end) = s[start..].find('}') else {
                    return Some(Err(unknown));
                };

                if (start..end).len() > 8 {
                    return Some(Err(unknown));
                }

                let Some(code) = parse_hex(&s[start..end]) else {
                    return Some(Err(unknown));
                };

                iter = s[end..].char_indices().peekable();
                let closing_brace = iter.next();
                debug_assert!(matches!(closing_brace, Some((_, '}'))));

                Part::CodePoint(code)
            }
            'x' => {
                if s.len() < 4 {
                    return Some(Err(unknown));
                }

                // Convert to byte slice.
                // Second boundary is not guaranteed to be on codepoint boundary.
                let inner = &s.as_bytes()[2..4];
                let Some(byte) = parse_hex_byte(inner) else {
                    return Some(Err(unknown));
                };

                iter = s[4..].char_indices().peekable();

                Part::Byte(byte)
            }
            '0'..='9' => {
                let end = 4.min(s.len());

                // Convert to byte slice.
                // Second boundary is not guaranteed to be on codepoint boundary.
                let inner = &s.as_bytes()[1..end];
                let (byte, offset) = parse_dec_byte(inner)?;

                iter = s[offset + 1..].char_indices().peekable();

                Part::Byte(byte)
            }
            _ => return Some(Err(unknown)),
        };

        let end = iter.next().map(|(offset, _)| offset).unwrap_or(s.len());

        let range = offset..offset + end;
        offset += end;
        s = &s[end..];

        Some(Ok((range, part)))
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
    iter: impl Iterator<Item = Result<(Range<usize>, Part<'a>), UnknownEscape>>,
) -> impl Iterator<Item = Result<Utf8Part<'a>, UnescapeError>> {
    let mut iter = iter.peekable();

    std::iter::from_fn(move || {
        let r = match iter.next()? {
            Ok((_, Part::Slice(t))) => Ok(Utf8Part::Slice(t)),
            Ok((_, Part::Char(t))) => Ok(Utf8Part::Char(t)),
            Err(err) => Err(err.into()),
            Ok((range, Part::CodePoint(code))) => match char::from_u32(code) {
                Some(ch) => Ok(Utf8Part::Char(ch)),
                None => {
                    let err = InvalidUnicodeError {
                        range,
                        code_point: Some(code),
                    };

                    Err(err.into())
                }
            },
            Ok((range, Part::Byte(byte))) => {
                let Some(count) = utf8_byte_count(byte) else {
                    let err = InvalidUnicodeError {
                        range,
                        code_point: Some(byte.into()),
                    };
                    return Some(Err(err.into()));
                };

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
                        return Some(Err(err.into()));
                    };
                    *place = next;
                    end = range.end;
                }

                let Ok(s) = std::str::from_utf8(&buf[..count]) else {
                    let err = InvalidUnicodeError {
                        range: start..end,
                        code_point: None,
                    };
                    return Some(Err(err.into()));
                };
                let ch = s.chars().next().unwrap();

                Ok(Utf8Part::Char(ch))
            }
        };

        Some(r)
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

#[derive(Debug, Clone)]
pub enum UnescapeError {
    UnknownEscape(UnknownEscape),
    InvalidUnicode(InvalidUnicodeError),
}

// impl Display for UnescapeError {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let s = match self {
//             UnescapeError::UnknownEscape => "unknown escape sequence",
//             UnescapeError::InvalidUnicode => {
//                 "escape sequence does not represent valid Unicode code point"
//             }
//         };

//         write!(f, "{s}")
//     }
// }

// impl Error for UnescapeError {}

impl From<UnknownEscape> for UnescapeError {
    fn from(value: UnknownEscape) -> Self {
        UnescapeError::UnknownEscape(value)
    }
}

impl From<InvalidUnicodeError> for UnescapeError {
    fn from(value: InvalidUnicodeError) -> Self {
        UnescapeError::InvalidUnicode(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UnknownEscape {
    /// Backslash index.
    pub index: usize,
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
