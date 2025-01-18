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

#[derive(Debug)]
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
            if s.strip_prefix('\\').is_none() {
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

            let mut iter = s.char_indices().peekable();
            let backslash = iter.next();
            debug_assert!(matches!(backslash, Some((_, '\\'))));

            let Some((_, ch)) = iter.next() else {
                return Err(unknown);
            };

            let (part, end) = match ch {
                'a' => (Part::Char('\u{07}'), None),
                'b' => (Part::Char('\u{08}'), None),
                'f' => (Part::Char('\u{0c}'), None),
                'r' => (Part::Char('\u{0d}'), None),
                't' => (Part::Char('\u{09}'), None),
                'v' => (Part::Char('\u{0b}'), None),
                'n' => (Part::Char('\u{0a}'), None),
                '\\' => (Part::Char('\u{5c}'), None),
                '"' => (Part::Char('\u{22}'), None),
                '\'' => (Part::Char('\u{27}'), None),
                '\n' => (Part::Char('\u{0a}'), None),
                '\r' => match iter.peek() {
                    Some((_, '\n')) => (Part::Slice(""), None),
                    _ => return Err(unknown),
                },
                'z' => {
                    while iter.next_if(|(_, ch)| ch.is_whitespace()).is_some() {}

                    (Part::Slice(""), None)
                }
                'u' => {
                    if !matches!(iter.next(), Some((_, '{'))) {
                        return Err(unknown);
                    }

                    let Some((start, _)) = iter.next() else {
                        return Err(unknown);
                    };

                    let Some(end_offset) = s[start..].find('}') else {
                        return Err(unknown);
                    };
                    let end = start + end_offset;
                    let outer_end = end + '}'.len_utf8();
                    let range = offset..offset + outer_end;

                    let err = MalformedEscapeError::Unicode { range };

                    if (start..end).len() > 8 {
                        return Err(err);
                    }

                    let Some(code) = parse_hex(&s[start..end]) else {
                        return Err(err);
                    };

                    (Part::CodePoint(code), Some(outer_end))
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

                    (Part::Byte(byte), Some(4))
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

                    (Part::Byte(byte), Some(end))
                }
                _ => return Err(unknown),
            };

            let end = end
                .or_else(|| iter.next().map(|(offset, _)| offset))
                .unwrap_or(s.len());

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

/// Replace escape sequences in a Lua string literal.
///
/// This function will recognize and replace all escape sequences permitted in [Lua short string literals][lua#3.1].
/// It does not enforce any additional validity rules imposed by language (such as no unescaped line breaks or internal quotes).
/// Those limitations exist to simplify tokenizing and disambiguating Lua programs and of no particular consequence out of that context.
///
/// However, you will get errors if input contains unknown or malformed escape sequences.
///
/// [lua#3.1]: https://www.lua.org/manual/5.4/manual.html#3.1
///
/// # Recognized escape sequences
///
/// Single character sequences:
///
/// * `\a` => `U+0007` (bell)
/// * `\b` => `U+0008` (backspace)
/// * `\f` => `U+000c` (form feed)
/// * `\n` => `U+000a` (line feed)
/// * `\r` => `U+000d` (carriage return)
/// * `\t` => `U+0009` (horizontal tab)
/// * `\v` => `U+000b` (vertical tab)
/// * `\\` => `U+005c` (`\`, backslash)
/// * `\"` => `U+0022` (`"`, double quote)
/// * `\'` => `U+0027` (`'`, single quote)
///
/// Additionally, there are escape sequences for manipulating whitespace:
///
/// * `\` followed by line break (either `LF` or `CRLF`) => `U+000a` (line feed)
/// * `\z` is removed along with any whitespace following it.
///
/// Lua also supports unicode escape sequences:
///
/// * `\u{XXXXX}`, where `XXXXX` is hexadecimal literal representing desired code point
///
/// Lastly, there is support for embedding bytes directly:
///
/// * `\xNN`, where `NN` is exactly 2-digit hexadecimal literal, representing byte content.
/// *   `\N` `\NN` `\NNN`, where `N`, `NN`, `NNN` is a decimal literal, representing byte content.
///     This escape sequence is *greedy* and will consume as many digits as it can.
///
/// # Examples
///
/// Embedding Unicode:
///
/// ```
/// # use std::error::Error;
/// # use literal::unescape;
/// assert_eq!(unescape(r"\u{2B50}")?, "⭐");
/// assert_eq!(unescape(r"\xe2\xad\x90")?, "⭐");
/// assert_eq!(unescape(r"\226\173\144")?, "⭐");
/// # Ok::<_, Box<dyn Error>>(())
/// ```
///
/// Include complex combinations:
///
/// ```
/// # use std::error::Error;
/// # use literal::unescape;
/// assert_eq!(unescape(r"\xe2\x9d\xa4\xef\xb8\x8f")?, "❤️");
/// assert_eq!(unescape(r"\226\157\164\239\184\143")?, "❤️");
/// # Ok::<_, Box<dyn Error>>(())
/// ```
///
/// `\` normalizes following newline:
///
/// ```
/// # use std::error::Error;
/// # use literal::unescape;
/// assert_eq!(unescape(r"break \
/// here")?, "break \nhere");
/// # Ok::<_, Box<dyn Error>>(())
/// ```
///
/// `\z` can remove extraneous whitespace and newlines:
///
/// ```
/// # use std::error::Error;
/// # use literal::unescape;
/// assert_eq!(unescape(r"jump \z     
///                            over")?, "jump over");
/// # Ok::<_, Box<dyn Error>>(())
/// ```
///
/// Strings without any escapes returned as-is:
/// ```
/// # use std::error::Error;
/// # use literal::unescape;
/// assert_eq!(unescape(r"Lua❤️")?, "Lua❤️");
/// # Ok::<_, Box<dyn Error>>(())
/// ```
///
/// # Differences from Lua spec
///
/// *   As you likely noticed, this function operates only with utf8-encoded strings.
///     This puts certain limitations on allowed escape sequences:
///     
///     * Unicode escape sequences must contain a valid code point, which is divergent from Lua spec.
///     * Byte escape sequences are allowed, but when embedded into final string they must form a valid utf8 sequence.
///
/// *  Whitespace consumed by `\z` escape is understood as unicode chars [containing `White_Space` property](char::is_whitespace).
///    Original spec recognizes only ASCII whitespace (which is a subset).
///
/// *  For the sake of simplicity, hexadecimals inside unicode escape sequences can only be up to 8 characters long.
///    This allows to specify up to 32 bits, which is sufficient to represent any unicode code point.
///
/// # Alternatives
///
/// If you don't care about faithfully representing escaped string content (e.g. for purpose of debug printing), consider using [`unescape_lossy`].
///
/// If you work with binary content, consider using [`unescape_bytes`].
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

/// Replace escape sequences in a Lua string literal, gracefully handling bad byte sequences.
///
/// This function will recognize and replace all escape sequences permitted in [Lua short string literals][lua#3.1].
/// See [`unescape`] for details on recognized escapes and how it compares to original Lua spec.
///
/// Unlike `unescape`, bad unicode embeddings will be replaced with '�' (`U+FFFD`, replacement character).
/// This makes unescaping process *lossy*, but guarantees to produce output (unless running into malformed escapes).
/// This function is useful if you intend to print/inspect content for diagnostic purposes.
///
/// # Behavior
///
/// This function affects only escape sequences that can potentially produce invalid unicode:
///
/// * Unicode escape (`\u{XXXXX}`), containing invalid unicode code point will be replaced with '�'.
/// * Each byte escape (`\xNN` or `\NNN`), which is not part of valid utf8 sequence will be replaced with '�'.
///
/// # Examples
///
/// ```
/// # use std::error::Error;
/// # use literal::unescape_lossy;
/// assert_eq!(unescape_lossy(r"\129\130\76\u{75}\x61")?, "��Lua");
/// # Ok::<_, Box<dyn Error>>(())
/// ```
///
/// [lua#3.1]: https://www.lua.org/manual/5.4/manual.html#3.1
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

/// Replace all byte escape sequences in Lua string literal, producing `Vec<u8>`.
///
/// Input must contain only [byte escapes](unescape#recognized-escape-sequences),
/// no other characters or escape sequences are allowed.
///
/// # Examples
///
/// ```
/// # use std::error::Error;
/// # use literal::unescape_bytes;
/// assert_eq!(unescape_bytes(r"\129\130\76\x75\097")?, [129, 130, 76, 117, 97]);
/// # Ok::<_, Box<dyn Error>>(())
/// ```
pub fn unescape_bytes(s: &str) -> Result<Vec<u8>, MalformedByteEscapeError> {
    unescaped_parts(s)
        .map(|part| {
            let part = part.map_err(MalformedByteEscapeError::from_full)?;
            match part {
                (_, Part::Byte(byte)) => Ok(byte),
                (range, _) => Err(MalformedByteEscapeError::Unknown { index: range.start }),
            }
        })
        .collect()
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

impl MalformedEscapeError {
    fn from_byte(value: MalformedByteEscapeError) -> Self {
        match value {
            MalformedByteEscapeError::Unknown { index } => MalformedEscapeError::Unknown { index },
            MalformedByteEscapeError::HexByte { range } => MalformedEscapeError::HexByte { range },
            MalformedByteEscapeError::DecByte { range, value } => {
                MalformedEscapeError::DecByte { range, value }
            }
        }
    }
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

#[derive(Debug, Clone)]
pub enum MalformedByteEscapeError {
    /// Backslash is not followed by any recognized escape sequence.
    Unknown { index: usize },
    /// Malformed hex byte escape sequence.
    HexByte { range: Range<usize> },
    /// Value in decimal byte escape sequence is too big to fit into a byte.
    DecByte { range: Range<usize>, value: u32 },
}

impl MalformedByteEscapeError {
    fn from_full(value: MalformedEscapeError) -> Self {
        match value {
            MalformedEscapeError::Unknown { index } => MalformedByteEscapeError::Unknown { index },
            MalformedEscapeError::HexByte { range } => MalformedByteEscapeError::HexByte { range },
            MalformedEscapeError::DecByte { range, value } => {
                MalformedByteEscapeError::DecByte { range, value }
            }
            MalformedEscapeError::Unicode { range } => {
                MalformedByteEscapeError::Unknown { index: range.start }
            }
        }
    }
}

impl Display for MalformedByteEscapeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", MalformedEscapeError::from_byte(self.clone()))
    }
}

impl Error for MalformedByteEscapeError {}

#[cfg(test)]
mod tests {
    fn test(origin: &str, expected: &str) {
        assert_eq!(super::unescape(origin).unwrap(), expected);
    }

    fn fail(origin: &str) {
        super::unescape(origin).unwrap_err();
    }

    #[test]
    fn verbatim() {
        test("", "");
        test("abcde", "abcde");
        test("Lua", "Lua");

        test("\n", "\n");
        test("\r\n", "\r\n");

        test("❤️", "❤️");
    }

    #[test]
    fn single_character() {
        test(r"\a", "\u{07}");
        test(r"\b", "\u{08}");
        test(r"\f", "\u{0c}");
        test(r"\t", "\u{09}");
        test(r"\v", "\u{0b}");

        test(r"\n", "\n");
        test(r"\r", "\r");

        test(r"\\", "\\");
        test(r#"\""#, "\"");
        test(r"\'", "\'");

        test(r"a\ab", "a\u{07}b");
        test(r"a\bb", "a\u{08}b");
        test(r"a\fb", "a\u{0c}b");
        test(r"a\tb", "a\u{09}b");
        test(r"a\vb", "a\u{0b}b");

        test(r"a\nb", "a\nb");
        test(r"a\rb", "a\rb");

        test(r"a\\b", "a\\b");
        test(r#"a\"b"#, "a\"b");
        test(r"a\'b", "a\'b");
    }

    #[test]
    fn whitespace() {
        test("a\\\n  b", "a\n  b");
        test("a\\\r\n  b", "a\n  b");

        test("a \\\nb", "a \nb");
        test("a \\\r\nb", "a \nb");

        test(r"a \z     b", "a b");
        test("a \\z \n    b", "a b");
        test("a \\z \r\n    b", "a b");
    }

    #[test]
    fn unicode() {
        test(r"\u{0}", "\u{0000}");
        test(r"\u{0041}", "A");
        test(r"\u{1F527}", "🔧");

        test(r"L\u{75}a", "Lua");
    }

    #[test]
    fn hex_bytes() {
        test(r"\x00", "\u{0000}");
        test(r"\x41", "A");
        test(r"\xf0\x9f\x94\xa7", "🔧");

        test(r"L\x75a", "Lua");
    }

    #[test]
    fn dec_bytes() {
        test(r"\000", "\u{0000}");
        test(r"\065", "A");
        test(r"\240\159\148\167", "🔧");

        test(r"\0", "\u{0000}");
        test(r"\00", "\u{0000}");
        test(r"\000", "\u{0000}");
        test(r"\0000", "\u{0000}0");

        test(r"\7", "\u{0007}");
        test(r"\07", "\u{0007}");
        test(r"\007", "\u{0007}");
        test(r"\0007", "\u{0000}7");

        test(r"L\117a", "Lua");

        test(r"\7🔧", "\u{0007}🔧");
        test(r"\07🔧", "\u{0007}🔧");
        test(r"\007🔧", "\u{0007}🔧");
    }

    #[test]
    fn hex_byte_fails() {
        fail(r"\x🔧");
        fail(r"\x0🔧");

        fail(r"\xf0");
        fail(r"\xf0\x9f");
        fail(r"\xf0\x9f\x94");
        fail(r"\xf0\x9f\x94\xe7");
    }

    #[test]
    fn dec_byte_fails() {
        fail(r"\128");
        fail(r"\256");
        fail(r"\999");

        fail(r"\240");
        fail(r"\240\159");
        fail(r"\240\159\148");
        fail(r"\240\159\148\231");
    }
}
