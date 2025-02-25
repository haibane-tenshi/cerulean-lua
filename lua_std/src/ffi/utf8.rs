//! UTF-8 support library
//!
//! # From Lua documentation
//!
//! This library provides basic support for UTF-8 encoding.
//! It provides all its functions inside the table `utf8`.
//! This library does not provide any support for Unicode other than the handling of the encoding.
//! Any operation that needs the meaning of a character, such as character classification, is outside its scope.
//!
//! Unless stated otherwise, all functions that expect a byte position as a parameter
//! assume that the given position is either the start of a byte sequence or one plus the length of the subject string.
//! As in the string library, negative indices count from the end of the string.
//!
//! Functions that create byte sequences accept all values up to `0x7FFFFFFF`, as defined in the original UTF-8 specification;
//! that implies byte sequences of up to six bytes.
//!
//! Functions that interpret byte sequences only accept valid sequences (well formed and not overlong).
//! By default, they only accept byte sequences that result in valid Unicode code points, rejecting values greater than `10FFFF` and surrogates.
//! A boolean argument `lax`, when available, lifts these checks, so that all values up to `0x7FFFFFFF` are accepted.
//! (Not well formed and overlong sequences are still rejected.)
//!
//! # Known incompatibilities
//!
//! *   There is no support for encoding/decoding integer values which do not represent valid Unicode code points.
//!
//!     While it is true that original UTF-8 spec permitted encoding values in range`0..=0x7FFFFFFF`,
//!     IETF [RFC 3629][rfc#3629] that restricted range of available code points only to 4 byte sequences
//!     and removed range of surrogate code points was **published in 2003**.
//!
//!     At this point UTF-8 is universally understood and defined as *Unicode encoding form*.
//!     To clarify, this implies two assumptions:
//!
//!     * It defines a mapping between a binary payload (of up to 21 bits) and an octet stream;
//!     * Binary payload represents a valid Unicode code point.
//!
//!     Here, Lua basically considers the last point *optional*, which is meaningless.
//!     According to the standard there is no such thing as valid UTF-8 encoded sequence that does not contain Unicode code points.
//!     We consider this a **legacy behavior** and current implementation opts out of supporting it.
//!
//!     It is obviously still possible to encode/decode arbitrary 31-bit values to/from octets using UTF-8 scheme (resulting in up to 6-byte sequences),
//!     however it should not be labeled as an actual UTF-8 encoding to avoid confusion.
//!     Instead it should be considered a distinct variable-length encoding.
//!     This functionality can be provided by a different library if needed.
//!
//! [rfc#3629]: https://datatracker.ietf.org/doc/html/rfc3629

use rt::ffi::delegate::{self, Delegate};
use rt::ffi::DLuaFfi;
use rt::value::Types;

enum Error {
    MissingBytes,
    InvalidLeadingByte,
    Overlong,
}

/// Attempt to extract next utf-8 encoded code point.
///
/// Overlong sequences (encoding values into more bytes than necessary) will be rejected.
///
/// # Returns
///
/// Second return indicates number of bytes consumed.
fn next_code_point(data: &[u8]) -> Result<(u32, usize), Error> {
    assert!(!data.is_empty());

    const TRAIL_MASK: u8 = 0b00111111;

    let byte0 = data[0];
    match byte0.leading_ones() {
        0 => {
            let part0 = (byte0 & 0b0111111) as u32;

            Ok((part0, 1))
        }
        2 => {
            let [_, byte1, ..] = data else {
                return Err(Error::MissingBytes);
            };

            let part0 = (byte0 & 0b00011111) as u32;
            let part1 = (byte1 & TRAIL_MASK) as u32;
            let output = (part0 << 6) | part1;

            if (0x80..0x800).contains(&output) {
                Ok((output, 2))
            } else {
                Err(Error::Overlong)
            }
        }
        3 => {
            let [_, byte1, byte2, ..] = data else {
                return Err(Error::MissingBytes);
            };

            let part0 = (byte0 & 0b00001111) as u32;
            let part1 = (byte1 & TRAIL_MASK) as u32;
            let part2 = (byte2 & TRAIL_MASK) as u32;
            let output = (part0 << 12) | (part1 << 6) | part2;

            if (0x800..0x10000).contains(&output) {
                Ok((output, 3))
            } else {
                Err(Error::Overlong)
            }
        }
        4 => {
            let [_, byte1, byte2, byte3, ..] = data else {
                return Err(Error::MissingBytes);
            };

            let part0 = (byte0 & 0b00000111) as u32;
            let part1 = (byte1 & TRAIL_MASK) as u32;
            let part2 = (byte2 & TRAIL_MASK) as u32;
            let part3 = (byte3 & TRAIL_MASK) as u32;
            let output = (part0 << 18) | (part1 << 12) | (part2 << 6) | part3;

            if (0x10000..0x200000).contains(&output) {
                Ok((output, 4))
            } else {
                Err(Error::Overlong)
            }
        }
        _ => Err(Error::InvalidLeadingByte),
    }
}

/// Encode sequence of integers with UTF-32 codes into a string.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(...: int) -> string`
///
/// Receives zero or more integers, converts each one to its corresponding UTF-8 byte sequence and
/// returns a string with the concatenation of all these sequences.
///
/// # Implementation-specific behavior
///
/// *   Integers must represent valid Unicode code points.
///     See module-level notes on [known incompatibilities](self#known-incompatibilities).
pub fn char<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::error::SignatureError;
        use rt::gc::AllocExt;
        use rt::value::Value;

        let mut r = String::new();

        for (i, value) in rt.stack.iter().copied().enumerate() {
            let Value::Int(n) = value else {
                let err = SignatureError::ConversionFailure {
                    index: i,
                    msg: format!("expected integer, found {}", value.type_()),
                };
                return Err(err.into());
            };

            let Ok(n) = n.try_into() else {
                let err = SignatureError::ConversionFailure {
                    index: i,
                    msg: format!(
                        "integer value {n:x} does not represent a valid unicode codepoint"
                    ),
                };
                return Err(err.into());
            };

            let Some(ch) = char::from_u32(n) else {
                let err = SignatureError::ConversionFailure {
                    index: i,
                    msg: format!(
                        "integer value {n:x} does not represent a valid unicode codepoint"
                    ),
                };
                return Err(err.into());
            };

            r.push(ch);
        }

        rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
            stack.clear();
            stack.push(heap.alloc_str(r).downgrade());
        });

        Ok(())
    })
}

/// Iterate over Unicode code points in a string.
///
/// From Lua documentation
///
/// **Signature:**
/// * `(s: string, [lax: bool]) -> (function, string, int)`
///
/// Returns values so that the construction
///
/// ```lua
/// for p, c in utf8.codes(s) do body end
/// ```
///
/// will iterate over all UTF-8 characters in string `s`, with `p` being the position (in bytes) and `c` the code point of each character.
/// It raises an error if it meets any invalid byte sequence.
///
/// # Notes
///
/// * Remember that Lua indexing starts with 1 and not 0.
///
/// # Implementation-specific behavior
///
/// *   `lax` mode is ignored.
///     See module-level notes on [known incompatibilities](self#known-incompatibilities).
///
/// *   It is not required for entire string to be valid utf8,
///     iterator will decode characters one-by-one in order.
///     However encountering invalid byte sequences will result in Lua panic.
pub fn codes<Ty>() -> impl Delegate<Ty>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn iter<Ty>() -> impl Delegate<Ty>
    where
        Ty: Types,
    {
        delegate::from_mut(|mut rt| {
            use rt::ffi::arg_parser::{Int, LuaString, ParseArgs};
            use rt::gc::AllocExt;
            use rt::value::Value;

            let (s, original_pos): (LuaString<_>, Int) = rt.stack.parse(&mut rt.core.gc)?;
            rt.stack.clear();
            let data = s.to_bytes(&rt.core.gc)?;

            let Some(pos) = original_pos.to_offset(data.len()) else {
                let err = rt
                    .core
                    .gc
                    .alloc_error_msg(format!("index {original_pos} out of bounds"));
                return Err(err);
            };

            let tail = &data[pos..];
            if tail.is_empty() {
                rt.stack.transient().push(Value::Nil);
                return Ok(());
            }

            let (code_point, offset) = match next_code_point(tail) {
                Ok(t) => t,
                Err(Error::InvalidLeadingByte | Error::MissingBytes) => {
                    let err = rt.core.gc.alloc_error_msg(format!(
                        "byte sequence at {original_pos} does not denote valid utf8 sequence"
                    ));
                    return Err(err);
                }
                Err(Error::Overlong) => {
                    let err = rt.core.gc.alloc_error_msg(format!(
                        "byte sequence at {original_pos} contains an overlong utf8 sequence"
                    ));
                    return Err(err);
                }
            };

            if char::from_u32(code_point).is_none() {
                let err = rt.core.gc.alloc_error_msg(format!(
                    "utf8 sequence at {original_pos} does not contain a valid unicode code point"
                ));
                return Err(err);
            }

            // Need +1 because Lua indexing start with 1 and not 0.
            let pos = Int::from_index(pos + offset).unwrap();

            rt.stack.transient_in(&mut rt.core.gc, |mut stack, _| {
                stack.push(pos.into());
                stack.push(Value::Int(code_point.into()));
            });

            Ok(())
        })
    }

    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{Boolean, LuaString, Opts, ParseArgs, Split};
        use rt::ffi::{boxed, from_fn};
        use rt::gc::LuaPtr;
        use rt::value::{Callable, Value};

        let (s, rest): (LuaString<_>, Opts<(Boolean,)>) = rt.stack.parse(&mut rt.core.gc)?;
        let (_lax,) = rest.split();

        rt.stack.transient_in(&mut rt.core.gc, |mut stack, heap| {
            let callable = from_fn(iter, "lua_std::std::codes::iter", ());
            let callable = Callable::Rust(LuaPtr(heap.alloc_cell(boxed(callable)))).downgrade();

            stack.push(callable.into());
            stack.push(s.into());
            stack.push(Value::Int(1));
        });

        Ok(())
    })
}

/// Unpack code points as integers on stack.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string, [i: int, [j: int, [lax: bool]]]) -> (...: int)`
///
/// Returns the code points (as integers) from all characters in `s` that start between byte position `i` and `j` (both included).
/// The default for `i` is 1 and for `j` is `i`.
/// It raises an error if it meets any invalid byte sequence.
///
/// # Implementation-specific behavior
///
/// *   `lax` mode is ignored.
///     See module-level notes on [known incompatibilities](self#known-incompatibilities).
pub fn codepoint<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{Boolean, Int, LuaString, Opts, ParseArgs, Split};
        use rt::gc::AllocExt;
        use rt::value::Value;

        let (s, rest): (LuaString<_>, Opts<(Int, Int, Boolean)>) =
            rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();
        let (i, j, _lax) = rest.split();
        let i = i.unwrap_or(Int(1));
        let j = j.unwrap_or(i);

        let bytes = s.to_bytes(&rt.core.gc)?;

        let Some(start) = i.to_offset(bytes.len()) else {
            let err = rt
                .core
                .gc
                .alloc_error_msg(format!("index {i} is out of bounds"));
            return Err(err);
        };

        let Some(end) = j.to_offset(bytes.len()) else {
            let err = rt
                .core
                .gc
                .alloc_error_msg(format!("index {j} is out of bounds"));
            return Err(err);
        };

        let Ok(s) = std::str::from_utf8(&bytes[start..=end]) else {
            let err = rt.core.gc.alloc_error_msg(format!(
                "range {start}..={end} does not contain a valid utf-8 sequence"
            ));
            return Err(err);
        };

        rt.stack
            .transient()
            .extend(s.chars().map(|ch| Value::Int(u32::from(ch).into())));

        Ok(())
    })
}

///
///
/// From Lua documentation
///
/// **Signature:**
/// * `(s: string, [i: int, [j: int, [lax: bool]]]) -> int`
/// * `(s: string, [i: int, [j: int, [lax: bool]]]) -> (fail, int)`
///
/// Returns the number of UTF-8 characters in string `s` that start between positions `i` and `j` (both inclusive).
/// The default for `i` is 1 and for `j` is -1.
/// If it finds any invalid byte sequence, returns **fail** plus the position of the first invalid byte.
///
/// # Implementation-specific behavior
///
/// *   `lax` mode is ignored.
///     See module-level notes on [known incompatibilities](self#known-incompatibilities).
pub fn len<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{
            Boolean, FormatReturns, Int, LuaString, Nil, Opts, ParseArgs, Split,
        };
        use rt::gc::AllocExt;

        let (s, rest): (LuaString<_>, Opts<(Int, Int, Boolean)>) =
            rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();
        let (i, j, _lax) = rest.split();
        let i = i.unwrap_or(Int(1));
        let j = j.unwrap_or(Int(-1));

        let bytes = s.to_bytes(&rt.core.gc)?;

        let Some(start) = i.to_offset(bytes.len()) else {
            let err = rt
                .core
                .gc
                .alloc_error_msg(format!("index {i} is out of bounds"));
            return Err(err);
        };

        let Some(end) = j.to_offset(bytes.len()) else {
            let err = rt
                .core
                .gc
                .alloc_error_msg(format!("index {j} is out of bounds"));
            return Err(err);
        };

        match std::str::from_utf8(&bytes[start..=end]) {
            Ok(s) => {
                let len = s.chars().count();

                rt.stack.transient().format(Int(len.try_into().unwrap()));
            }
            Err(err) => {
                let valid_len = err.valid_up_to();

                rt.stack
                    .transient()
                    .format((Nil, Int(valid_len.try_into().unwrap())));
            }
        };

        Ok(())
    })
}

///
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string, n: int, [i: int]) -> int | fail`
///
/// Returns the position (in bytes) where the encoding of the `n-th character of `s` (counting from position `i`) starts.
/// A negative `n` gets characters before position `i`.
/// The default for `i` is 1 when `n` is non-negative and `#s + 1` otherwise,
/// so that `utf8.offset(s, -n)` gets the offset of the `n`-th character from the end of the string.
/// If the specified character is neither in the subject nor right after its end, the function returns **fail**.
///
/// As a special case, when `n` is 0 the function returns the start of the encoding of the character that contains the `i`-th byte of s.
///
/// This function assumes that `s` is a valid UTF-8 string.
///
/// # Implementation-specific behavior
///
/// * This function will Lua panic if `n` is not 0 and `i` does not point at code point boundary.
pub fn offset<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{FormatReturns, Int, LuaString, Nil, Opts, ParseArgs, Split};
        use rt::gc::AllocExt;

        let (s, n, rest): (LuaString<_>, Int, Opts<(Int,)>) = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();
        let (i,) = rest.split();

        let s = s.to_str(&rt.core.gc)?;

        let offset = match n.0 {
            1.. => {
                let i = i.unwrap_or(Int(1));
                let Some(start) = i.to_offset(s.len()) else {
                    let err = rt
                        .core
                        .gc
                        .alloc_error_msg(format!("index {i} is out of bounds"));
                    return Err(err);
                };

                let Some(s) = s.get(start..) else {
                    let err = rt
                        .core
                        .gc
                        .alloc_error_msg(format!("index {i} is not on code point boundary"));
                    return Err(err);
                };

                // Counting from the beginning.
                let nth = usize::try_from(n.0).unwrap() - 1;
                let mut count = 0;
                let mut iter = s.char_indices().inspect(|_| count += 1);
                if let Some((offset, _)) = iter.nth(nth) {
                    offset
                } else if nth == count + 1 {
                    // Lua permits pointing to char after the last.
                    // Its boundary is the length of the string.
                    s.len()
                } else {
                    rt.stack.transient().format(Nil);
                    return Ok(());
                }
            }
            ..0 => {
                let end = match i {
                    Some(i) => match i.to_offset(s.len()) {
                        Some(i) => i,
                        None => {
                            let err = rt
                                .core
                                .gc
                                .alloc_error_msg(format!("index {i} is out of bounds"));
                            return Err(err);
                        }
                    },
                    None => s.len(),
                };

                let Some(s) = s.get(..end) else {
                    let err = rt.core.gc.alloc_error_msg(format!(
                        "index {} is not on code point boundary",
                        i.unwrap()
                    ));
                    return Err(err);
                };

                // Counting from the end.
                let nth = usize::try_from(n.0.unsigned_abs()).unwrap() - 1;
                let mut iter = s.char_indices().rev();
                let Some((offset, _)) = iter.nth(nth) else {
                    rt.stack.transient().format(Nil);
                    return Ok(());
                };

                offset
            }
            0 => {
                // Attempt to find char boundary before or at `i`.

                let i = i.unwrap_or(Int(1));
                let Some(end) = i.to_offset(s.len()) else {
                    let err = rt
                        .core
                        .gc
                        .alloc_error_msg(format!("index {i} is out of bounds"));
                    return Err(err);
                };

                // `str::floor_char_boundary` is unstable.
                // 0 is considered char boundary, so this never panics.
                (0..=end).rev().find(|i| s.is_char_boundary(*i)).unwrap()
            }
        };

        rt.stack
            .transient()
            .format(Int::from_index(offset).unwrap());
        Ok(())
    })
}
