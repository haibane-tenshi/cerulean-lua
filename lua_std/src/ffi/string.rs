//! String manipulation library
//!
//! # From Lua documentation
//!
//! This library provides generic functions for string manipulation, such as finding and extracting substrings, and pattern matching.
//! When indexing a string in Lua, the first character is at position 1 (not at 0, as in C).
//! Indices are allowed to be negative and are interpreted as indexing backwards, from the end of the string.
//! Thus, the last character is at position -1, and so on.
//!
//! The string library provides all its functions inside the table `string`.
//! It also sets a metatable for strings where the `__index` field points to the string table.
//! Therefore, you can use the string functions in object-oriented style.
//! For instance, `string.byte(s,i)` can be written as `s:byte(i)`.
//!
//! The string library assumes one-byte character encodings.

use rt::ffi::delegate::{self, Delegate};
use rt::value::Types;

/// Unpack byte values onto stack.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string, [i: int, [j: int]]) -> (...: int)`
///
/// Returns the internal numeric codes of the characters `s[i]`, `s[i+1]`, ..., `s[j]`.
/// The default value for `i` is 1; the default value for `j` is `i`.
/// These indices are corrected following the same rules of function `string.sub`.
///
/// If, after the translation of negative indices, `i` is less than 1, it is corrected to 1.
/// If `j` is greater than the string length, it is corrected to that length.
/// If, after these corrections, `i` is greater than `j`, the function returns the empty string.
///
/// Numeric codes are not necessarily portable across platforms.
///
/// # Implementation-specific behavior
///
/// *   This function will simply unpack string bytes as integers onto stack.
pub fn byte<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{range_from_lua_clamp, Index, LuaString, Opts, ParseArgs, Split};
        use rt::gc::AllocExt;
        use rt::value::Value;

        let (s, rest): (LuaString<_>, Opts<(Index, Index)>) = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();
        let (i, j) = rest.split();
        let i = i.unwrap_or(Index(1));
        let j = j.unwrap_or(i);

        let bytes = s.to_bytes(&rt.core.gc)?;

        let Some(range) = range_from_lua_clamp(i..=j, bytes.len()) else {
            let err = rt
                .core
                .gc
                .alloc_error_msg(format!("index range [{i}; {j}] is out of bounds"));
            return Err(err);
        };

        rt.stack.transient().extend(
            bytes[range]
                .iter()
                .copied()
                .map(|byte| Value::Int(byte.into())),
        );

        Ok(())
    })
}

/// Pack list of integers into string.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(...: int) -> string`
///
/// Receives zero or more integers.
/// Returns a string with length equal to the number of arguments,
/// in which each character has the internal numeric code equal to its corresponding argument.
///
/// Numeric codes are not necessarily portable across platforms.
///
/// # Implementation-specific behavior
///
/// *   This function will simply pack all integers as bytes into the string.
///     Values that outside of `0..=255` range (`u8::MIN..=u8::MAX`) will cause Lua panic.
pub fn char<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::error::SignatureError;
        use rt::ffi::arg_parser::{Int, TypeMismatchError};
        use rt::gc::AllocExt;

        let mut output: Vec<u8> = Vec::with_capacity(rt.stack.len());

        for (i, value) in rt.stack.drain(..).enumerate() {
            let byte: Int = value.try_into().map_err(|err: TypeMismatchError| {
                SignatureError::ConversionFailure {
                    index: i,
                    msg: err.to_string(),
                }
            })?;
            let Ok(byte) = byte.0.try_into() else {
                let err = SignatureError::ConversionFailure {
                    index: i,
                    msg: format!("integer {byte} cannot be represented as u8"),
                };
                return Err(err.into());
            };

            output.push(byte);
        }

        let s = rt.core.gc.alloc_str(output);
        rt.stack.format_sync(&mut rt.core.gc, s);

        Ok(())
    })
}

/// Return string length in bytes.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string) -> int`
///
/// Receives a string and returns its length. The empty string "" has length 0.
/// Embedded zeros are counted, so "a\000bc\000" has length 5.
pub fn len<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{Int, LuaString, ParseArgs};

        let s: LuaString<_> = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let bytes = s.to_bytes(&rt.core.gc)?;
        let len = bytes.len();

        rt.stack
            .format_sync(&mut rt.core.gc, Int(len.try_into().unwrap()));
        Ok(())
    })
}
