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

/// Convert string to lowercase.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string) -> string`
///
/// Receives a string and returns a copy of this string with all uppercase letters changed to lowercase.
/// All other characters are left unchanged.
/// The definition of what an uppercase letter is depends on the current locale.
///
/// # Implementation-specific behavior
///
/// *   String will be converted to utf8 before modification and converted back afterwards.
///     'Lowercase' is defined according to the terms of the Unicode Derived Core Property `Lowercase`.
pub fn lower<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{LuaString, ParseArgs};
        use rt::gc::AllocExt;

        let s: LuaString<_> = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let s = s.to_str(&rt.core.gc)?;
        let s = s.to_lowercase();
        let s = rt.core.gc.alloc_str(s);

        rt.stack.format_sync(&mut rt.core.gc, s);
        Ok(())
    })
}

/// Repeat a string.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string, n: int, [sep: string]) -> string`
///
/// Returns a string that is the concatenation of `n` copies of the string `s` separated by the string `sep`.
/// The default value for `sep` is the empty string (that is, no separator).
/// Returns the empty string if `n` is not positive.
///
/// (Note that it is very easy to exhaust the memory of your machine with a single call to this function.)
///
/// # Implementation-specific behavior
///
/// *   Strings are not required to contain text.
///     They will be concatenated using raw [`Concat`](rt::value::traits::Concat) op.
pub fn rep<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{Int, LuaString, Opts, ParseArgs, Split};
        use rt::ffi::delegate::StackSlot;
        use rt::gc::AllocExt;
        use rt::value::traits::Concat;

        let (s, n, rest): (LuaString<_>, Int, Opts<(LuaString<_>,)>) =
            rt.stack.parse(&mut rt.core.gc)?;
        let (sep,) = rest.split();

        match n {
            Int(..=0) => {
                let output = rt.core.gc.alloc_str("");
                rt.stack.clear();
                rt.stack.format_sync(&mut rt.core.gc, output);
                Ok(())
            }
            Int(1) => {
                rt.stack.adjust_height(StackSlot(1));
                Ok(())
            }
            Int(n) => {
                let source: &Ty::String = s.as_native(&rt.core.gc)?;
                let sep = sep.map(|t| t.as_native(&rt.core.gc)).transpose()?;

                let mut output = source.clone();

                for _ in 1..n {
                    if let Some(sep) = sep {
                        output.concat(sep);
                    }

                    output.concat(source);
                }

                let output = rt.core.gc.alloc_str(output);
                rt.stack.clear();
                rt.stack.format_sync(&mut rt.core.gc, output);
                Ok(())
            }
        }
    })
}

/// Reverse bytes in string.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string) -> string`
///
/// Returns a string that is the string `s` reversed.
///
/// # Implementation-specific behavior
///
/// *   As is customary in Lua behavior of this function is ambiguous.
///     For the most part `string` library treats strings as *byte sequences*,
///     however it is unlikely that this function will be used on strings with binary data.
///
///     Sadly, user attempting to reverse text is for an unpleasant surprise:
///     *byte-reversed* text is unlikely to contain a valid encoding (which in particular is the case with utf8).
///
///     You should use a different function in order to perform reversal on text.
///     (Note that it may require more than just recording characters in reversed order.
///     For example, Unicode's combining characters are applied to [preceding character][unicode#2.11.1],
///     so blindly reversing character sequence will alter how it is rendered.)
///
/// [unicode#2.11.1]: https://www.unicode.org/versions/Unicode16.0.0/core-spec/chapter-2/#G1821
pub fn reverse<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{LuaString, ParseArgs};
        use rt::gc::AllocExt;

        let s: LuaString<_> = rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();

        let bytes = s.to_bytes(&rt.core.gc)?;
        let bytes: Vec<_> = bytes.iter().copied().rev().collect();

        let output = rt.core.gc.alloc_str(bytes);

        rt.stack.format_sync(&mut rt.core.gc, output);
        Ok(())
    })
}

/// Return a substring
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string, i: int, [j: int]) -> string`
///
/// Returns the substring of `s` that starts at `i` and continues until `j`; `i` and `j` can be negative.
/// If `j` is absent, then it is assumed to be equal to -1 (which is the same as the string length).
/// In particular, the call `string.sub(s,1,j)` returns a prefix of `s` with length `j`,
/// and `string.sub(s, -i)` (for a positive `i`) returns a suffix of `s` with length `i`.
///
/// If, after the translation of negative indices, `i` is less than 1, it is corrected to 1.
/// If `j` is greater than the string length, it is corrected to that length.
/// If, after these corrections, `i` is greater than `j`, the function returns the empty string.
///
/// # Implementation-specific behavior
///
/// * `i` and `j` are treated as *byte* offsets.
pub fn sub<Ty>() -> impl Delegate<Ty>
where
    Ty: Types,
{
    delegate::from_mut(|mut rt| {
        use rt::ffi::arg_parser::{range_from_lua_clamp, Index, LuaString, Opts, ParseArgs, Split};
        use rt::gc::AllocExt;

        let (s, i, rest): (LuaString<_>, Index, Opts<(Index,)>) =
            rt.stack.parse(&mut rt.core.gc)?;
        rt.stack.clear();
        let (j,) = rest.split();

        let bytes = s.to_bytes(&rt.core.gc)?;
        let range = if let Some(j) = j {
            range_from_lua_clamp(i..=j, bytes.len())
        } else {
            range_from_lua_clamp(i.., bytes.len())
        };

        let Some(range) = range else {
            let err = rt
                .core
                .gc
                .alloc_error_msg(format!("index {i} is out of bounds"));
            return Err(err);
        };

        let output = bytes[range].to_vec();
        let output = rt.core.gc.alloc_str(output);

        rt.stack.format_sync(&mut rt.core.gc, output);
        Ok(())
    })
}
