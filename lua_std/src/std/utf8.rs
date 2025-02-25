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

use rt::ffi::{self, DLuaFfi};
use rt::runtime::Core;
use rt::value::Types;

use crate::lib::{set_func, set_str};
use crate::traits::{RootTable, TableEntry};

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
#[expect(non_camel_case_types)]
pub struct char;

impl<Ty> TableEntry<Ty> for char
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::utf8::char, "lua_std::std::utf8::char", ());
        set_func("char", fn_body).build(table, core);
    }
}

/// Constant containing regex which matches on single UTF-8 byte sequence.
///
/// # From Lua documentation
///
/// The pattern (a string, not a function) `[\0-\x7F\xC2-\xFD][\x80-\xBF]*` (see §6.4.1),
/// which matches exactly one UTF-8 byte sequence, assuming that the subject is a valid UTF-8 string.
#[expect(non_camel_case_types)]
pub struct charpattern;

impl<Ty> TableEntry<Ty> for charpattern
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        set_str("charpattern", r#"[\0-\x7F\xC2-\xFD][\x80-\xBF]*"#).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct codes;

impl<Ty> TableEntry<Ty> for codes
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::utf8::codes, "lua_std::std::utf8::codes", ());
        set_func("codes", fn_body).build(table, core);
    }
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
#[expect(non_camel_case_types)]
pub struct codepoint;

impl<Ty> TableEntry<Ty> for codepoint
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(
            crate::ffi::utf8::codepoint,
            "lua_std::std::utf8::codepoint",
            (),
        );
        set_func("codepoint", fn_body).build(table, core);
    }
}

/// Calculate number of UTF-8 characters.
///
/// # From Lua documentation
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
#[expect(non_camel_case_types)]
pub struct len;

impl<Ty> TableEntry<Ty> for len
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::utf8::len, "lua_std::std::utf8::len", ());
        set_func("len", fn_body).build(table, core);
    }
}

/// Calculate code point boundary index.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(s: string, n: int, [i: int]) -> int | fail`
///
/// Returns the position (in bytes) where the encoding of the `n`-th character of `s` (counting from position `i`) starts.
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
#[expect(non_camel_case_types)]
pub struct offset;

impl<Ty> TableEntry<Ty> for offset
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = ffi::from_fn(crate::ffi::utf8::offset, "lua_std::std::utf8::offset", ());
        set_func("offset", fn_body).build(table, core);
    }
}
