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
