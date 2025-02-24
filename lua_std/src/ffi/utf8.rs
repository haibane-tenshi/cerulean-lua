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

use rt::ffi::delegate::{self, Delegate};
use rt::value::Types;

/// Encode sequence of integers with UTF-32 codes into a string.
///
/// # From Lua documentation
///
/// Receives zero or more integers, converts each one to its corresponding UTF-8 byte sequence and
/// returns a string with the concatenation of all these sequences.
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
