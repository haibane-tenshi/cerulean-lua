//! Utilities for handling Lua-denoted numbers and string literals.
//!
//! # Number parsing
//!
//! You can use provided APIs to parse a string containing Lua-denoted number:
//!
//! * [`parse`] - parse as either int or float
//! * [`parse_int`]
//! * [`parse_float`]
//!
//! # String unescaping
//!
//! The following functions allow to replace escape sequences in Lua short string literals:
//!
//! * [`unescape`]
//! * [`unescape_lossy`] - similar to `unescape`, but don't error on invalid Unicode
//! * [`unescape_bytes`] - handle strings containing only byte escapes

mod number;
mod string;

pub use number::{parse, parse_float, parse_int, Number, UnknownNumberFormatError};
pub use string::{
    unescape, unescape_bytes, unescape_lossy, InvalidUnicodeError, MalformedByteEscapeError,
    MalformedEscapeError, UnescapeError,
};
