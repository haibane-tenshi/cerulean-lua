//! Utilities for handling Lua-denoted numbers and string literals.
//!
//! # Number parsing
//!
//! You can use provided APIs to parse a string containing Lua-denoted number:
//!
//! * [`parse`] - parse as either int or float
//! * [`parse_int`]
//! * [`parse_float`]

mod number;
mod string;

pub use number::{parse, parse_float, parse_int, Number, UnknownNumberFormatError};
pub use string::{unescape, InvalidUnicodeError, MalformedEscapeError, UnescapeError};
