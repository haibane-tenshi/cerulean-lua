//! Utilities for handling Lua-style number and string literals.

mod number;

pub use number::{parse, parse_float, parse_int, Number, UnknownNumberFormatError};
