//! Routines for handling [Lua binary packing format][lua#6.4.2].
//!
//! Lua standard library defines functions for packing and unpacking binary data into Lua strings (which are effectively `Vec<u8>`)
//! according to a custom format.
//! This crate contains implementations for format string parser as well as encoding/decoding routines.
//!
//! # Format string
//!
//! Format string is a sequence of 0 or more formatting specifiers, possibly separated by space (` `, U+0020).
//! Specifiers are split into two groups: **control** and **value** specifiers.
//!
//! **Control** specifiers alter current state of encoder/decoder, affecting representation of following values.
//! Those include endianness, [alignment](#alignment) (from the beginning of the byte stream), as well as custom padding.
//!
//! **Value** specifiers encode/decode actual data entries.
//! Each value specifier defines representation of single entry in packed form.
//!
//! Below is the full list of recognized specifiers.
//! *n* indicates a decimal literal, \[*n*\] indicates an optional decimal literal.
//!
//! **Control** specifier:
//!
//! * `<` - set little endian
//! * `>` - set big endian
//! * `=` - set native endian
//! * `!`\[*n*\] - set *maximum* alignment to *n* (default: `usize`)
//! * `x` - 1 byte of padding
//! * `X`*op* - 0 or more bytes of padding, aligning to *op*, where *op* must be a valid **value** specifier
//!
//! **Value** specifiers:
//!
//! * `b` - represent as `i8`
//! * `B` - represent as `u8`
//! * `h` - represent as `i16`
//! * `H` - represent as `u16`
//! * `l` - represent as `i32`
//! * `L` - represent as `u32`
//! * `T` - represent as `usize`
//! * `f` - represent as `f32`
//! * `d` - represent as `f64`
//! * `i`\[*n*\] - represent as signed integer of *n* bytes width (default: `usize`)
//! * `I`\[*n*\] - represent as unsigend integer of *n* bytes width (default: `usize`)
//! * `j` - represent as `i64` (Lua signed integer)
//! * `J` - represent as `u64` (Lua unsigned integer)
//! * `n` - represent as `f64`, consume any integer/float (Lua number)
//! * `c`*n* - represent as byte string with length of *n*
//! * `z` - represent as zero-terminated byte string
//! * `s`\[*n*\] - represent as byte string with preceding length represented as *n*-byte wide unsigned integer (default: `usize`)
//!
//! # Custom-sized integers
//!
//! Packing format includes special options for packing unconventionally-sized integers, such as `i3` and `I11`.
//! (Note that Lua here specifies number of *bytes* as opposed to number of bits; so for example Rust's `u32` is equivalent to `I4` option).
//!
//! Those are defined in the same way as conventional Rust integers:
//! * signed variants use complement of 2
//! * signed integers have range of -2^(*n* * 8 - 1) to 2^(*n* * 8 - 1) - 1 (with 0 mapping to all bits set to 0)
//! * unsigned integers have range of 0 to 2^(*n* * 8) - 1
//!
//! # Coercions
//!
//! Packing format supports three categories of data: integers, floats and strings.
//! Coercions between categories are not permitted, so for example you cannot serialize `u32` as `f64`.
//! However, coercions within each category are tentatively allowed for convenience sake.
//!
//! The only exception to this rule is `n` specifier (Lua number).
//! It will forcibly coerce integers to `f64` during encoding even if such conversion is lossy.
//!
//! Encoder behavior:
//! *   Integers may get coerced to target representation if such coercion is lossless,
//!     i.e. input value is representable by target type.
//!     In particular, attempting to serialize negative integers as unsigned integer will always fail.
//! *   Floats can always get coerced to each other.
//!     `f32` to `f64` coercion is always lossless, `f64` to `f32` coercion may lose precision.
//! *   Lua number (`n` option) is an exception to coercion rules.
//!     It will always serialize its output as `f64`, coercing both integers and floats as necessary.
//!     This conversion may be lossy.
//! *   Strings perform no coercions.
//!     Packed representation choice is independent from payload.
//! *   C-style strings (`z` option) payload must not contain embedded zeros, even if it is a terminating one.
//!
//! Decoder behavior:
//! *   Decoder performs no type coercions: produced values will have the same type as their specifier.
//! *   Lua number (`n` option) is always parsed as `f64` due to our encoding convention.
//! *   Custom-sized integers (`i` and `I` options) are deserialized as either `i128` and `u128`.
//!     This is due to the fact that each number may be up to 16 bytes wide.
//! *   After deserializing, length of dynamically-sized string (`s` option) will be additionally coerced to `usize`.
//!
//! # Alignment
//!
//! Both encoding and decoding happen in presence of specific *maximum alignment*.
//! Quoting Lua documentation
//!
//! > Alignment works as follows:
//! > For each option, the format gets extra padding until the data starts at an offset
//! > that is a multiple of the minimum between the option size and the maximum alignment;
//! > this minimum must be a power of 2.
//! > Options "c" and "z" are not aligned; option "s" follows the alignment of its starting integer.
//!
//! Setting low maximum alignment allows to override natural alignment and pack data more efficiently.
//! By default both encoder and decoder set maximum alignment to 1, which is no alignment at all.
//!
//! <div class="warning">
//!
//! As a warning to overeager Rust enthusiasts, packing alignment aligns **from the beginning of the byte stream**,
//! it does not mean that packed data will be actually aligned in memory!
//! You cannot rely on this to prove soundness of unsafe code.
//!
//! Rust is strict about such issues, failing to obey is Undefined Behavior.
//! Make sure to read documentation of [`transmute`](std::intrinsics::transmute) and
//! [nomicon](https://doc.rust-lang.org/nomicon/transmutes.html) pages before doing anything foolish.
//!
//! </div>
//!
//! Pay attention to the wording:
//! data alignment is chosen from its *size*, not natural alignment!
//! This isn't a problem for most options as those emit regular integers or floats for which alignment is equal to their size.
//! However there are three problematic value options which may produce invalid alignments: `i`, `I` and `s`.
//! You should be careful when handling those options with sizes that are not power of 2.
//! Depending on set maximum alignment it may be considered invalid to encode/decode data using such options.
//!
//! # Known incompatibilities
//!
//! *   Original spec defines options in terms of C types: `short`, `long`, etc.
//!     Those may have different width depending on target platform.
//!     This implementation takes liberty and sets representation to fixed well-known Rust types.
//! *   This crate assumes that Lua numeric types are 64 bit wide.
//!     As such `j` option represents `i64`, `J` represents `u64` and `n` represents `f64`.
//!
//! [lua#6.4.2]: https://www.lua.org/manual/5.4/manual.html#6.4.2

mod custom;
mod decoder;
mod encoder;
mod endian;
mod lex;

pub use decoder::{DecodeError, Decoder};
pub use encoder::{EncodeError, Encoder};
pub use endian::Endianness;
pub use lex::{parse_format, PackSpecError};

/// Byte width of an integer.
///
/// Byte width can be any value in `1..=16` range.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByteWidth(u8);

impl ByteWidth {
    pub fn new(len: usize) -> Option<Self> {
        (1..=16)
            .contains(&len)
            .then(|| ByteWidth(len.try_into().unwrap()))
    }

    pub fn into_inner(self) -> u8 {
        self.0
    }

    fn to_align(self) -> Alignment {
        Alignment(self.0)
    }
}

/// Alignment of packed data.
///
/// Conventionally, this denotes alignment **from the beginning of the byte stream**;
/// this has no relation to alignment of serialized data in actual memory.
///
/// Alignment can be in any value in `1..=16` range.
/// Technically, alignments that are not power-of-2 are permitted,
/// so it may not be a valid alignment in Rust sense.
/// However attempting to encode/decode data with such alignment may fail.
/// See [aligment](crate#alignment) section for more details.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Alignment(u8);

impl Alignment {
    pub fn new(len: usize) -> Option<Self> {
        (1..=16)
            .contains(&len)
            .then(|| Alignment(len.try_into().unwrap()))
    }

    pub fn into_inner(self) -> u8 {
        self.0
    }
}

/// Packing format specifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PackSpec {
    Control(ControlSpec),
    Value(ValueSpec),
}

impl From<ControlSpec> for PackSpec {
    fn from(value: ControlSpec) -> Self {
        PackSpec::Control(value)
    }
}

impl From<ValueSpec> for PackSpec {
    fn from(value: ValueSpec) -> Self {
        PackSpec::Value(value)
    }
}

/// **Control** specifier.
///
/// Control specifiers alter encoder/decoder state, affecting representation of following values.
/// They do not interact with actual data.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ControlSpec {
    /// Set endianness for representation of following values.
    SetEndianness(Endianness),

    /// Set maximum allowed [alignment](crate#alignment).
    MaxAlignment(Alignment),

    /// Emit/consume a padding byte.
    ///
    /// Our encoder uses 0 as padding byte, but decoders should not rely on this knowledge.
    PadByte,

    /// Emit/consume padding bytes, aligning next item to specified alignment.
    ///
    /// Note that this option is still subject to current maximum alignment.
    ///
    /// Our encoder uses 0 as padding byte, but decoders should not rely on this knowledge.
    AlignTo(Alignment),
}

/// **Value** specifier.
///
/// Value specifiers encode/decode actual data entries.
/// Each value specifier defines representation of single entry in packed form.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueSpec {
    U8,
    U16,
    U32,
    U64,
    Usize,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,

    /// Use unsigned integer of [custom length](crate#custom-sized-integers).
    Unsigned(ByteWidth),

    /// Use signed integer of [custom length](crate#custom-sized-integers).
    Signed(ByteWidth),

    /// Use Lua number.
    ///
    /// Lua likes to pun integers and floats, and this option is direct consequence of that.
    ///
    /// During encoding this option will accept either integer or float and will encode it as `f64`.
    /// This process may be lossy.
    ///
    /// During decoding this option will expect a `f64`.
    Number,

    /// Use string of fixed length.
    StrFixed {
        len: usize,
    },

    /// Use C-style string (denoted by trailing `\0` byte).
    ///
    /// It is an error for payload to contain embedded zeros (even if it is a terminating one).
    StrC,

    /// Use dynamically-sized string.
    ///
    /// This option will place string length as integer of specific [custom length](crate#custom-sized-integers) before its content.
    StrDyn {
        len_width: ByteWidth,
    },
}

impl ValueSpec {
    /// Requested alignment of this specifier.
    pub fn align(self) -> Alignment {
        use ValueSpec::*;

        match self {
            U8 | I8 | StrC | StrFixed { .. } => Alignment::new(1).unwrap(),
            U16 | I16 => Alignment::new(2).unwrap(),
            U32 | I32 | F32 => Alignment::new(4).unwrap(),
            U64 | I64 | F64 | Number => Alignment::new(8).unwrap(),
            Usize => Alignment::new(std::mem::size_of::<usize>()).unwrap(),
            Unsigned(width) | Signed(width) | StrDyn { len_width: width } => width.to_align(),
        }
    }
}

/// Possible payloads that can be produced/emitted.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value<'s> {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    Usize(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    F32(f32),
    F64(f64),
    Str(&'s [u8]),
}

impl<'s> Value<'s> {
    fn as_u8(self) -> Result<u8, ValueError> {
        match self {
            Value::U8(value) => Ok(value),
            Value::U16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_i8(self) -> Result<i8, ValueError> {
        match self {
            Value::U8(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => Ok(value),
            Value::I16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_u16(self) -> Result<u16, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => Ok(value),
            Value::U32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_i16(self) -> Result<i16, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => Ok(value.into()),
            Value::I16(value) => Ok(value),
            Value::I32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_u32(self) -> Result<u32, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => Ok(value.into()),
            Value::U32(value) => Ok(value),
            Value::U64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_i32(self) -> Result<i32, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => Ok(value.into()),
            Value::U32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => Ok(value.into()),
            Value::I16(value) => Ok(value.into()),
            Value::I32(value) => Ok(value),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_u64(self) -> Result<u64, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => Ok(value.into()),
            Value::U32(value) => Ok(value.into()),
            Value::U64(value) => Ok(value),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_i64(self) -> Result<i64, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => Ok(value.into()),
            Value::U32(value) => Ok(value.into()),
            Value::U64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => Ok(value.into()),
            Value::I16(value) => Ok(value.into()),
            Value::I32(value) => Ok(value.into()),
            Value::I64(value) => Ok(value),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_u128(self) -> Result<u128, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => Ok(value.into()),
            Value::U32(value) => Ok(value.into()),
            Value::U64(value) => Ok(value.into()),
            Value::U128(value) => Ok(value),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_i128(self) -> Result<i128, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => Ok(value.into()),
            Value::U32(value) => Ok(value.into()),
            Value::U64(value) => Ok(value.into()),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I8(value) => Ok(value.into()),
            Value::I16(value) => Ok(value.into()),
            Value::I32(value) => Ok(value.into()),
            Value::I64(value) => Ok(value.into()),
            Value::I128(value) => Ok(value),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_usize(self) -> Result<usize, ValueError> {
        match self {
            Value::U8(value) => Ok(value.into()),
            Value::U16(value) => Ok(value.into()),
            Value::U32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::U128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::Usize(value) => Ok(value),
            Value::I8(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I16(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I32(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I64(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::I128(value) => value.try_into().map_err(|_| ValueError::Unrepresentable),
            Value::F32(_) | Value::F64(_) | Value::Str(_) => Err(ValueError::IncompatibleType),
        }
    }

    fn as_f32(self) -> Result<f32, ValueError> {
        match self {
            Value::F32(value) => Ok(value),
            Value::F64(value) => Ok(value as _),
            _ => Err(ValueError::IncompatibleType),
        }
    }

    fn as_f64(self) -> Result<f64, ValueError> {
        match self {
            Value::F32(value) => Ok(value as _),
            Value::F64(value) => Ok(value),
            _ => Err(ValueError::IncompatibleType),
        }
    }

    fn as_str(self) -> Result<&'s [u8], ValueError> {
        match self {
            Value::Str(value) => Ok(value),
            _ => Err(ValueError::IncompatibleType),
        }
    }
}

impl From<u8> for Value<'_> {
    fn from(value: u8) -> Self {
        Value::U8(value)
    }
}

impl From<u16> for Value<'_> {
    fn from(value: u16) -> Self {
        Value::U16(value)
    }
}

impl From<u32> for Value<'_> {
    fn from(value: u32) -> Self {
        Value::U32(value)
    }
}

impl From<u64> for Value<'_> {
    fn from(value: u64) -> Self {
        Value::U64(value)
    }
}

impl From<u128> for Value<'_> {
    fn from(value: u128) -> Self {
        Value::U128(value)
    }
}

impl From<i8> for Value<'_> {
    fn from(value: i8) -> Self {
        Value::I8(value)
    }
}

impl From<i16> for Value<'_> {
    fn from(value: i16) -> Self {
        Value::I16(value)
    }
}

impl From<i32> for Value<'_> {
    fn from(value: i32) -> Self {
        Value::I32(value)
    }
}

impl From<i64> for Value<'_> {
    fn from(value: i64) -> Self {
        Value::I64(value)
    }
}

impl From<i128> for Value<'_> {
    fn from(value: i128) -> Self {
        Value::I128(value)
    }
}

impl From<usize> for Value<'_> {
    fn from(value: usize) -> Self {
        Value::Usize(value)
    }
}

impl<'s> From<&'s [u8]> for Value<'s> {
    fn from(value: &'s [u8]) -> Self {
        Value::Str(value)
    }
}

enum ValueError {
    IncompatibleType,
    Unrepresentable,
}
