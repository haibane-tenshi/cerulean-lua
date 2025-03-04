//! Routines for handling [Lua binary packing format][lua#6.4.2].
//!
//! Lua standard library defines functions for packing and unpacking binary data into Lua strings (which are effectively `Vec<u8>`)
//! according to a custom format.
//! This crate contains implementations for parsing the format string as well as encoding/decoding routines.
//!
//! # Format string
//!
//! Format string may contain any sequence of the following options.
//! Additionally, space can be used to separate sequences.
//!
//! Options are split into **control** and **value** sequences.
//! Control sequences modify current state of encoder/decoder,
//! value sequences encode/decode actual data.
//!
//! In the following *n* indicates a decimal literal, [*n*] indicates an optional decimal literal.
//!
//! **Control** sequences:
//!
//! * `<` - set little endian
//! * `>` - set big endian
//! * `=` - set native endian
//! * `!`[*n*] - set *maximum* alignment to *n* (default: `usize`)
//! * `x` - emit 1 byte of padding
//! * `X`*op* - emit 0 or more bytes of padding, aligning to *op*, where *op* must be a valid **value** sequence
//!
//! See [alignment](#alignment) section for discussion about target alignment and related pitfalls.
//!
//! **Value** sequences describe data type on wire format:
//!
//! * `b` - treat as `i8`
//! * `B` - treat as `u8`
//! * `h` - treat as `i16`
//! * `H` - treat as `u16`
//! * `l` - treat as `i32`
//! * `L` - treat as `u32`
//! * `T` - treat as `usize`
//! * `f` - treat as `f32`
//! * `d` - treat as `f64`
//! * `i`[*n*] - treat as signed integer of *n* bytes width (default: `usize`)
//! * `I`[*n*] - treat as unsigend integer of *n* bytes width (default: `usize`)
//! * `j` - treat as `i64`
//! * `J` - treat as `u64`
//! * `n` - treat as either `i64` or `f64`
//! * `c`*n* - treat as byte string with length of *n*
//! * `z` - treat as zero-terminated byte string
//! * `s`[*n*] - treat as byte string with preceding *n*-byte wide integer length (default: `usize`)
//!
//! Each value option produces/consumes one data entry.
//!
//! # Custom-sized integers
//!
//! Packing format has special options for packing unconventionally-sized integers, such as `i3` and `I11`.
//! (Note that Lua here specifies number of *bytes* as opposed to number of bits; so for example Rust's `u32` is equivalent to `I4` option).
//! Those are defined in the same way as conventional Rust integers:
//! both signed and unsigned variants use complement of 2,
//! signed integers have range of -2^(*n* * 8 - 1) to 2^(*n* * 8 - 1) - 1 (with 0 mapping to all bits equal to 0),
//! unsigned integers have range of 0 to 2^(*n* * 8) - 1.
//!
//! # Coercions
//!
//! Numeric data may be coerced during encoding/decoding.
//! There are no coercions between integers and floats.
//!
//! Encoder will attempt to coerce input integers to the type specified by the format if such coercion is lossless,
//! i.e. input is within representable range of target type.
//! This coercion may happen between signed and unsigned variants as well.
//!
//! Encoder will forcefully coerce floats, regardless of representability.
//! `f32` to `f64` coercions are always lossless, however `f64` to `f32` coercions may lose precision.
//!
//! Decoder will never perform coercions of native integer and float types and will return them as is.
//! Custom-sized integers will be coerced to a bigger native integer type.
//! Since custom integers can only be at most 16 bytes long, it can always be represented by `i128` or `u128`
//! (which is the only case where those two are generated).
//!
//! String data does not participate in coercions.
//! However, encoded length of `s` option will be internally coerced to `usize` using the same rules.
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
//! Depending on maximum alignment it may be considered invalid to encode/decode data using such options.
//!
//! # Known incompatibilities
//!
//! [lua#6.4.2]: https://www.lua.org/manual/5.4/manual.html#6.4.2

mod lex;

pub use lex::{parse_options, PackOptionError};

/// Selected endianness of packed format.
///
/// Note that it can be changed by format string,
/// so different part of the packed byte sequence may be encoded using different endianness.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum Endianness {
    Big,
    Little,
    #[default]
    Native,
}

/// Byte width of an integer.
///
/// Lua packing format only permits integers widths in `1..=16` range.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ByteLength(u8);

impl ByteLength {
    pub fn new(len: usize) -> Option<Self> {
        (1..=16)
            .contains(&len)
            .then(|| ByteLength(len.try_into().unwrap()))
    }

    pub fn into_inner(self) -> u8 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PackOption {
    Control(ControlOption),
    Value(ValueOption),
}

impl From<ControlOption> for PackOption {
    fn from(value: ControlOption) -> Self {
        PackOption::Control(value)
    }
}

impl From<ValueOption> for PackOption {
    fn from(value: ValueOption) -> Self {
        PackOption::Value(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ControlOption {
    SetEndianness(Endianness),
    MaxAlignment(ByteLength),
    PadByte,
    AlignTo(ByteLength),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueOption {
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

    Unsigned(ByteLength),
    Signed(ByteLength),

    Number,

    StrFixed { len: usize },
    StrC,
    StrDyn { len_width: ByteLength },
}
