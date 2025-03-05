use std::fmt::Display;
use std::io::Write;

use super::{Alignment, ControlOption, Endianness, Value, ValueError, ValueOption};

/// Encoder for Lua binary packing format.
#[derive(Debug, Clone, Copy)]
pub struct Encoder<W> {
    writer: W,
    count: usize,
    endianness: Endianness,
    max_alignment: Alignment,
}

impl<W> Encoder<W> {
    /// Construct new encoder with native endianness and alignment to 1 byte.
    pub fn new(sink: W) -> Self {
        Self::new_with(sink, Endianness::Native, Alignment::new(1).unwrap())
    }

    /// Construct new encoder.
    ///
    /// Refer to section about [alignment](crate#alignment) about behavior and caveats.
    pub fn new_with(sink: W, endianness: Endianness, max_alignment: Alignment) -> Self {
        Encoder {
            writer: sink,
            count: 0,
            endianness,
            max_alignment,
        }
    }
}

impl<W> Encoder<W>
where
    W: Write,
{
    fn write_bytes(&mut self, mut buf: &[u8]) -> std::io::Result<()> {
        // Adopted from Write::write_all.
        // We want to correctly record how many characters was written because of alignment rules.
        use std::io::{Error, ErrorKind};

        while !buf.is_empty() {
            match self.writer.write(buf) {
                Ok(0) => {
                    return Err(Error::new(ErrorKind::UnexpectedEof, ""));
                }
                Ok(n) => {
                    buf = &buf[n..];
                    self.count += n;
                }
                Err(ref e) if e.kind() == ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    fn align_to(&mut self, align: Alignment) -> Result<(), EncodeError> {
        const PADDING: [u8; 16] = [0; 16];

        let len = align.min(self.max_alignment);
        let align: usize = len.into_inner().into();

        if !align.is_power_of_two() {
            let err = EncodeError::BadAlignment { align: len };
            return Err(err);
        }

        let tail = self.count % align;
        let pad = if tail > 0 { align - tail } else { 0 };

        self.write_bytes(&PADDING[..pad])?;
        Ok(())
    }

    fn write(&mut self, data: &[u8]) -> Result<(), EncodeError> {
        self.write_bytes(data)?;
        Ok(())
    }

    /// Put in a control sequence.
    pub fn put_control(&mut self, ctrl: ControlOption) -> Result<(), EncodeError> {
        match ctrl {
            ControlOption::SetEndianness(endianness) => {
                self.endianness = endianness;
                Ok(())
            }
            ControlOption::MaxAlignment(align) => {
                self.max_alignment = align;
                Ok(())
            }
            ControlOption::PadByte => {
                self.write_bytes(&[0])?;
                Ok(())
            }
            ControlOption::AlignTo(align) => self.align_to(align),
        }
    }

    /// Put in a value.
    pub fn put_value(&mut self, ctrl: ValueOption, value: Value<'_>) -> Result<(), EncodeError> {
        use crate::custom::{Signed, Unsigned};
        use crate::endian::ToBytes;

        let mut buf = [0; 16];
        let buf = &mut buf;

        self.align_to(ctrl.align())?;

        match ctrl {
            ValueOption::U8 => {
                let value = value.as_u8()?;
                self.write(&[value])?;
                Ok(())
            }
            ValueOption::U16 => {
                let value = value.as_u16()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::U32 => {
                let value = value.as_u32()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::U64 => {
                let value = value.as_u64()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::I8 => {
                let value = value.as_i8()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::I16 => {
                let value = value.as_i16()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::I32 => {
                let value = value.as_i32()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::I64 => {
                let value = value.as_i64()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::Usize => {
                let value = value.as_usize()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::F32 => {
                let value = value.as_f32()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::F64 => {
                let value = value.as_f64()?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::Number => {
                let value = if let Ok(value) = value.as_i64() {
                    value as _
                } else if let Ok(value) = value.as_f64() {
                    value
                } else {
                    return Err(ValueError::IncompatibleType.into());
                };
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::Signed(width) => {
                let value = value.as_i128()?;
                let value = Signed::new(value, width).ok_or(ValueError::Unrepresentable)?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::Unsigned(width) => {
                let value = value.as_u128()?;
                let value = Unsigned::new(value, width).ok_or(ValueError::Unrepresentable)?;
                let bytes = self.endianness.to_bytes(value, buf);
                self.write(bytes)?;
                Ok(())
            }
            ValueOption::StrC => {
                let s = value.as_str()?;

                let inner_zero = s
                    .iter()
                    .copied()
                    .enumerate()
                    .find_map(|(i, b)| (b == 0).then_some(i));
                if let Some(offset) = inner_zero {
                    let err = EncodeError::StrInnerZero { offset };
                    return Err(err);
                }

                self.write(s)?;
                self.write(&[0])?;
                Ok(())
            }
            ValueOption::StrFixed { len } => {
                let s = value.as_str()?;

                if s.len() != len {
                    let err = EncodeError::StrBadLength {
                        actual_len: s.len(),
                        expected_len: len,
                    };
                    return Err(err);
                }

                self.write(s)?;
                Ok(())
            }
            ValueOption::StrDyn { len_width } => {
                let s = value.as_str()?;
                let len = Unsigned::new(s.len().try_into().unwrap(), len_width)
                    .ok_or(ValueError::Unrepresentable)?;
                let bytes = self.endianness.to_bytes(len, buf);

                self.write(bytes)?;
                self.write(s)?;
                Ok(())
            }
        }
    }
}

/// Possible encoding errors.
#[derive(Debug)]
pub enum EncodeError {
    /// Attempted to serialize value incompatible with target type.
    ///
    /// Encoder does not perform coercions between integers, floats and/or strings.
    /// Coercions within each group are permitted, but may trigger other errors.
    IncompatibleType,

    /// Integer cannot be represented in target type.
    ///
    /// This error happens when target integer type doesn't have enough bits to faithfully represent the argument
    /// or when coercing negative integer to unsigned representation.
    Unrepresentable,

    /// Attempted to serialize C-style string with embedded zeros.
    StrInnerZero { offset: usize },

    /// Attempted to serialize static string with wrong length.
    StrBadLength {
        expected_len: usize,
        actual_len: usize,
    },

    /// Data alignment is not power of 2.
    BadAlignment { align: Alignment },

    /// Writing to sink failed.
    Io(std::io::Error),
}

impl From<ValueError> for EncodeError {
    fn from(value: ValueError) -> Self {
        match value {
            ValueError::Unrepresentable => EncodeError::Unrepresentable,
            ValueError::IncompatibleType => EncodeError::IncompatibleType,
        }
    }
}

impl From<std::io::Error> for EncodeError {
    fn from(value: std::io::Error) -> Self {
        EncodeError::Io(value)
    }
}

impl Display for EncodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EncodeError::IncompatibleType => {
                write!(f, "provided value is incompatible with target type")
            }
            EncodeError::Unrepresentable => {
                write!(f, "integer cannot be represented as target type")
            }
            EncodeError::StrInnerZero { offset } => {
                write!(f, "c-style string contains embedded zero at index {offset}")
            }
            EncodeError::StrBadLength {
                expected_len,
                actual_len,
            } => write!(
                f,
                "expected byte string of length {expected_len}, but its length is {actual_len}"
            ),
            EncodeError::BadAlignment { align } => write!(
                f,
                "target alignment must be a power of 2 (resolved to {})",
                align.into_inner()
            ),
            EncodeError::Io(err) => Display::fmt(err, f),
        }
    }
}
