use super::{Alignment, ControlOption, Endianness, Value, ValueOption};

/// Decoder for Lua binary packing format.
pub struct Decoder<'s> {
    source: &'s [u8],
    count: usize,
    endianness: Endianness,
    max_alignment: Alignment,
}

impl<'s> Decoder<'s> {
    /// Construct new decoder with native endianness and alignment to 1 byte.
    pub fn new(source: &'s [u8]) -> Self {
        Self::new_with(source, Endianness::Native, Alignment::new(1).unwrap())
    }

    /// Construct new decoder.
    pub fn new_with(source: &'s [u8], endianness: Endianness, max_alignment: Alignment) -> Self {
        Decoder {
            source,
            count: 0,
            endianness,
            max_alignment,
        }
    }

    fn align_to(&mut self, align: Alignment) -> Result<(), DecodeError> {
        let current = align.min(self.max_alignment);
        let align: usize = current.into_inner().into();

        if !align.is_power_of_two() {
            let err = DecodeError::BadAlignment { align: current };
            return Err(err);
        }

        let tail = self.count % align;
        let pad = if tail > 0 { align - tail } else { 0 };

        self.read(pad)
    }

    fn read(&mut self, count: usize) -> Result<(), DecodeError> {
        if self.source.len() <= count {
            self.source = &self.source[count..];
            self.count += count;
            Ok(())
        } else {
            Err(DecodeError::UnexpectedEoF)
        }
    }

    /// Take control sequence out of byte stream.
    pub fn take_control(&mut self, ctrl: ControlOption) -> Result<(), DecodeError> {
        match ctrl {
            ControlOption::SetEndianness(endianness) => {
                self.endianness = endianness;
                Ok(())
            }
            ControlOption::MaxAlignment(align) => {
                self.max_alignment = align;
                Ok(())
            }
            ControlOption::PadByte => self.read(1),
            ControlOption::AlignTo(align) => self.align_to(align),
        }
    }

    /// Decode value out of byte stream.
    pub fn take_value(&mut self, ctrl: ValueOption) -> Result<Value<'s>, DecodeError> {
        use crate::endian::FromBytes;

        self.align_to(ctrl.align())?;

        match ctrl {
            ValueOption::U8 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(1).unwrap();
                Ok(Value::U8(value))
            }
            ValueOption::U16 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(2).unwrap();
                Ok(Value::U16(value))
            }
            ValueOption::U32 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(4).unwrap();
                Ok(Value::U32(value))
            }
            ValueOption::U64 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(8).unwrap();
                Ok(Value::U64(value))
            }
            ValueOption::I8 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(1).unwrap();
                Ok(Value::I8(value))
            }
            ValueOption::I16 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(2).unwrap();
                Ok(Value::I16(value))
            }
            ValueOption::I32 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(4).unwrap();
                Ok(Value::I32(value))
            }
            ValueOption::I64 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(8).unwrap();
                Ok(Value::I64(value))
            }
            ValueOption::Usize => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(std::mem::size_of::<usize>()).unwrap();
                Ok(Value::Usize(value))
            }
            ValueOption::F32 => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(4).unwrap();
                Ok(Value::F32(value))
            }
            ValueOption::F64 | ValueOption::Number => {
                let value = self
                    .endianness
                    .from_bytes(self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(8).unwrap();
                Ok(Value::F64(value))
            }
            ValueOption::Signed(len) => {
                let value = self
                    .endianness
                    .from_bytes_signed(len, self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(value.len()).unwrap();
                Ok(Value::I128(value.value()))
            }
            ValueOption::Unsigned(len) => {
                let value = self
                    .endianness
                    .from_bytes_unsigned(len, self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(value.len()).unwrap();
                Ok(Value::U128(value.value()))
            }
            ValueOption::StrFixed { len } => {
                let s = self.source.get(..len).ok_or(DecodeError::UnexpectedEoF)?;
                self.read(len).unwrap();
                Ok(Value::Str(s))
            }
            ValueOption::StrC => {
                let len = self
                    .source
                    .iter()
                    .copied()
                    .enumerate()
                    .find_map(|(i, b)| (b == 0).then_some(i))
                    .ok_or(DecodeError::UnexpectedEoF)?;
                let s = &self.source[..len];
                self.read(len).unwrap();
                Ok(Value::Str(s))
            }
            ValueOption::StrDyn { len_width } => {
                let len = self
                    .endianness
                    .from_bytes_unsigned(len_width, self.source)
                    .ok_or(DecodeError::UnexpectedEoF)?;
                self.read(len.len()).unwrap();

                let len: usize = len
                    .value()
                    .try_into()
                    .map_err(|_| DecodeError::LenOverflow { len: len.value() })?;
                let s = self.source.get(..len).ok_or(DecodeError::UnexpectedEoF)?;
                self.read(len).unwrap();
                Ok(Value::Str(s))
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DecodeError {
    /// Reached EoF while there is more data is expected.
    UnexpectedEoF,

    /// Data alignment is not power of 2.
    BadAlignment { align: Alignment },

    /// Length of dynamic string didn't fit into `usize`.
    LenOverflow { len: u128 },
}
