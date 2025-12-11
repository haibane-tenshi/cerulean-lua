use super::ByteWidth;
use crate::custom::{Signed, Unsigned};

fn is_little_endian() -> bool {
    let value = 1_u16;
    let bytes = value.to_ne_bytes();
    bytes[0] == 1
}

/// Selected endianness of packed format.
///
/// Endianness determines the order in which bytes in multi-byte data is serialized.
/// Little-endian puts least significant bytes first, big-endian puts most significant bytes first.
/// Native-endian is chosen to be either big or little endian to match endianness of your compilation target.
///
/// Note that endianness can be changed by format string,
/// so different part of the packed byte sequence may be encoded using different endianness.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub enum Endianness {
    Big,
    Little,
    #[default]
    Native,
}

impl Endianness {
    #[expect(clippy::wrong_self_convention)]
    pub(crate) fn from_bytes_signed(self, len: ByteWidth, buf: &[u8]) -> Option<Signed> {
        let e: usize = len.into_inner().into();
        let bytes = buf.get(..e)?;

        let is_le = match self {
            Endianness::Little => true,
            Endianness::Big => false,
            Endianness::Native => is_little_endian(),
        };

        let mut buf = [0; 16];
        const SIGN_MASK: u8 = 1 << 7;

        let value = if is_le {
            buf[..e].copy_from_slice(bytes);

            let is_neg = bytes.last().unwrap() & SIGN_MASK != 0;

            if is_neg {
                buf[e..].fill(u8::MAX);
            }

            i128::from_le_bytes(buf)
        } else {
            buf[16 - e..].copy_from_slice(bytes);

            let is_neg = bytes.first().unwrap() & SIGN_MASK != 0;

            if is_neg {
                buf[..16 - e].fill(u8::MAX);
            }

            i128::from_be_bytes(buf)
        };

        let num = Signed::new(value, len).unwrap();
        Some(num)
    }

    #[expect(clippy::wrong_self_convention)]
    pub(crate) fn from_bytes_unsigned(self, len: ByteWidth, buf: &[u8]) -> Option<Unsigned> {
        let e: usize = len.into_inner().into();
        let bytes = buf.get(..e)?;

        let is_le = match self {
            Endianness::Little => true,
            Endianness::Big => false,
            Endianness::Native => is_little_endian(),
        };

        let mut buf = [0; 16];
        let value = if is_le {
            buf[..e].copy_from_slice(bytes);
            u128::from_le_bytes(buf)
        } else {
            buf[16 - e..].copy_from_slice(bytes);
            u128::from_be_bytes(buf)
        };

        let num = Unsigned::new(value, len).unwrap();
        Some(num)
    }
}

pub(crate) trait ToBytes<T> {
    fn to_bytes(self, value: T, buf: &mut [u8; 16]) -> &[u8];
}

impl ToBytes<u8> for Endianness {
    fn to_bytes(self, value: u8, buf: &mut [u8; 16]) -> &[u8] {
        buf[0] = value;
        &buf[..1]
    }
}

impl ToBytes<u16> for Endianness {
    fn to_bytes(self, value: u16, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..2].copy_from_slice(&bytes);
        &buf[..2]
    }
}

impl ToBytes<u32> for Endianness {
    fn to_bytes(self, value: u32, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..4].copy_from_slice(&bytes);
        &buf[..4]
    }
}

impl ToBytes<u64> for Endianness {
    fn to_bytes(self, value: u64, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..8].copy_from_slice(&bytes);
        &buf[..8]
    }
}

impl ToBytes<u128> for Endianness {
    fn to_bytes(self, value: u128, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..16].copy_from_slice(&bytes);
        &buf[..16]
    }
}

impl ToBytes<i8> for Endianness {
    fn to_bytes(self, value: i8, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..1].copy_from_slice(&bytes);
        &buf[..1]
    }
}

impl ToBytes<i16> for Endianness {
    fn to_bytes(self, value: i16, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..2].copy_from_slice(&bytes);
        &buf[..2]
    }
}

impl ToBytes<i32> for Endianness {
    fn to_bytes(self, value: i32, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..4].copy_from_slice(&bytes);
        &buf[..4]
    }
}

impl ToBytes<i64> for Endianness {
    fn to_bytes(self, value: i64, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..8].copy_from_slice(&bytes);
        &buf[..8]
    }
}

impl ToBytes<i128> for Endianness {
    fn to_bytes(self, value: i128, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..8].copy_from_slice(&bytes);
        &buf[..8]
    }
}

impl ToBytes<usize> for Endianness {
    fn to_bytes(self, value: usize, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };
        let len = std::mem::size_of::<usize>();

        buf[..len].copy_from_slice(&bytes);
        &buf[..len]
    }
}

impl ToBytes<isize> for Endianness {
    fn to_bytes(self, value: isize, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };
        let len = std::mem::size_of::<isize>();

        buf[..len].copy_from_slice(&bytes);
        &buf[..len]
    }
}

impl ToBytes<f32> for Endianness {
    fn to_bytes(self, value: f32, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..4].copy_from_slice(&bytes);
        &buf[..4]
    }
}

impl ToBytes<f64> for Endianness {
    fn to_bytes(self, value: f64, buf: &mut [u8; 16]) -> &[u8] {
        let bytes = match self {
            Endianness::Native => value.to_ne_bytes(),
            Endianness::Big => value.to_be_bytes(),
            Endianness::Little => value.to_le_bytes(),
        };

        buf[..8].copy_from_slice(&bytes);
        &buf[..8]
    }
}

impl ToBytes<Signed> for Endianness {
    fn to_bytes(self, value: Signed, buf: &mut [u8; 16]) -> &[u8] {
        let _ = self.to_bytes(value.value(), buf);
        let len = value.len();

        if is_little_endian() {
            &buf[..len]
        } else {
            &buf[16 - len..]
        }
    }
}

impl ToBytes<Unsigned> for Endianness {
    fn to_bytes(self, value: Unsigned, buf: &mut [u8; 16]) -> &[u8] {
        let _ = self.to_bytes(value.value(), buf);
        let len = value.len();

        if is_little_endian() {
            &buf[..len]
        } else {
            &buf[16 - len..]
        }
    }
}

pub(crate) trait FromBytes<T> {
    #[expect(clippy::wrong_self_convention)]
    fn from_bytes(self, buf: &[u8]) -> Option<T>;
}

impl FromBytes<u8> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<u8> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => u8::from_le_bytes(bytes),
            Endianness::Big => u8::from_be_bytes(bytes),
            Endianness::Native => u8::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<u16> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<u16> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => u16::from_le_bytes(bytes),
            Endianness::Big => u16::from_be_bytes(bytes),
            Endianness::Native => u16::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<u32> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<u32> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => u32::from_le_bytes(bytes),
            Endianness::Big => u32::from_be_bytes(bytes),
            Endianness::Native => u32::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<u64> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<u64> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => u64::from_le_bytes(bytes),
            Endianness::Big => u64::from_be_bytes(bytes),
            Endianness::Native => u64::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<u128> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<u128> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => u128::from_le_bytes(bytes),
            Endianness::Big => u128::from_be_bytes(bytes),
            Endianness::Native => u128::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<i8> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<i8> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => i8::from_le_bytes(bytes),
            Endianness::Big => i8::from_be_bytes(bytes),
            Endianness::Native => i8::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<i16> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<i16> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => i16::from_le_bytes(bytes),
            Endianness::Big => i16::from_be_bytes(bytes),
            Endianness::Native => i16::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<i32> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<i32> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => i32::from_le_bytes(bytes),
            Endianness::Big => i32::from_be_bytes(bytes),
            Endianness::Native => i32::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<i64> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<i64> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => i64::from_le_bytes(bytes),
            Endianness::Big => i64::from_be_bytes(bytes),
            Endianness::Native => i64::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<i128> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<i128> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => i128::from_le_bytes(bytes),
            Endianness::Big => i128::from_be_bytes(bytes),
            Endianness::Native => i128::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<usize> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<usize> {
        const LEN: usize = std::mem::size_of::<usize>();
        let bytes: [_; LEN] = buf.try_into().ok()?;

        let mut buf = [0; 8];
        buf[..LEN].copy_from_slice(&bytes);

        let r = match self {
            Endianness::Little => usize::from_le_bytes(buf),
            Endianness::Big => usize::from_be_bytes(buf),
            Endianness::Native => usize::from_ne_bytes(buf),
        };

        Some(r)
    }
}

impl FromBytes<isize> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<isize> {
        const LEN: usize = std::mem::size_of::<isize>();
        let bytes: [_; LEN] = buf.try_into().ok()?;

        let mut buf = [0; 8];
        buf[..LEN].copy_from_slice(&bytes);

        let r = match self {
            Endianness::Little => isize::from_le_bytes(buf),
            Endianness::Big => isize::from_be_bytes(buf),
            Endianness::Native => isize::from_ne_bytes(buf),
        };

        Some(r)
    }
}

impl FromBytes<f32> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<f32> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => f32::from_le_bytes(bytes),
            Endianness::Big => f32::from_be_bytes(bytes),
            Endianness::Native => f32::from_ne_bytes(bytes),
        };

        Some(r)
    }
}

impl FromBytes<f64> for Endianness {
    fn from_bytes(self, buf: &[u8]) -> Option<f64> {
        let bytes = buf.try_into().ok()?;

        let r = match self {
            Endianness::Little => f64::from_le_bytes(bytes),
            Endianness::Big => f64::from_be_bytes(bytes),
            Endianness::Native => f64::from_ne_bytes(bytes),
        };

        Some(r)
    }
}
