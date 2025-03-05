use super::ByteWidth;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Signed {
    value: i128,
    len: ByteWidth,
}

impl Signed {
    pub(crate) fn new(value: i128, len: ByteWidth) -> Option<Self> {
        let bits = len.into_inner() * 8;

        debug_assert!((1..=16).contains(&len.into_inner()));

        // For signed integers all truncated bits either:
        // * all contain 0
        // * all contain 1, and msb also 1

        let mask = -1_i128 << bits;
        let msb = 1_i128 << (bits - 1);

        let is_pos = value & mask == 0;
        let is_neg = value & mask == mask && value & msb != 0;

        if !(is_pos || is_neg) {
            return None;
        }

        let r = Signed { value, len };

        Some(r)
    }

    pub(crate) fn value(self) -> i128 {
        self.value
    }

    pub(crate) fn len(self) -> usize {
        self.len.into_inner().into()
    }

    pub(crate) fn size(self) -> ByteWidth {
        self.len
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Unsigned {
    value: u128,
    len: ByteWidth,
}

impl Unsigned {
    pub(crate) fn new(value: u128, len: ByteWidth) -> Option<Self> {
        let bits = len.into_inner() * 8;

        debug_assert!((1..=16).contains(&len.into_inner()));

        // For unsigned integers all truncated bits must contain 0.

        let mask = u128::MAX << bits;

        let in_bounds = value & mask == 0;

        if !in_bounds {
            return None;
        }

        let r = Unsigned { value, len };

        Some(r)
    }

    pub(crate) fn value(self) -> u128 {
        self.value
    }

    pub(crate) fn len(self) -> usize {
        self.len.into_inner().into()
    }

    pub(crate) fn size(self) -> ByteWidth {
        self.len
    }
}
