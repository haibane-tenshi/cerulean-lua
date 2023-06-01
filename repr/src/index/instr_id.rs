use std::ops::{Add, AddAssign, Sub, SubAssign};

use thiserror::Error;

use crate::index_vec::Index;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash)]
pub struct InstrId(pub u32);

#[derive(Debug, Error)]
#[error("instruction count overflowed u32")]
pub struct InstrCountError;

impl InstrId {
    pub fn checked_sub_offset(self, rhs: InstrOffset) -> Option<Self> {
        let val = self.0.checked_sub(rhs.0)?;
        Some(InstrId(val))
    }

    pub fn checked_sub(self, rhs: InstrId) -> Option<InstrOffset> {
        let offset = self.0.checked_sub(rhs.0)?;
        Some(InstrOffset(offset))
    }
}

impl Index for InstrId {
    type Error = InstrCountError;
    const MAX: Self = InstrId(u32::MAX);

    fn try_from(val: usize) -> Result<Self, Self::Error> {
        let inner = val.try_into().map_err(|_| InstrCountError)?;
        Ok(InstrId(inner))
    }

    fn into(self) -> usize {
        self.0.try_into().expect("u32 should fit into usize")
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct InstrOffset(pub u32);

impl AddAssign<u32> for InstrId {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl Add<u32> for InstrId {
    type Output = Self;

    fn add(mut self, rhs: u32) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign<InstrOffset> for InstrId {
    fn add_assign(&mut self, rhs: InstrOffset) {
        self.0 += rhs.0
    }
}

impl Add<InstrOffset> for InstrId {
    type Output = Self;

    fn add(mut self, rhs: InstrOffset) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<InstrOffset> for InstrId {
    fn sub_assign(&mut self, rhs: InstrOffset) {
        self.0 -= rhs.0
    }
}

impl Sub<InstrOffset> for InstrId {
    type Output = Self;

    fn sub(mut self, rhs: InstrOffset) -> Self::Output {
        self -= rhs;
        self
    }
}

impl Sub for InstrId {
    type Output = InstrOffset;

    fn sub(self, rhs: Self) -> Self::Output {
        let offset = self.0 - rhs.0;
        InstrOffset(offset)
    }
}

impl AddAssign<u32> for InstrOffset {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl Add<u32> for InstrOffset {
    type Output = Self;

    fn add(mut self, rhs: u32) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<u32> for InstrOffset {
    fn sub_assign(&mut self, rhs: u32) {
        self.0 -= rhs;
    }
}

impl Sub<u32> for InstrOffset {
    type Output = Self;

    fn sub(mut self, rhs: u32) -> Self::Output {
        self -= rhs;
        self
    }
}
