use std::fmt::Display;
use std::ops::{Add, AddAssign, Sub, SubAssign};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct ConstId(pub usize);

impl From<usize> for ConstId {
    fn from(value: usize) -> Self {
        ConstId(value)
    }
}

impl From<ConstId> for usize {
    fn from(value: ConstId) -> Self {
        value.0
    }
}

impl Display for ConstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct FunctionId(pub usize);

impl FunctionId {
    pub fn script() -> Self {
        FunctionId(0)
    }
}

impl From<usize> for FunctionId {
    fn from(value: usize) -> Self {
        FunctionId(value)
    }
}

impl From<FunctionId> for usize {
    fn from(value: FunctionId) -> Self {
        value.0
    }
}

impl Display for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash)]
pub struct InstrId(pub usize);

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

impl From<usize> for InstrId {
    fn from(value: usize) -> Self {
        InstrId(value)
    }
}

impl From<InstrId> for usize {
    fn from(value: InstrId) -> Self {
        value.0
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct InstrOffset(pub usize);

impl AddAssign<usize> for InstrId {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl Add<usize> for InstrId {
    type Output = Self;

    fn add(mut self, rhs: usize) -> Self::Output {
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

impl AddAssign<usize> for InstrOffset {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl Add<usize> for InstrOffset {
    type Output = Self;

    fn add(mut self, rhs: usize) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<usize> for InstrOffset {
    fn sub_assign(&mut self, rhs: usize) {
        self.0 -= rhs;
    }
}

impl Sub<usize> for InstrOffset {
    type Output = Self;

    fn sub(mut self, rhs: usize) -> Self::Output {
        self -= rhs;
        self
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash)]
pub struct StackSlot(pub usize);

impl From<usize> for StackSlot {
    fn from(value: usize) -> Self {
        StackSlot(value)
    }
}

impl From<StackSlot> for usize {
    fn from(value: StackSlot) -> Self {
        value.0
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct StackOffset(pub usize);

impl From<usize> for StackOffset {
    fn from(value: usize) -> Self {
        StackOffset(value)
    }
}

impl From<StackOffset> for usize {
    fn from(value: StackOffset) -> Self {
        value.0
    }
}

impl StackSlot {
    pub fn checked_sub(self, rhs: Self) -> Option<StackOffset> {
        let inner = self.0.checked_sub(rhs.0)?;
        Some(StackOffset(inner))
    }
}

impl Sub for StackSlot {
    type Output = StackOffset;

    fn sub(self, rhs: Self) -> Self::Output {
        self.checked_sub(rhs).unwrap()
    }
}

impl AddAssign<usize> for StackSlot {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl Add<usize> for StackSlot {
    type Output = Self;

    fn add(mut self, rhs: usize) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<StackOffset> for StackSlot {
    fn sub_assign(&mut self, rhs: StackOffset) {
        self.0 -= rhs.0;
    }
}

impl Sub<StackOffset> for StackSlot {
    type Output = Self;

    fn sub(mut self, rhs: StackOffset) -> Self::Output {
        self -= rhs;
        self
    }
}

impl AddAssign<StackOffset> for StackSlot {
    fn add_assign(&mut self, rhs: StackOffset) {
        self.0 += rhs.0;
    }
}

impl Add<StackOffset> for StackSlot {
    type Output = Self;

    fn add(mut self, rhs: StackOffset) -> Self::Output {
        self += rhs;
        self
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct UpvalueSlot(pub usize);

impl From<usize> for UpvalueSlot {
    fn from(value: usize) -> Self {
        UpvalueSlot(value)
    }
}

impl From<UpvalueSlot> for usize {
    fn from(value: UpvalueSlot) -> Self {
        value.0
    }
}
