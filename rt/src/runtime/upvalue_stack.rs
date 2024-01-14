use std::ops::{Add, Deref, DerefMut};

use repr::index::UpvalueSlot;
use repr::tivec::{TiSlice, TiVec};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq)]
pub(crate) struct RawUpvalueSlot(usize);

impl Add<UpvalueSlot> for RawUpvalueSlot {
    type Output = Self;

    fn add(self, rhs: UpvalueSlot) -> Self::Output {
        let index = self.0 + rhs.0;
        RawUpvalueSlot(index)
    }
}

impl From<usize> for RawUpvalueSlot {
    fn from(value: usize) -> Self {
        RawUpvalueSlot(value)
    }
}

impl From<RawUpvalueSlot> for usize {
    fn from(value: RawUpvalueSlot) -> Self {
        value.0
    }
}

#[derive(Debug)]
pub(crate) struct UpvalueStack<Value> {
    stack: TiVec<RawUpvalueSlot, Value>,
}

impl<Value> UpvalueStack<Value> {
    pub(crate) fn view(&mut self) -> UpvalueStackView<Value> {
        UpvalueStackView::new(self)
    }

    fn next_slot(&self) -> RawUpvalueSlot {
        self.stack.next_key()
    }

    fn truncate(&mut self, slot: RawUpvalueSlot) {
        self.stack.truncate(slot.0)
    }
}

impl<Value> Default for UpvalueStack<Value> {
    fn default() -> Self {
        Self {
            stack: Default::default(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct UpvalueStackView<'a, Value> {
    stack: &'a mut UpvalueStack<Value>,
    boundary: RawUpvalueSlot,
}

impl<'a, Value> UpvalueStackView<'a, Value> {
    pub(crate) fn new(stack: &'a mut UpvalueStack<Value>) -> Self {
        UpvalueStackView {
            stack,
            boundary: RawUpvalueSlot(0),
        }
    }

    pub(crate) fn view(&mut self, boundary: RawUpvalueSlot) -> Option<UpvalueStackView<Value>> {
        if self.stack.next_slot() < boundary {
            return None;
        }

        let r = UpvalueStackView {
            stack: self.stack,
            boundary,
        };

        Some(r)
    }

    pub(crate) fn view_over(&mut self) -> UpvalueStackView<Value> {
        let boundary = self.stack.next_slot();

        UpvalueStackView {
            stack: self.stack,
            boundary,
        }
    }

    pub(crate) fn next_raw_slot(&self) -> RawUpvalueSlot {
        self.stack.next_slot()
    }

    pub(crate) fn clear(&mut self) {
        self.stack.truncate(self.boundary)
    }

    pub(crate) fn boundary(&self) -> RawUpvalueSlot {
        self.boundary
    }
}

impl<'a, Value> Extend<Value> for UpvalueStackView<'a, Value> {
    fn extend<T: IntoIterator<Item = Value>>(&mut self, iter: T) {
        self.stack.stack.extend(iter)
    }
}

impl<'a, Value> Deref for UpvalueStackView<'a, Value> {
    type Target = TiSlice<UpvalueSlot, Value>;

    fn deref(&self) -> &Self::Target {
        self.stack.stack[self.boundary..].raw.as_ref()
    }
}

impl<'a, Value> DerefMut for UpvalueStackView<'a, Value> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.stack.stack[self.boundary..].raw.as_mut()
    }
}
