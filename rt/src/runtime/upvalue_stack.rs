use std::ops::Add;

use repr::index::UpvalueSlot;
use repr::tivec::TiVec;

use super::Value;

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
pub(crate) struct UpvalueStack<C> {
    stack: TiVec<RawUpvalueSlot, Value<C>>,
}

impl<C> UpvalueStack<C> {
    pub(crate) fn view(&mut self) -> UpvalueStackView<C> {
        UpvalueStackView::new(self)
    }

    fn next_slot(&self) -> RawUpvalueSlot {
        self.stack.next_key()
    }

    fn get(&self, slot: RawUpvalueSlot) -> Option<&Value<C>> {
        self.stack.get(slot)
    }

    fn get_mut(&mut self, slot: RawUpvalueSlot) -> Option<&mut Value<C>> {
        self.stack.get_mut(slot)
    }

    fn truncate(&mut self, slot: RawUpvalueSlot) {
        self.stack.truncate(slot.0)
    }

    fn len(&self) -> usize {
        self.stack.len()
    }
}

impl<C> Default for UpvalueStack<C> {
    fn default() -> Self {
        Self {
            stack: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct UpvalueStackView<'a, C> {
    stack: &'a mut UpvalueStack<C>,
    boundary: RawUpvalueSlot,
}

impl<'a, C> UpvalueStackView<'a, C> {
    pub(crate) fn new(stack: &'a mut UpvalueStack<C>) -> Self {
        UpvalueStackView {
            stack,
            boundary: RawUpvalueSlot(0),
        }
    }

    pub(crate) fn view(&mut self, boundary: RawUpvalueSlot) -> Option<UpvalueStackView<C>> {
        if self.stack.next_slot() < boundary {
            return None;
        }

        let r = UpvalueStackView {
            stack: self.stack,
            boundary,
        };

        Some(r)
    }

    pub(crate) fn view_over(&mut self) -> UpvalueStackView<C> {
        let boundary = self.stack.next_slot();

        UpvalueStackView {
            stack: self.stack,
            boundary,
        }
    }

    pub(crate) fn next_raw_slot(&self) -> RawUpvalueSlot {
        self.stack.next_slot()
    }

    pub(crate) fn get(&self, slot: UpvalueSlot) -> Option<&Value<C>> {
        let index = self.boundary + slot;
        self.stack.get(index)
    }

    pub(crate) fn get_mut(&mut self, slot: UpvalueSlot) -> Option<&mut Value<C>> {
        let index = self.boundary + slot;
        self.stack.get_mut(index)
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<Value<C>> {
        self.stack.stack.get(self.boundary..).unwrap().iter()
    }

    pub(crate) fn iter_mut(&mut self) -> std::slice::IterMut<Value<C>> {
        self.stack
            .stack
            .get_mut(self.boundary..)
            .unwrap()
            .iter_mut()
    }

    pub(crate) fn clear(&mut self) {
        self.stack.truncate(self.boundary)
    }

    pub(crate) fn len(&self) -> usize {
        self.stack.len() - self.boundary.0
    }

    pub(crate) fn boundary(&self) -> RawUpvalueSlot {
        self.boundary
    }
}

impl<'a, C> Extend<Value<C>> for UpvalueStackView<'a, C> {
    fn extend<T: IntoIterator<Item = Value<C>>>(&mut self, iter: T) {
        self.stack.stack.extend(iter)
    }
}
