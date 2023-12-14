use std::ops::Add;

use repr::index::UpvalueSlot;

use super::Value;

#[derive(Debug, Clone, Copy)]
pub(crate) struct ProtectedSize(pub(crate) usize);

impl ProtectedSize {
    pub(crate) fn index(self, slot: UpvalueSlot) -> usize {
        self.0 + slot.0
    }

    // pub(crate) fn slot(self, index: usize) -> Option<UpvalueSlot> {
    //     let offset = index.checked_sub(self.0)?;
    //
    //     Some(UpvalueSlot(offset))
    // }
}

impl Add<usize> for ProtectedSize {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        ProtectedSize(self.0 + rhs)
    }
}

impl Add<UpvalueSlot> for ProtectedSize {
    type Output = Self;

    fn add(self, rhs: UpvalueSlot) -> Self::Output {
        let index = self.index(rhs);
        ProtectedSize(index)
    }
}

#[derive(Debug)]
pub struct UpvalueView<'a, C> {
    stack: &'a mut Vec<Value<C>>,
    protected_size: ProtectedSize,
}

impl<'a, C> UpvalueView<'a, C> {
    pub(crate) fn new(stack: &'a mut Vec<Value<C>>) -> Self {
        UpvalueView {
            stack,
            protected_size: ProtectedSize(0),
        }
    }

    pub(crate) fn view(&mut self, protected_size: ProtectedSize) -> Option<UpvalueView<C>> {
        if self.stack.len() < protected_size.0 {
            return None;
        }

        let r = UpvalueView {
            stack: self.stack,
            protected_size,
        };

        Some(r)
    }

    pub(crate) fn view_over(&mut self) -> UpvalueView<C> {
        let protected_size = ProtectedSize(self.stack.len());

        UpvalueView {
            stack: self.stack,
            protected_size,
        }
    }

    pub fn get(&self, slot: UpvalueSlot) -> Option<&Value<C>> {
        let index = self.protected_size.index(slot);
        self.stack.get(index)
    }

    pub fn get_mut(&mut self, slot: UpvalueSlot) -> Option<&mut Value<C>> {
        let index = self.protected_size.index(slot);
        self.stack.get_mut(index)
    }

    pub fn iter(&self) -> std::slice::Iter<Value<C>> {
        self.stack.get(self.protected_size.0..).unwrap().iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<Value<C>> {
        self.stack
            .get_mut(self.protected_size.0..)
            .unwrap()
            .iter_mut()
    }

    pub fn clear(self) {
        self.stack.truncate(self.protected_size.0)
    }

    pub fn len(&self) -> usize {
        self.stack.len() - self.protected_size.0
    }

    pub(crate) fn protected_size(&self) -> ProtectedSize {
        self.protected_size
    }
}

impl<'a, C> Extend<Value<C>> for UpvalueView<'a, C> {
    fn extend<T: IntoIterator<Item = Value<C>>>(&mut self, iter: T) {
        self.stack.extend(iter)
    }
}
