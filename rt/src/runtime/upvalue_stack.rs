use std::ops::Add;

use repr::index::UpvalueSlot;

use super::Value;
use crate::RuntimeError;

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
pub struct UpvalueView<'a> {
    stack: &'a mut Vec<Value>,
    protected_size: ProtectedSize,
}

impl<'a> UpvalueView<'a> {
    pub(crate) fn new(stack: &'a mut Vec<Value>) -> Self {
        UpvalueView {
            stack,
            protected_size: ProtectedSize(0),
        }
    }

    pub(crate) fn view(&mut self, protected_size: ProtectedSize) -> Option<UpvalueView> {
        if self.stack.len() < protected_size.0 {
            return None;
        }

        let r = UpvalueView {
            stack: self.stack,
            protected_size,
        };

        Some(r)
    }

    pub(crate) fn view_over(&mut self) -> UpvalueView {
        let protected_size = ProtectedSize(self.stack.len());

        UpvalueView {
            stack: self.stack,
            protected_size,
        }
    }

    pub fn get(&self, slot: UpvalueSlot) -> Result<&Value, RuntimeError> {
        let index = self.protected_size.index(slot);
        self.stack.get(index).ok_or(RuntimeError)
    }

    pub fn get_mut(&mut self, slot: UpvalueSlot) -> Result<&mut Value, RuntimeError> {
        let index = self.protected_size.index(slot);
        self.stack.get_mut(index).ok_or(RuntimeError)
    }

    pub fn iter(&self) -> std::slice::Iter<Value> {
        self.stack.get(self.protected_size.0..).unwrap().iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<Value> {
        self.stack
            .get_mut(self.protected_size.0..)
            .unwrap()
            .iter_mut()
    }

    pub fn clear(self) {
        self.stack.truncate(self.protected_size.0)
    }

    pub(crate) fn protected_size(&self) -> ProtectedSize {
        self.protected_size
    }
}

impl<'a> Extend<Value> for UpvalueView<'a> {
    fn extend<T: IntoIterator<Item = Value>>(&mut self, iter: T) {
        self.stack.extend(iter)
    }
}
