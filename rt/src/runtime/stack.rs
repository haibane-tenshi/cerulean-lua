use std::ops::Add;

use repr::index::StackSlot;
use repr::value::Value;

use crate::RuntimeError;

#[derive(Debug, Clone, Copy)]
pub(crate) struct ProtectedSize(pub(crate) usize);

impl ProtectedSize {
    pub(crate) fn index(self, slot: StackSlot) -> usize {
        let offset: usize = slot
            .0
            .try_into()
            .expect("stack index should fit into usize");
        self.0 + offset
    }

    pub(crate) fn slot(self, index: usize) -> Option<StackSlot> {
        let offset = index.checked_sub(self.0)?;
        let offset = offset.try_into().expect("stack offset should fit into u32");

        Some(StackSlot(offset))
    }
}

impl Add<usize> for ProtectedSize {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        ProtectedSize(self.0 + rhs)
    }
}

impl Add<StackSlot> for ProtectedSize {
    type Output = Self;

    fn add(self, rhs: StackSlot) -> Self::Output {
        let index = self.index(rhs);
        ProtectedSize(index)
    }
}

#[derive(Debug)]
pub struct StackView<'a> {
    stack: &'a mut Vec<Value>,
    protected_size: ProtectedSize,
}

impl<'a> StackView<'a> {
    pub(crate) fn new(stack: &'a mut Vec<Value>) -> Self {
        StackView {
            stack,
            protected_size: ProtectedSize(0),
        }
    }

    pub(crate) fn view(&mut self, protected_size: ProtectedSize) -> Option<StackView> {
        if self.stack.len() < protected_size.0 {
            return None;
        }

        let r = StackView {
            stack: self.stack,
            protected_size,
        };

        Some(r)
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    pub fn pop(&mut self) -> Result<Value, RuntimeError> {
        if self.stack.len() <= self.protected_size.0 {
            return Err(RuntimeError);
        }

        self.stack.pop().ok_or(RuntimeError)
    }

    pub fn last(&self) -> Result<&Value, RuntimeError> {
        self.stack.last().ok_or(RuntimeError)
    }

    pub fn top(&mut self) -> StackSlot {
        self.protected_size.slot(self.stack.len()).unwrap()
    }

    pub fn get(&self, slot: StackSlot) -> Result<&Value, RuntimeError> {
        let index = self.protected_size.index(slot);
        self.stack.get(index).ok_or(RuntimeError)
    }

    pub fn get_mut(&mut self, slot: StackSlot) -> Result<&mut Value, RuntimeError> {
        let index = self.protected_size.index(slot);
        self.stack.get_mut(index).ok_or(RuntimeError)
    }

    pub(crate) fn protected_size(&self) -> ProtectedSize {
        self.protected_size
    }

    pub fn adjust_height(&mut self, height: StackSlot) {
        let requested_height = self.protected_size.index(height);

        match requested_height.checked_sub(self.stack.len()) {
            None => self.stack.truncate(requested_height),
            Some(0) => (),
            Some(n) => self.stack.extend(std::iter::repeat(Value::Nil).take(n)),
        }
    }

    pub fn drop_under(&mut self, slot: StackSlot) -> Result<(), RuntimeError> {
        let height = self.protected_size.index(slot);
        self.stack.drain(self.protected_size.0..height);

        Ok(())
    }
}
