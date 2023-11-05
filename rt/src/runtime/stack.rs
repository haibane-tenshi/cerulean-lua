use std::collections::HashMap;
use std::ops::{Add, Range};

use repr::index::StackSlot;

use super::Value;
use crate::RuntimeError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RawStackSlot(pub(crate) usize);

impl RawStackSlot {
    pub(crate) fn index(self, slot: StackSlot) -> RawStackSlot {
        RawStackSlot(self.0 + slot.0)
    }

    pub(crate) fn slot(self, index: RawStackSlot) -> Option<StackSlot> {
        let offset = index.0.checked_sub(self.0)?;

        Some(StackSlot(offset))
    }
}

impl Add<usize> for RawStackSlot {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        RawStackSlot(self.0 + rhs)
    }
}

impl Add<StackSlot> for RawStackSlot {
    type Output = Self;

    fn add(self, rhs: StackSlot) -> Self::Output {
        self.index(rhs)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct RawUpvalueId(u32);

impl RawUpvalueId {
    fn increment(&mut self) -> RawUpvalueId {
        let r = *self;
        self.0 += 1;
        r
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UpvalueId(RawUpvalueId, RawStackSlot);

#[derive(Debug, Default)]
pub struct Stack {
    temporaries: Vec<Value>,
    on_stack_upvalues: HashMap<RawStackSlot, RawUpvalueId>,
    evicted_upvalues: HashMap<RawUpvalueId, Value>,
    next_upvalue_id: RawUpvalueId,
}

impl Stack {
    // pub fn new() -> Self {
    //     Default::default()
    // }

    pub(crate) fn push(&mut self, value: Value) -> RawStackSlot {
        let slot = self.temporaries.len();
        self.temporaries.push(value);

        RawStackSlot(slot)
    }

    pub(crate) fn pop(&mut self) -> Option<Value> {
        let r = self.temporaries.pop();
        if let Some(value) = &r {
            let slot = RawStackSlot(self.temporaries.len());
            if let Some(upvalue_id) = self.on_stack_upvalues.remove(&slot) {
                self.evicted_upvalues.insert(upvalue_id, value.clone());
            }
        }

        r
    }

    pub(crate) fn adjust_height_with_variadics(&mut self, height: RawStackSlot) -> Vec<Value> {
        match height.0.checked_sub(self.temporaries.len()) {
            None => {
                for slot in (height.0..self.len().0).map(RawStackSlot) {
                    if let Some(upvalue_id) = self.on_stack_upvalues.remove(&slot) {
                        let value = self.get(slot).unwrap().clone();
                        self.evicted_upvalues.insert(upvalue_id, value);
                    }
                }

                self.temporaries.split_off(height.0)
            }
            Some(0) => Default::default(),
            Some(n) => {
                self.temporaries
                    .extend(std::iter::repeat(Value::Nil).take(n));
                Default::default()
            }
        }
    }

    pub(crate) fn drop_range(&mut self, range: Range<RawStackSlot>) -> Result<(), RuntimeError> {
        let range = range.start.0..range.end.0;
        for (value, slot) in self
            .temporaries
            .drain(range.clone())
            .zip(range.map(RawStackSlot))
        {
            if let Some(upvalue_id) = self.on_stack_upvalues.remove(&slot) {
                self.evicted_upvalues.insert(upvalue_id, value);
            }
        }

        Ok(())
    }

    pub(crate) fn get(&self, slot: RawStackSlot) -> Option<&Value> {
        self.temporaries.get(slot.0)
    }

    pub(crate) fn get_mut(&mut self, slot: RawStackSlot) -> Option<&mut Value> {
        self.temporaries.get_mut(slot.0)
    }

    pub fn last(&self) -> Option<&Value> {
        self.temporaries.last()
    }

    pub fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&Value> {
        self.evicted_upvalues
            .get(&upvalue.0)
            .or_else(|| self.get(upvalue.1))
    }

    pub fn get_upvalue_mut(&mut self, upvalue: UpvalueId) -> Option<&mut Value> {
        self.evicted_upvalues
            .get_mut(&upvalue.0)
            .or_else(|| self.temporaries.get_mut(upvalue.1 .0))
    }

    pub(crate) fn mark_as_upvalue(&mut self, slot: RawStackSlot) -> Option<UpvalueId> {
        if slot.0 >= self.len().0 {
            return None;
        }

        let upvalue_id = *self
            .on_stack_upvalues
            .entry(slot)
            .or_insert_with(|| self.next_upvalue_id.increment());

        Some(UpvalueId(upvalue_id, slot))
    }

    pub(crate) fn len(&self) -> RawStackSlot {
        RawStackSlot(self.temporaries.len())
    }
}

#[derive(Debug)]
pub struct StackView<'a> {
    stack: &'a mut Stack,
    protected_size: RawStackSlot,
}

impl<'a> StackView<'a> {
    pub(crate) fn new(stack: &'a mut Stack) -> Self {
        StackView {
            stack,
            protected_size: RawStackSlot(0),
        }
    }

    pub(crate) fn view(&mut self, protected_size: RawStackSlot) -> Option<StackView> {
        if self.stack.len().0 < protected_size.0 {
            return None;
        }

        let r = StackView {
            stack: self.stack,
            protected_size,
        };

        Some(r)
    }

    pub(crate) fn view_over(&mut self) -> StackView {
        let protected_size = self.stack.len();

        StackView {
            stack: self.stack,
            protected_size,
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Result<Value, RuntimeError> {
        if self.stack.len().0 <= self.protected_size.0 {
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

    pub fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&Value> {
        self.stack.get_upvalue(upvalue)
    }

    pub fn get_upvalue_mut(&mut self, upvalue: UpvalueId) -> Option<&mut Value> {
        self.stack.get_upvalue_mut(upvalue)
    }

    pub fn mark_as_upvalue(&mut self, slot: StackSlot) -> Result<UpvalueId, RuntimeError> {
        let slot = self.protected_size.index(slot);
        self.stack.mark_as_upvalue(slot).ok_or(RuntimeError)
    }

    pub(crate) fn protected_size(&self) -> RawStackSlot {
        self.protected_size
    }

    pub fn adjust_height(&mut self, height: StackSlot) {
        self.adjust_height_with_variadics(height);
    }

    pub fn adjust_height_with_variadics(&mut self, height: StackSlot) -> Vec<Value> {
        let requested_height = self.protected_size.index(height);

        self.stack.adjust_height_with_variadics(requested_height)
    }

    pub fn drop_under(&mut self, slot: StackSlot) -> Result<(), RuntimeError> {
        let height = self.protected_size.index(slot);
        self.stack.drop_range(self.protected_size..height)?;

        Ok(())
    }
}

impl<'a> Extend<Value> for StackView<'a> {
    fn extend<T: IntoIterator<Item = Value>>(&mut self, iter: T) {
        self.stack.temporaries.extend(iter)
    }
}
