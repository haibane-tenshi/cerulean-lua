use std::collections::HashMap;
use std::fmt::Debug;
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

pub struct Stack<C> {
    temporaries: Vec<Value<C>>,
    on_stack_upvalues: HashMap<RawStackSlot, RawUpvalueId>,
    evicted_upvalues: HashMap<RawUpvalueId, Value<C>>,
    next_upvalue_id: RawUpvalueId,
}

impl<C> Stack<C> {
    // pub fn new() -> Self {
    //     Default::default()
    // }

    pub(crate) fn push(&mut self, value: Value<C>) -> RawStackSlot {
        let slot = self.temporaries.len();
        self.temporaries.push(value);

        RawStackSlot(slot)
    }

    pub(crate) fn pop(&mut self) -> Option<Value<C>> {
        let r = self.temporaries.pop();
        if let Some(value) = &r {
            let slot = RawStackSlot(self.temporaries.len());
            if let Some(upvalue_id) = self.on_stack_upvalues.remove(&slot) {
                self.evicted_upvalues.insert(upvalue_id, value.clone());
            }
        }

        r
    }

    pub(crate) fn adjust_height_with_variadics(&mut self, height: RawStackSlot) -> Vec<Value<C>> {
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

    pub(crate) fn get(&self, slot: RawStackSlot) -> Option<&Value<C>> {
        self.temporaries.get(slot.0)
    }

    pub(crate) fn get_mut(&mut self, slot: RawStackSlot) -> Option<&mut Value<C>> {
        self.temporaries.get_mut(slot.0)
    }

    pub fn last(&self) -> Option<&Value<C>> {
        self.temporaries.last()
    }

    pub fn fresh_upvalue(&mut self, value: Value<C>) -> UpvalueId {
        let raw_id = self.next_upvalue_id.increment();

        self.evicted_upvalues.insert(raw_id, value);

        // Provide a dummy stack slot.
        // Ideally I would like to wrap it in Option and use None here,
        // but creating niche on RawStackSlot is pain.
        // Maybe some other day.
        // We never remove upvalues so this works for now.
        UpvalueId(raw_id, RawStackSlot(0))
    }

    pub fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&Value<C>> {
        self.evicted_upvalues
            .get(&upvalue.0)
            .or_else(|| self.get(upvalue.1))
    }

    pub fn get_upvalue_mut(&mut self, upvalue: UpvalueId) -> Option<&mut Value<C>> {
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

impl<C> Debug for Stack<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Stack")
            .field("temporaries", &self.temporaries)
            .field("on_stack_upvalues", &self.on_stack_upvalues)
            .field("evicted_upvalues", &self.evicted_upvalues)
            .field("next_upvalue_id", &self.next_upvalue_id)
            .finish()
    }
}

impl<C> Default for Stack<C> {
    fn default() -> Self {
        Self {
            temporaries: Default::default(),
            on_stack_upvalues: Default::default(),
            evicted_upvalues: Default::default(),
            next_upvalue_id: Default::default(),
        }
    }
}

pub struct StackView<'a, C> {
    stack: &'a mut Stack<C>,
    protected_size: RawStackSlot,
}

impl<'a, C> StackView<'a, C> {
    pub(crate) fn new(stack: &'a mut Stack<C>) -> Self {
        StackView {
            stack,
            protected_size: RawStackSlot(0),
        }
    }

    pub(crate) fn view(&mut self, protected_size: RawStackSlot) -> Option<StackView<'_, C>> {
        if self.stack.len().0 < protected_size.0 {
            return None;
        }

        let r = StackView {
            stack: self.stack,
            protected_size,
        };

        Some(r)
    }

    pub fn push(&mut self, value: Value<C>) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Result<Value<C>, RuntimeError> {
        if self.stack.len().0 <= self.protected_size.0 {
            return Err(RuntimeError);
        }

        self.stack.pop().ok_or(RuntimeError)
    }

    pub fn last(&self) -> Result<&Value<C>, RuntimeError> {
        self.stack.last().ok_or(RuntimeError)
    }

    pub fn top(&mut self) -> StackSlot {
        self.protected_size.slot(self.stack.len()).unwrap()
    }

    pub fn get(&self, slot: StackSlot) -> Result<&Value<C>, RuntimeError> {
        let index = self.protected_size.index(slot);
        self.stack.get(index).ok_or(RuntimeError)
    }

    pub fn get_mut(&mut self, slot: StackSlot) -> Result<&mut Value<C>, RuntimeError> {
        let index = self.protected_size.index(slot);
        self.stack.get_mut(index).ok_or(RuntimeError)
    }

    pub fn fresh_upvalue(&mut self, value: Value<C>) -> UpvalueId {
        self.stack.fresh_upvalue(value)
    }

    pub fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&Value<C>> {
        self.stack.get_upvalue(upvalue)
    }

    pub fn get_upvalue_mut(&mut self, upvalue: UpvalueId) -> Option<&mut Value<C>> {
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

    pub fn adjust_height_with_variadics(&mut self, height: StackSlot) -> Vec<Value<C>> {
        let requested_height = self.protected_size.index(height);

        self.stack.adjust_height_with_variadics(requested_height)
    }

    pub fn drop_under(&mut self, slot: StackSlot) -> Result<(), RuntimeError> {
        let height = self.protected_size.index(slot);
        self.stack.drop_range(self.protected_size..height)?;

        Ok(())
    }
}

impl<'a, C> Extend<Value<C>> for StackView<'a, C> {
    fn extend<T: IntoIterator<Item = Value<C>>>(&mut self, iter: T) {
        self.stack.temporaries.extend(iter)
    }
}

impl<'a, C> Debug for StackView<'a, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StackView")
            .field("stack", &self.stack)
            .field("protected_size", &self.protected_size)
            .finish()
    }
}
