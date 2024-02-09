use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::ops::{Add, Bound, Deref, DerefMut, RangeBounds};

use repr::index::StackSlot;
use repr::tivec::{TiSlice, TiVec};

use super::{Event, MapBound};
use crate::error::opcode::MissingArgsError;
use crate::value::{TypeProvider, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct RawStackSlot(usize);

impl RawStackSlot {
    // pub(crate) fn checked_sub(self, other: Self) -> Option<StackSlot> {
    //     self.0.checked_sub(other.0).map(StackSlot)
    // }
}

impl Add<StackSlot> for RawStackSlot {
    type Output = Self;

    fn add(self, rhs: StackSlot) -> Self::Output {
        RawStackSlot(self.0 + rhs.0)
    }
}

impl From<usize> for RawStackSlot {
    fn from(value: usize) -> Self {
        RawStackSlot(value)
    }
}

impl From<RawStackSlot> for usize {
    fn from(value: RawStackSlot) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct UpvalueId(usize);

impl UpvalueId {
    /// Increment and return current id.
    fn increment(&mut self) -> UpvalueId {
        let r = *self;
        self.0 += 1;
        r
    }
}

#[derive(Debug, Clone)]
enum UpvaluePlace<Value> {
    Stack(RawStackSlot),
    Place(Value),
}

/// Backing storage for stack of temporaries.
#[derive(Debug, Clone)]
pub struct Stack<Value> {
    /// Values contained on the stack.
    temporaries: TiVec<RawStackSlot, Value>,

    /// Indices of upvalues which are currently being hosted on stack.
    ///
    /// Note that upvalue represent *a place* where value is stored, not value itself.
    /// Any removals/insertions *must* update all upvalues hosted above modification point
    /// since any existing upvalue id id going to point to wrong place.
    on_stack_upvalues: BTreeMap<RawStackSlot, UpvalueId>,

    /// Upvalues that were evicted from the stack.
    evicted_upvalues: HashMap<UpvalueId, UpvaluePlace<Value>>,

    /// Upvalue id counter.
    next_upvalue_id: UpvalueId,
}

impl<Value> Stack<Value> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn view(&mut self) -> StackView<Value> {
        StackView::new(self)
    }

    fn push(&mut self, value: Value) -> RawStackSlot {
        self.temporaries.push_and_get_key(value)
    }

    fn get(&self, slot: RawStackSlot) -> Option<&Value> {
        self.temporaries.get(slot)
    }

    fn get_mut(&mut self, slot: RawStackSlot) -> Option<&mut Value> {
        self.temporaries.get_mut(slot)
    }

    /// Create a new upvalue.
    ///
    /// Resulting upvalue is unique,
    /// e.g. place it points to is not shared with any other existing upvalue.
    fn fresh_upvalue(&mut self, value: Value) -> UpvalueId {
        let id = self.next_upvalue_id.increment();

        self.evicted_upvalues.insert(id, UpvaluePlace::Place(value));

        id
    }

    fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&Value> {
        let value = match self.evicted_upvalues.get(&upvalue)? {
            UpvaluePlace::Stack(slot) => self
                .temporaries
                .get(*slot)
                .expect("on-stack upvalue should point to an existing stack slot"),
            UpvaluePlace::Place(value) => value,
        };

        Some(value)
    }

    fn get_upvalue_mut(&mut self, upvalue: UpvalueId) -> Option<&mut Value> {
        let value = match self.evicted_upvalues.get_mut(&upvalue)? {
            UpvaluePlace::Stack(slot) => self
                .temporaries
                .get_mut(*slot)
                .expect("on-stack upvalue should point to an existing stack slot"),
            UpvaluePlace::Place(value) => value,
        };

        Some(value)
    }

    fn mark_as_upvalue(&mut self, slot: RawStackSlot) -> Option<UpvalueId> {
        if slot.0 >= self.len() {
            return None;
        }

        let upvalue_id = *self.on_stack_upvalues.entry(slot).or_insert_with(|| {
            let id = self.next_upvalue_id.increment();
            self.evicted_upvalues.insert(id, UpvaluePlace::Stack(slot));
            id
        });

        Some(upvalue_id)
    }

    fn len(&self) -> usize {
        self.temporaries.len()
    }

    fn insert(&mut self, slot: RawStackSlot, value: Value) {
        // Construct tail iterator before any modifications.
        let tail = (slot.0..self.temporaries.len())
            .rev()
            .map(|i| (RawStackSlot(i), RawStackSlot(i + 1)));

        self.temporaries.insert(slot, value);

        // Shift slots for on-stack upvalues above insertion point.
        self.reassociate(tail);
    }

    fn remove_range(&mut self, range: impl RangeBounds<RawStackSlot>) {
        let (start, end) = self.transform_range(range);

        let len = end
            .0
            .checked_sub(start.0)
            .expect("starting point of range should not be grater than ending point");

        let tail_start = end;
        let tail_end = self.temporaries.next_key();

        // Construct tail iterator before any modifications.
        let tail = (tail_start.0..tail_end.0).map(|i| (RawStackSlot(i), RawStackSlot(i - len)));

        // Removing happens in two steps in this order.
        // First, we need to ensure that upvalues inside removal region are correctly dissoaciated.
        self.drain_into_upvalues(start..end);

        // Second, we need to adjust any upvalues that reside above removal region
        // since their stack slots will get shifted.
        self.reassociate(tail);
    }

    /// Reassociate upvalue ids using `(old_slot, new_slot)` pairs.
    ///
    /// Note that order might matter!
    /// If the `new_slot` overwrites over some future `old_slot` you will get unspecified behavior.
    fn reassociate(&mut self, iter: impl IntoIterator<Item = (RawStackSlot, RawStackSlot)>) {
        for (old_slot, new_slot) in iter.into_iter() {
            let Some(upvalue_id) = self.on_stack_upvalues.remove(&old_slot) else {
                continue;
            };

            let assoc_slot = match self.evicted_upvalues.get_mut(&upvalue_id) {
                Some(UpvaluePlace::Stack(s)) if *s == old_slot => s,
                _ => {
                    unreachable!("on-stack upvalue should be correctly associated with stack slot")
                }
            };

            *assoc_slot = new_slot;
            self.on_stack_upvalues.insert(new_slot, upvalue_id);
        }
    }

    /// Remove stack range and move values into associated upvalue places if any.
    ///
    /// This function **does not adjust portion of the stack above removal point**.
    /// As such it is your responsibility to reinforce invariants after calling this function.
    fn drain_into_upvalues(&mut self, range: impl RangeBounds<RawStackSlot>) {
        let (start, end) = self.transform_range(range);

        let iter = self
            .temporaries
            .drain(start..end)
            .zip((start.0..end.0).map(RawStackSlot));

        for (value, slot) in iter {
            let Some(upvalue_id) = self.on_stack_upvalues.remove(&slot) else {
                continue;
            };

            let place = self
                .evicted_upvalues
                .get_mut(&upvalue_id)
                .expect("registered upvalues should have their place allocated at creation");

            assert!(matches!(place, UpvaluePlace::Stack(s) if *s == slot));

            *place = UpvaluePlace::Place(value);
        }
    }

    fn truncate(&mut self, slot: RawStackSlot) {
        self.remove_range(slot..)
    }

    fn transform_range(
        &self,
        range: impl RangeBounds<RawStackSlot>,
    ) -> (RawStackSlot, RawStackSlot) {
        let start = match range.start_bound().cloned() {
            Bound::Excluded(RawStackSlot(slot)) => RawStackSlot(slot + 1),
            Bound::Included(slot) => slot,
            Bound::Unbounded => RawStackSlot(0),
        };
        let end = match range.end_bound().cloned() {
            Bound::Excluded(slot) => slot,
            Bound::Included(RawStackSlot(slot)) => RawStackSlot(slot + 1),
            Bound::Unbounded => self.temporaries.next_key(),
        };

        (start, end)
    }
}

impl<Value> Stack<Value>
where
    Value: Clone,
{
    fn remove(&mut self, slot: RawStackSlot) -> Option<Value> {
        if slot < self.temporaries.next_key() {
            let r = self.temporaries.get(slot).cloned();
            self.remove_range(slot..=slot);
            r
        } else {
            None
        }
    }

    fn pop(&mut self) -> Option<Value> {
        let r = self.temporaries.last().cloned();

        if let Some(id) = self.temporaries.last_key() {
            self.truncate(id);
        }

        r
    }
}

impl<Value> Stack<Value>
where
    Value: Default + Clone,
{
    fn adjust_height_with_variadics(&mut self, height: RawStackSlot) -> Vec<Value> {
        match height.0.checked_sub(self.temporaries.len()) {
            None => {
                let r = self.temporaries[height..].to_vec().into();
                self.truncate(height);
                r
            }
            Some(0) => Default::default(),
            Some(n) => {
                self.temporaries
                    .extend(std::iter::repeat_with(|| Default::default()).take(n));
                Default::default()
            }
        }
    }
}

impl<Value> Default for Stack<Value> {
    fn default() -> Self {
        Self {
            temporaries: Default::default(),
            on_stack_upvalues: Default::default(),
            evicted_upvalues: Default::default(),
            next_upvalue_id: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct StackView<'a, Value> {
    stack: &'a mut Stack<Value>,
    boundary: RawStackSlot,
}

impl<'a, Value> StackView<'a, Value> {
    pub fn new(stack: &'a mut Stack<Value>) -> Self {
        StackView {
            stack,
            boundary: RawStackSlot(0),
        }
    }

    pub(crate) fn view(&mut self, protected_size: RawStackSlot) -> Option<StackView<'_, Value>> {
        if self.stack.len() < protected_size.0 {
            return None;
        }

        let r = StackView {
            stack: self.stack,
            boundary: protected_size,
        };

        Some(r)
    }

    pub fn as_slice(&self) -> &TiSlice<StackSlot, Value> {
        self.deref()
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn len(&self) -> usize {
        self.stack.len() - self.boundary().0
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn next_slot(&self) -> StackSlot {
        let val = self.stack.len() - self.boundary.0;
        StackSlot(val)
    }

    pub fn get(&self, slot: StackSlot) -> Option<&Value> {
        let index = self.boundary + slot;
        self.stack.get(index)
    }

    pub fn get_mut(&mut self, slot: StackSlot) -> Option<&mut Value> {
        let index = self.boundary + slot;
        self.stack.get_mut(index)
    }

    pub fn fresh_upvalue(&mut self, value: Value) -> UpvalueId {
        self.stack.fresh_upvalue(value)
    }

    pub fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&Value> {
        self.stack.get_upvalue(upvalue)
    }

    pub fn get_upvalue_mut(&mut self, upvalue: UpvalueId) -> Option<&mut Value> {
        self.stack.get_upvalue_mut(upvalue)
    }

    pub fn mark_as_upvalue(&mut self, slot: StackSlot) -> Option<UpvalueId> {
        let slot = self.boundary + slot;
        self.stack.mark_as_upvalue(slot)
    }

    pub fn insert(&mut self, slot: StackSlot, value: Value) {
        let slot = self.boundary + slot;
        self.stack.insert(slot, value)
    }

    pub fn remove_range(&mut self, range: impl RangeBounds<StackSlot>) {
        let start = range.start_bound().mapb(|slot| self.boundary + *slot);
        let end = range.end_bound().mapb(|slot| self.boundary + *slot);

        self.stack.remove_range((start, end))
    }

    pub fn truncate(&mut self, new_len: StackSlot) {
        let new_len = self.boundary + new_len;
        self.stack.truncate(new_len)
    }

    pub fn clear(&mut self) {
        self.truncate(StackSlot(0))
    }

    pub(crate) fn boundary(&self) -> RawStackSlot {
        self.boundary
    }

    pub(crate) fn evicted_upvalue_iter(&self) -> impl Iterator<Item = &Value> {
        self.stack
            .evicted_upvalues
            .values()
            .flat_map(|place| match place {
                UpvaluePlace::Place(value) => Some(value),
                UpvaluePlace::Stack(_) => None,
            })
    }
}

impl<'a, Value> StackView<'a, Value>
where
    Value: Clone,
{
    pub(crate) fn take1(&mut self) -> Result<[Value; 1], MissingArgsError> {
        let v0 = self.pop().ok_or_else(|| MissingArgsError {
            stack_len: self.stack.len(),
            expected_args: 1,
        })?;
        Ok([v0])
    }

    pub(crate) fn take2(&mut self) -> Result<[Value; 2], MissingArgsError> {
        if self.len() < 2 {
            return Err(MissingArgsError {
                expected_args: 2,
                stack_len: self.len(),
            });
        }

        let v1 = self.pop().unwrap();
        let v0 = self.pop().unwrap();

        Ok([v0, v1])
    }

    pub(crate) fn take3(&mut self) -> Result<[Value; 3], MissingArgsError> {
        if self.len() < 3 {
            return Err(MissingArgsError {
                expected_args: 3,
                stack_len: self.len(),
            });
        }

        let v2 = self.pop().unwrap();
        let v1 = self.pop().unwrap();
        let v0 = self.pop().unwrap();

        Ok([v0, v1, v2])
    }

    pub fn pop(&mut self) -> Option<Value> {
        if self.stack.len() <= self.boundary.0 {
            return None;
        }

        self.stack.pop()
    }

    pub fn remove(&mut self, slot: StackSlot) -> Option<Value> {
        let slot = self.boundary + slot;
        self.stack.remove(slot)
    }
}

impl<'a, Value> StackView<'a, Value>
where
    Value: Default + Clone,
{
    /// Set stack to specified height.
    ///
    /// If `len < height` it will get filled with default values.
    /// If `len > height` extra values will be removed.
    ///
    /// If you want to obtain extraneous values use
    /// [`adjust_height_and_collect`](Self::adjust_height_and_collect).
    pub fn adjust_height(&mut self, height: StackSlot) {
        self.adjust_height_and_collect(height);
    }

    /// Set stack to specified height and obtain extraneous values if any.
    ///
    /// If `len < height` it will get filled with default values.
    /// If `len > height` extra values will be removed from the stack and returned.
    /// Otherwise function returns empty vec.
    pub fn adjust_height_and_collect(&mut self, height: StackSlot) -> Vec<Value> {
        let requested_height = self.boundary + height;

        self.stack.adjust_height_with_variadics(requested_height)
    }
}

impl<'a, Value> StackView<'a, Value>
where
    Value: Display,
{
    pub fn emit_pretty(&self, writer: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        if self.is_empty() {
            return write!(writer, "[]");
        }

        writeln!(writer, "[")?;
        for (slot, value) in self.iter_enumerated() {
            let upvalue_mark = if self
                .stack
                .on_stack_upvalues
                .contains_key(&(self.boundary + slot))
            {
                '*'
            } else {
                ' '
            };

            writeln!(writer, "    {upvalue_mark}[{slot:>2}] {value:#}")?;
        }

        writeln!(writer, "]")?;

        Ok(())
    }

    pub fn to_pretty_string(&self) -> String {
        let mut r = String::new();
        self.emit_pretty(&mut r).unwrap();
        r
    }
}

impl<'a, Gc> StackView<'a, Value<Gc>>
where
    Gc: TypeProvider,
    Value<Gc>: Clone,
{
    pub(crate) fn adjust_event_returns(&mut self, event: Event) {
        use Event::*;

        match event {
            // Ops resulting in single value.
            Add | Sub | Mul | Div | FloorDiv | Rem | Pow | BitAnd | BitOr | BitXor | ShL | ShR
            | Neg | BitNot | Len | Concat => {
                self.adjust_height(StackSlot(1));
            }
            // Ops resulting in single value + coercion to bool.
            Eq | Lt | LtEq => {
                self.adjust_height(StackSlot(1));
                let value = self.stack.pop().unwrap();
                self.push(Value::Bool(value.to_bool()));
            }
            // Not-equal additionally needs to inverse the resulting boolean.
            Neq => {
                self.adjust_height(StackSlot(1));
                let value = self.stack.pop().unwrap();
                self.push(Value::Bool(!value.to_bool()));
            }
            // Index getter results in single value.
            Index => {
                self.adjust_height(StackSlot(1));
            }
            // Index setter results in no values.
            NewIndex => {
                self.adjust_height(StackSlot(0));
            }
            // Calls don't adjust results.
            Call => (),
        }
    }
}

impl<'a, Value> Deref for StackView<'a, Value> {
    type Target = TiSlice<StackSlot, Value>;

    fn deref(&self) -> &Self::Target {
        self.stack.temporaries[self.boundary..].raw.as_ref()
    }
}

impl<'a, Value> DerefMut for StackView<'a, Value> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.stack.temporaries[self.boundary..].raw.as_mut()
    }
}

impl<'a, Value> Extend<Value> for StackView<'a, Value> {
    fn extend<T: IntoIterator<Item = Value>>(&mut self, iter: T) {
        self.stack.temporaries.extend(iter)
    }
}
