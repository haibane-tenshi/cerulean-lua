use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Display};
use std::ops::{Add, Bound, Deref, DerefMut, RangeBounds};

use gc::{Heap, Root};
use repr::index::StackSlot;
use repr::tivec::{TiSlice, TiVec};

use super::{Event, MapBound};
use crate::error::opcode::MissingArgsError;
use crate::value::{CoreTypes, StrongValue, Value, WeakValue};

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

pub(super) enum Source<T> {
    StackSlot(T),
    TrustedIsRooted(bool),
}

impl<T> Source<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Source<U> {
        match self {
            Source::StackSlot(t) => Source::StackSlot(f(t)),
            Source::TrustedIsRooted(t) => Source::TrustedIsRooted(t),
        }
    }
}

impl<T> Default for Source<T> {
    fn default() -> Self {
        Source::TrustedIsRooted(false)
    }
}

/// Backing storage for stack of temporaries.
pub struct Stack<Ty>
where
    Ty: CoreTypes,
{
    /// Values contained on the stack.
    ///
    /// Note that we operate with `WeakValue`s!
    /// Contained references can be gced without us knowing.
    main: TiVec<RawStackSlot, WeakValue<Ty>>,

    /// Rooted mirror of the stack.
    ///
    /// Contents of this vec should be syncronized with `main` field.
    /// Its purpose is to keep on-stack references alive.
    root: Root<Vec<WeakValue<Ty>>>,

    /// Index of first value known to be not in sync between `main` and `root`.
    sync_point: RawStackSlot,

    /// Index of first value known to be *not* rooted.
    ///
    /// It implies that all values below it are rooted.
    first_transient: Option<RawStackSlot>,

    /// Indices of upvalues which are currently being hosted on stack.
    ///
    /// Note that upvalue represent *a place* where value is stored, not value itself.
    /// Any removals/insertions *must* update all upvalues hosted above modification point
    /// since any existing upvalue id id going to point to wrong place.
    on_stack_upvalues: BTreeMap<RawStackSlot, UpvalueId>,

    /// Upvalues that were evicted from the stack.
    ///
    /// Those values are weak.
    /// As a transition measure to a different setup we ensure to drop roots
    /// to prevent those from being gced.
    evicted_upvalues: HashMap<UpvalueId, UpvaluePlace<WeakValue<Ty>>>,

    /// Upvalue id counter.
    next_upvalue_id: UpvalueId,
}

impl<Ty> Stack<Ty>
where
    Ty: CoreTypes,
{
    pub fn new(heap: &mut Heap) -> Self {
        Stack {
            main: Default::default(),
            root: heap.alloc(Default::default()),
            sync_point: RawStackSlot(0),
            first_transient: Default::default(),
            on_stack_upvalues: Default::default(),
            evicted_upvalues: Default::default(),
            next_upvalue_id: Default::default(),
        }
    }

    pub fn guard(&mut self) -> StackGuard<Ty> {
        StackGuard::new(self)
    }

    fn is_transient_source(&self, source: Source<RawStackSlot>) -> bool {
        match source {
            Source::StackSlot(slot) => self.first_transient.map(|tr| slot < tr).unwrap_or(true),
            Source::TrustedIsRooted(is_rooted) => !is_rooted,
        }
    }

    fn mark_transient(&mut self, slot: RawStackSlot) {
        let value = match self.first_transient {
            Some(tr) => tr.min(slot),
            None => slot,
        };

        self.first_transient = Some(value)
    }

    fn mark_unsync(&mut self, slot: RawStackSlot) {
        self.sync_point = self.sync_point.min(slot);
    }

    fn push(&mut self, value: WeakValue<Ty>, source: Source<RawStackSlot>) -> RawStackSlot {
        let slot = self.main.next_key();
        if value.is_transient() && self.is_transient_source(source) {
            self.mark_transient(slot);
        }
        self.mark_unsync(slot);

        self.main.push_and_get_key(value)
    }

    fn set(&mut self, slot: RawStackSlot, value: WeakValue<Ty>, source: Source<RawStackSlot>) {
        if value.is_transient() && self.is_transient_source(source) {
            self.mark_transient(slot);
        }
        self.mark_unsync(slot);
        if let Some(place) = self.main.get_mut(slot) {
            *place = value;
        }
    }

    /// Create a new upvalue.
    ///
    /// Resulting upvalue is unique,
    /// e.g. place it points to is not shared with any other existing upvalue.
    fn fresh_upvalue(&mut self, value: StrongValue<Ty>) -> UpvalueId {
        let id = self.next_upvalue_id.increment();

        let t = value.downgrade();
        self.evicted_upvalues.insert(id, UpvaluePlace::Place(t));

        // This will prevent value's references from being dropped.
        // This is a bit ugly, but works as a transition measure.
        std::mem::forget(value);

        id
    }

    fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&WeakValue<Ty>> {
        let value = match self.evicted_upvalues.get(&upvalue)? {
            UpvaluePlace::Stack(slot) => self
                .main
                .get(*slot)
                .expect("on-stack upvalue should point to an existing stack slot"),
            UpvaluePlace::Place(value) => value,
        };

        Some(value)
    }

    fn set_upvalue(
        &mut self,
        upvalue: UpvalueId,
        value: WeakValue<Ty>,
        source: Source<RawStackSlot>,
    ) {
        todo!()
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
        self.main.len()
    }

    fn insert(&mut self, slot: RawStackSlot, value: WeakValue<Ty>, source: Source<RawStackSlot>) {
        // Construct tail iterator before any modifications.
        let tail = (slot.0..self.main.len())
            .rev()
            .map(|i| (RawStackSlot(i), RawStackSlot(i + 1)));

        if value.is_transient() && self.is_transient_source(source) {
            self.mark_transient(slot);
        } else if let Some(tr) = &mut self.first_transient {
            // Non-transient insertion shifts the boundary up.
            tr.0 += 1;
        }
        self.mark_unsync(slot);

        self.main.insert(slot, value);

        // Shift slots for on-stack upvalues above insertion point.
        self.reassociate(tail);
    }

    fn remove_range(&mut self, range: impl RangeBounds<RawStackSlot>) {
        let (start, end) = self.transform_range(range);

        let len = end
            .0
            .checked_sub(start.0)
            .expect("starting point of range should not be grater than ending point");

        if len == 0 {
            return;
        }

        let tail_start = end;
        let tail_end = self.main.next_key();

        // Construct tail iterator before any modifications.
        let tail = (tail_start.0..tail_end.0).map(|i| (RawStackSlot(i), RawStackSlot(i - len)));

        // Removing happens in two steps in this order.
        // First, we need to ensure that upvalues inside removal region are correctly dissoaciated.
        self.drain_into_upvalues(start..end);

        // Second, we need to adjust any upvalues that reside above removal region
        // since their stack slots will get shifted.
        self.reassociate(tail);

        // Lastly adjust root bookkeeping.
        // We know range is non-empty here.
        self.mark_unsync(start);

        match &mut self.first_transient {
            None => (),
            Some(slot) if *slot < start => (),
            Some(slot) if end <= *slot => {
                slot.0 -= len;
            }
            // When transient marker points to somewhere inside removed range
            // we can only do some approximations.
            // Any values above marker can also be transient.
            Some(slot) => {
                if start < self.main.next_key() {
                    *slot = start;
                } else {
                    // But if there are no values above removal point left
                    // reset the marker.
                    self.first_transient = None;
                }
            }
        }
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
            .main
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
            Bound::Unbounded => self.main.next_key(),
        };

        (start, end)
    }

    fn sync(&mut self, heap: &mut Heap) {
        if self.sync_point == self.main.next_key() {
            return;
        }

        let mirror = &mut heap[&self.root];
        mirror.truncate(self.sync_point.0);
        mirror.extend(self.main[self.sync_point..].iter().cloned());

        self.sync_point = self.main.next_key();
        self.first_transient = None;
    }
}

impl<Ty> Stack<Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Clone,
{
    fn remove(&mut self, slot: RawStackSlot) -> Option<WeakValue<Ty>> {
        if slot < self.main.next_key() {
            let r = self.main.get(slot).cloned();
            self.remove_range(slot..=slot);
            r
        } else {
            None
        }
    }

    fn pop(&mut self) -> Option<WeakValue<Ty>> {
        let r = self.main.last().cloned();

        if let Some(id) = self.main.last_key() {
            self.truncate(id);
        }

        r
    }
}

impl<Ty> Stack<Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Default + Clone,
{
    fn adjust_height_with_variadics(&mut self, height: RawStackSlot) -> Vec<WeakValue<Ty>> {
        match height.0.checked_sub(self.main.len()) {
            None => {
                let r = self.main[height..].to_vec().into();
                self.truncate(height);
                r
            }
            Some(0) => Default::default(),
            Some(n) => {
                self.main
                    .extend(std::iter::repeat_with(Default::default).take(n));
                Default::default()
            }
        }
    }
}

impl<Ty> Debug for Stack<Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Stack")
            .field("main", &self.main)
            .field("on_stack_upvalues", &self.on_stack_upvalues)
            .field("evicted_upvalues", &self.evicted_upvalues)
            .field("next_upvalue_id", &self.next_upvalue_id)
            .finish_non_exhaustive()
    }
}

pub struct StackGuard<'a, Ty>
where
    Ty: CoreTypes,
{
    stack: &'a mut Stack<Ty>,
    boundary: RawStackSlot,
}

impl<'a, Ty> StackGuard<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn new(stack: &'a mut Stack<Ty>) -> Self {
        StackGuard {
            stack,
            boundary: RawStackSlot(0),
        }
    }

    pub(super) fn guard(&mut self, protected_size: RawStackSlot) -> Option<StackGuard<'_, Ty>> {
        if self.stack.len() < protected_size.0 {
            return None;
        }

        let r = StackGuard {
            stack: self.stack,
            boundary: protected_size,
        };

        Some(r)
    }

    fn reborrow(&mut self) -> StackGuard<'_, Ty> {
        let StackGuard { stack, boundary } = self;

        StackGuard {
            stack,
            boundary: *boundary,
        }
    }

    pub(super) fn lua_frame(&mut self) -> LuaStackFrame<'_, Ty> {
        LuaStackFrame(self.reborrow())
    }

    // pub fn transient_frame(&mut self) -> TransientStackFrame<'_, Ty> {
    //     self.mark_unsync(StackSlot(0));
    //     self.mark_transient(StackSlot(0));

    //     TransientStackFrame(self.reborrow())
    // }

    pub fn as_slice(&self) -> &TiSlice<StackSlot, WeakValue<Ty>> {
        self.stack.main[self.boundary..].raw.as_ref()
    }

    fn as_mut_slice(&mut self) -> &mut TiSlice<StackSlot, WeakValue<Ty>> {
        self.stack.main[self.boundary..].raw.as_mut()
    }

    fn mark_unsync(&mut self, slot: StackSlot) {
        let slot = self.boundary + slot;
        self.stack.mark_unsync(slot);
    }

    fn mark_transient(&mut self, slot: StackSlot) {
        let slot = self.boundary + slot;
        self.stack.mark_transient(slot);
    }

    fn push(&mut self, value: WeakValue<Ty>, source: Source<StackSlot>) {
        let source = source.map(|slot| self.boundary + slot);
        self.stack.push(value, source);
    }

    fn set(&mut self, slot: StackSlot, value: WeakValue<Ty>, source: Source<StackSlot>) {
        let slot = self.boundary + slot;
        let source = source.map(|slot| self.boundary + slot);
        self.stack.set(slot, value, source)
    }

    pub fn next_slot(&self) -> StackSlot {
        let val = self.stack.len() - self.boundary.0;
        StackSlot(val)
    }

    fn fresh_upvalue(&mut self, value: StrongValue<Ty>) -> UpvalueId {
        self.stack.fresh_upvalue(value)
    }

    pub fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&WeakValue<Ty>> {
        self.stack.get_upvalue(upvalue)
    }

    fn set_upvalue(&mut self, upvalue: UpvalueId, value: WeakValue<Ty>, source: Source<StackSlot>) {
        let source = source.map(|slot| self.boundary + slot);
        self.stack.set_upvalue(upvalue, value, source)
    }

    fn mark_as_upvalue(&mut self, slot: StackSlot) -> Option<UpvalueId> {
        let slot = self.boundary + slot;
        self.stack.mark_as_upvalue(slot)
    }

    fn insert(&mut self, slot: StackSlot, value: WeakValue<Ty>, source: Source<StackSlot>) {
        let slot = self.boundary + slot;
        let source = source.map(|slot| self.boundary + slot);
        self.stack.insert(slot, value, source)
    }

    fn remove_range(&mut self, range: impl RangeBounds<StackSlot>) {
        let start = range.start_bound().mapb(|slot| self.boundary + *slot);
        let end = range.end_bound().mapb(|slot| self.boundary + *slot);

        self.stack.remove_range((start, end))
    }

    fn truncate(&mut self, new_len: StackSlot) {
        let new_len = self.boundary + new_len;
        self.stack.truncate(new_len)
    }

    fn clear(&mut self) {
        self.truncate(StackSlot(0))
    }

    fn sync(&mut self, heap: &mut Heap) {
        self.stack.sync(heap);
    }

    pub(super) fn boundary(&self) -> RawStackSlot {
        self.boundary
    }

    fn pop(&mut self) -> Option<WeakValue<Ty>> {
        if self.stack.len() <= self.boundary.0 {
            return None;
        }

        self.stack.pop()
    }

    fn remove(&mut self, slot: StackSlot) -> Option<WeakValue<Ty>> {
        let slot = self.boundary + slot;
        self.stack.remove(slot)
    }

    /// Set stack to specified height.
    ///
    /// If `len < height` it will get filled with default values.
    /// If `len > height` extra values will be removed.
    ///
    /// If you want to obtain extraneous values use
    /// [`adjust_height_and_collect`](Self::adjust_height_and_collect).
    fn adjust_height(&mut self, height: StackSlot) {
        self.adjust_height_and_collect(height);
    }

    /// Set stack to specified height and obtain extraneous values if any.
    ///
    /// If `len < height` it will get filled with default values.
    /// If `len > height` extra values will be removed from the stack and returned.
    /// Otherwise function returns empty vec.
    fn adjust_height_and_collect(&mut self, height: StackSlot) -> Vec<WeakValue<Ty>> {
        let requested_height = self.boundary + height;

        self.stack.adjust_height_with_variadics(requested_height)
    }
}

impl<'a, Ty> StackGuard<'a, Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Display,
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

impl<'a, Ty> Deref for StackGuard<'a, Ty>
where
    Ty: CoreTypes,
{
    type Target = TiSlice<StackSlot, WeakValue<Ty>>;

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<'a, Ty> Debug for StackGuard<'a, Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StackView")
            .field("stack", &self.stack)
            .field("boundary", &self.boundary)
            .finish()
    }
}

// pub struct TransientStackFrame<'a, Ty>(StackGuard<'a, Ty>)
// where
//     Ty: CoreTypes;

// impl<'a, Ty> TransientStackFrame<'a, Ty>
// where
//     Ty: CoreTypes
// {
//     pub fn as_slice(&self) -> &TiSlice<StackSlot, WeakValue<Ty>> {
//         self.0.as_slice()
//     }

//     pub fn as_mut_slice(&mut self) -> &mut TiSlice<StackSlot, WeakValue<Ty>> {
//         self.0.as_mut_slice()
//     }

//     pub fn next_slot(&self) -> StackSlot {
//         self.0.next_slot()
//     }

//     pub(super) fn push_with_source(&mut self, value: WeakValue<Ty>, source: Source<StackSlot>) {
//         self.0.push(value, source)
//     }

//     pub fn push(&mut self, value: WeakValue<Ty>) {
//         self.push_with_source(value, Default::default())
//     }

//     pub fn pop(&mut self) -> Option<WeakValue<Ty>> {
//         self.0.pop()
//     }

//     pub(super) fn fresh_upvalue(&mut self, value: StrongValue<Ty>) -> UpvalueId {
//         self.0.fresh_upvalue(value)
//     }

//     pub fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&WeakValue<Ty>> {
//         self.0.get_upvalue(upvalue)
//     }

//     pub fn get_upvalue_mut(&mut self, upvalue: UpvalueId) -> Option<&mut WeakValue<Ty>> {
//         // self.0.get_upvalue_mut(upvalue)
//         todo!()
//     }

//     pub(super) fn mark_as_upvalue(&mut self, slot: StackSlot) -> Option<UpvalueId> {
//         self.0.mark_as_upvalue(slot)
//     }

//     pub(super) fn insert_with_source(&mut self, slot: StackSlot, value: WeakValue<Ty>, source: Source<StackSlot>) {
//         self.0.insert(slot, value, source)
//     }

//     pub fn insert(&mut self, slot: StackSlot, value: WeakValue<Ty>) {
//         self.insert_with_source(slot, value, Default::default())
//     }

//     pub fn remove(&mut self, slot: StackSlot) -> Option<WeakValue<Ty>> {
//         self.0.remove(slot)
//     }

//     pub fn remove_range(&mut self, range: impl RangeBounds<StackSlot>) {
//         self.0.remove_range(range)
//     }

//     pub fn truncate(&mut self, new_len: StackSlot) {
//         self.0.truncate(new_len)
//     }

//     pub fn clear(&mut self) {
//         self.0.clear()
//     }

//     pub fn adjust_height(&mut self, height: StackSlot) {
//         self.0.adjust_height(height)
//     }

//     pub fn adjust_height_and_collect(&mut self, height: StackSlot) -> Vec<WeakValue<Ty>> {
//         self.0.adjust_height_and_collect(height)
//     }

//     pub fn sync(&mut self, heap: &mut Heap) {
//         self.0.sync(heap)
//     }

//     pub(super) fn boundary(&self) -> RawStackSlot {
//         self.0.boundary()
//     }
// }

// impl<'a, Ty> Deref for TransientStackFrame<'a, Ty>
// where
//     Ty: CoreTypes
// {
//     type Target = TiSlice<StackSlot, WeakValue<Ty>>;

//     fn deref(&self) -> &Self::Target {
//         self.as_slice()
//     }
// }

// impl<'a, Ty> DerefMut for TransientStackFrame<'a, Ty>
// where
//     Ty: CoreTypes
// {
//     fn deref_mut(&mut self) -> &mut Self::Target {
//         self.as_mut_slice()
//     }
// }

// impl<'a, Ty> Extend<WeakValue<Ty>> for TransientStackFrame<'a, Ty>
// where
//     Ty: CoreTypes
// {
//     fn extend<T: IntoIterator<Item = WeakValue<Ty>>>(&mut self, iter: T) {
//         self.0.stack.main.extend(iter)
//     }
// }

// impl<'a, Ty> Debug for TransientStackFrame<'a, Ty>
// where
//     Ty: CoreTypes,
//     WeakValue<Ty>: Debug,
// {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         f.debug_tuple("TransientStackFrame").field(&self.0).finish()
//     }
// }

pub(super) struct LuaStackFrame<'a, Ty>(StackGuard<'a, Ty>)
where
    Ty: CoreTypes;

impl<'a, Ty> LuaStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(super) fn as_slice(&self) -> &TiSlice<StackSlot, WeakValue<Ty>> {
        self.0.as_slice()
    }

    pub(super) fn push(&mut self, value: WeakValue<Ty>, source: Source<StackSlot>) {
        self.0.push(value, source)
    }

    pub(super) fn set(&mut self, slot: StackSlot, value: WeakValue<Ty>, source: Source<StackSlot>) {
        self.0.set(slot, value, source)
    }

    pub(super) fn next_slot(&self) -> StackSlot {
        self.0.next_slot()
    }

    pub(super) fn fresh_upvalue(&mut self, value: StrongValue<Ty>) -> UpvalueId {
        self.0.fresh_upvalue(value)
    }

    pub(super) fn get_upvalue(&self, upvalue: UpvalueId) -> Option<&WeakValue<Ty>> {
        self.0.get_upvalue(upvalue)
    }

    pub(super) fn set_upvalue(
        &mut self,
        upvalue: UpvalueId,
        value: WeakValue<Ty>,
        source: Source<StackSlot>,
    ) {
        self.0.set_upvalue(upvalue, value, source)
    }

    pub(super) fn mark_as_upvalue(&mut self, slot: StackSlot) -> Option<UpvalueId> {
        self.0.mark_as_upvalue(slot)
    }

    pub(super) fn insert(
        &mut self,
        slot: StackSlot,
        value: WeakValue<Ty>,
        source: Source<StackSlot>,
    ) {
        self.0.insert(slot, value, source)
    }

    pub(super) fn remove_range(&mut self, range: impl RangeBounds<StackSlot>) {
        self.0.remove_range(range)
    }

    pub(super) fn truncate(&mut self, new_len: StackSlot) {
        self.0.truncate(new_len)
    }

    pub(super) fn clear(&mut self) {
        self.0.clear()
    }

    pub(super) fn sync(&mut self, heap: &mut Heap) {
        self.0.sync(heap);
    }

    pub(super) fn boundary(&self) -> RawStackSlot {
        self.0.boundary()
    }

    pub(super) fn pop(&mut self) -> Option<WeakValue<Ty>> {
        self.0.pop()
    }

    pub(super) fn remove(&mut self, slot: StackSlot) -> Option<WeakValue<Ty>> {
        self.0.remove(slot)
    }

    /// Set stack to specified height.
    ///
    /// If `len < height` it will get filled with default values.
    /// If `len > height` extra values will be removed.
    ///
    /// If you want to obtain extraneous values use
    /// [`adjust_height_and_collect`](Self::adjust_height_and_collect).
    pub(super) fn adjust_height(&mut self, height: StackSlot) {
        self.0.adjust_height_and_collect(height);
    }

    /// Set stack to specified height and obtain extraneous values if any.
    ///
    /// If `len < height` it will get filled with default values.
    /// If `len > height` extra values will be removed from the stack and returned.
    /// Otherwise function returns empty vec.
    pub(super) fn adjust_height_and_collect(&mut self, height: StackSlot) -> Vec<WeakValue<Ty>> {
        self.0.adjust_height_and_collect(height)
    }

    pub(super) fn extend(
        &mut self,
        iter: impl IntoIterator<Item = WeakValue<Ty>>,
        is_rooted: bool,
    ) {
        todo!()
    }

    pub(super) fn take1(&mut self) -> Result<[WeakValue<Ty>; 1], MissingArgsError> {
        let v0 = self.pop().ok_or_else(|| MissingArgsError {
            stack_len: self.len(),
            expected_args: 1,
        })?;
        Ok([v0])
    }

    pub(super) fn take2(&mut self) -> Result<[WeakValue<Ty>; 2], MissingArgsError> {
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

    pub(super) fn take3(&mut self) -> Result<[WeakValue<Ty>; 3], MissingArgsError> {
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
                let value = self.pop().unwrap();
                self.push(Value::Bool(value.to_bool()), Source::TrustedIsRooted(false));
            }
            // Not-equal additionally needs to inverse the resulting boolean.
            Neq => {
                self.adjust_height(StackSlot(1));
                let value = self.pop().unwrap();
                self.push(
                    Value::Bool(!value.to_bool()),
                    Source::TrustedIsRooted(false),
                );
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

impl<'a, Ty> Deref for LuaStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    type Target = TiSlice<StackSlot, WeakValue<Ty>>;

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}
