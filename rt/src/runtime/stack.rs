use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::ops::{Add, Bound, Deref, DerefMut, Range, RangeBounds, Sub};

use bitvec::vec::BitVec;
use gc::{Gc, Heap, Root};
use repr::index::StackSlot;
use repr::tivec::TiVec;

use super::frame::{Closure, UpvaluePlace};
use super::Event;
use crate::error::opcode::MissingArgsError;
use crate::value::{CoreTypes, Value, WeakValue};

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

impl Sub for RawStackSlot {
    type Output = StackSlot;

    fn sub(self, rhs: Self) -> Self::Output {
        let val = self
            .0
            .checked_sub(rhs.0)
            .expect("cannot generate StackSlot for a stack position below `self`");
        StackSlot(val)
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

    /// Marker set `true` for on-stack upvalues.
    ///
    /// # Invariants
    ///
    /// ```ignore
    /// assert!(upvalue_mark.len() <= main.len());
    /// ```
    ///
    /// It is ok for the length to not be extended in time:
    /// the only place where upvalues can be created is inside [`register_closure`](Self::register_closure).
    /// It will extend bitvec as necessary to fit `true` bits.
    ///
    /// However, *removing* values from the stack should be done with care
    /// to ensure that length is properly adjusted.
    /// If removed slot contains an upvalue you need to see that it is properly evicted (and marker reset).
    ///
    /// There are two reasons for relaxed invariant (compared to keeping length in sync at all times):
    /// * It is relatevely rare for upvalues to get evicted,
    ///    so most of the time it will boil down to shuffling `false` on and off.
    /// * It simplifies implementation of `TransientStackFrame` as it doesn't need to care about markers anymore.
    upvalue_mark: BitVec,

    /// Index of first value known to be *not* rooted.
    ///
    /// It implies that all values below it are rooted.
    first_transient: Option<RawStackSlot>,

    /// Rooted mirror of the stack.
    ///
    /// Contents of this vec should be syncronized with `main` field.
    /// Its purpose is to keep on-stack references alive.
    root: Root<Vec<WeakValue<Ty>>>,

    /// Index of first value known to be not in sync between `main` and `root`.
    sync_point: RawStackSlot,

    /// Closures that keep upvalues on the stack.
    ///
    /// Keep weak references here:
    /// we only care about updating upvalues for alive closures
    /// so it doesn't matter whether closure is dropped or not.
    ///
    /// Upvalues referencing stack slots need to be updated when those are dissociated.
    closures: Vec<Gc<Closure<Ty>>>,

    /// Upvalues that got dissociated from the stack,
    /// but that change was not yet propagated to affected closures.
    ///
    /// We only keep one value for a reason.
    /// Constructing new closure requires allocation,
    /// therefore stack needs to be synced before that happens.
    /// It implies that any dissociated upvalues got updated.
    evicted_upvalues: HashMap<RawStackSlot, WeakValue<Ty>>,
}

impl<Ty> Stack<Ty>
where
    Ty: CoreTypes,
{
    pub fn new(heap: &mut Heap) -> Self {
        Stack {
            main: Default::default(),
            upvalue_mark: Default::default(),
            first_transient: Default::default(),
            root: heap.alloc(Default::default()),
            sync_point: RawStackSlot(0),
            closures: Default::default(),
            evicted_upvalues: Default::default(),
        }
    }

    fn sync_stack_cache(&mut self, heap: &mut Heap) {
        let mirror = &mut heap[&self.root];
        mirror.truncate(self.sync_point.0);
        mirror.extend(self.main[self.sync_point..].iter().cloned());

        self.sync_point = self.main.next_key();
        self.first_transient = None;
    }

    fn sync_upvalue_cache(&mut self, heap: &mut Heap) {
        heap.pause(|heap| {
            let evicted_upvalues = std::mem::take(&mut self.evicted_upvalues);

            let reallocated: HashMap<_, _> = evicted_upvalues
                .into_iter()
                .map(|(slot, value)| {
                    let value = heap.alloc(value).downgrade();

                    (slot, value)
                })
                .collect();

            self.closures.retain(|&closure_ref| {
                // Discard closure if it was already dropped.
                let Some(closure) = heap.get_mut(closure_ref) else {
                    return false;
                };

                let mut have_upvalue_on_stack = false;
                for place in closure.upvalues_mut() {
                    let value = match *place {
                        UpvaluePlace::Stack(slot) => {
                            if let Some(&value) = reallocated.get(&slot) {
                                value
                            } else {
                                have_upvalue_on_stack = true;
                                continue;
                            }
                        }
                        UpvaluePlace::Place(_) => continue,
                    };

                    *place = UpvaluePlace::Place(value);
                }

                // Discard closure if it have no on-stack upvalues left.
                have_upvalue_on_stack
            })
        })
    }

    fn sync_transient(&mut self, heap: &mut Heap) {
        if self.first_transient.is_some() {
            self.sync_stack_cache(heap);
        }

        if self
            .evicted_upvalues
            .iter()
            .any(|(_, value)| value.is_transient())
        {
            self.sync_upvalue_cache(heap);
        }
    }

    fn sync_upvalues(&mut self, heap: &mut Heap) {
        if self.evicted_upvalues.is_empty() {
            return;
        }

        if self.first_transient.is_some() {
            self.sync_stack_cache(heap);
        }

        self.sync_upvalue_cache(heap);
    }

    fn sync_full(&mut self, heap: &mut Heap) {
        self.sync_stack_cache(heap);
        self.sync_upvalue_cache(heap);
    }

    fn register_closure(&mut self, closure_ref: &Root<Closure<Ty>>, heap: &Heap) {
        let closure = &heap[closure_ref];

        let iter = closure.upvalues().iter().filter_map(|place| match place {
            UpvaluePlace::Stack(slot) => Some(*slot),
            UpvaluePlace::Place(_) => None,
        });

        let Some(max_slot) = iter.clone().max() else {
            return;
        };
        let max_slot = RawStackSlot(max_slot.0 + 1);

        self.closures.push(closure_ref.downgrade());

        if self.upvalue_mark.len() < max_slot.0 {
            self.upvalue_mark.resize(max_slot.0, false);
        }

        for slot in iter {
            self.upvalue_mark.set(slot.0, true);
        }
    }

    pub fn guard(&mut self) -> StackGuard<Ty> {
        StackGuard::new(self)
    }

    fn is_transient(&self, source: Source<RawStackSlot>) -> bool {
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
        debug_assert!(self.upvalue_mark.len() <= self.main.len());

        let slot = self.main.next_key();
        if value.is_transient() && self.is_transient(source) {
            self.mark_transient(slot);
        }

        debug_assert!(self.sync_point <= self.main.next_key());

        self.main.push_and_get_key(value)
    }

    fn set(&mut self, slot: RawStackSlot, value: WeakValue<Ty>, source: Source<RawStackSlot>) {
        if self.main.get(slot).is_some() {
            if value.is_transient() && self.is_transient(source) {
                self.mark_transient(slot);
            }
            self.mark_unsync(slot);
        }

        if let Some(place) = self.main.get_mut(slot) {
            *place = value;
        }
    }

    fn len(&self) -> usize {
        self.main.len()
    }

    /// Evict upvalues in range from the stack.
    ///
    /// This function assumes that no unupdated upvalues exist in this range.
    fn evict_upvalues(&mut self, range: impl RangeBounds<RawStackSlot>) {
        let (start, end) = self.transform_range(range);
        let mark_end = self.upvalue_mark.len().min(end.0);

        let marks = self
            .upvalue_mark
            .get_mut(start.0..mark_end)
            .unwrap_or_default();

        let iter = marks
            .iter_ones()
            .map(|i| RawStackSlot(start.0 + i))
            .map(|slot| (slot, self.main[slot].clone()));

        for (slot, value) in iter {
            let old_value = self.evicted_upvalues.insert(slot, value.clone());
            debug_assert!(old_value.is_none());
        }

        if start.0 < self.upvalue_mark.len() {
            self.upvalue_mark.truncate(start.0);
        }
    }

    fn insert(&mut self, slot: RawStackSlot, value: WeakValue<Ty>, source: Source<RawStackSlot>) {
        if value.is_transient() && self.is_transient(source) {
            self.mark_transient(slot);
        } else if let Some(tr) = &mut self.first_transient {
            // Non-transient insertion shifts the boundary up.
            tr.0 += 1;
        }
        self.mark_unsync(slot);

        // Internally inserting happens only in one case:
        // when invoked value have `__call` metamethod and needs to be passed in.
        // Regardless, everything above insertion point is going to be evicted anyways.
        self.evict_upvalues(slot..);

        self.main.insert(slot, value);
    }

    fn drain(
        &mut self,
        range: impl RangeBounds<RawStackSlot>,
    ) -> std::vec::Drain<'_, WeakValue<Ty>> {
        let (start, end) = self.transform_range(range);

        let len = end
            .0
            .checked_sub(start.0)
            .expect("starting point of range should not be grater than ending point");

        if len == 0 {
            return self.main.raw.drain(0..0);
        }

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
                    // But if there are no values left above removal point reset the marker.
                    self.first_transient = None;
                }
            }
        }

        self.evict_upvalues(start..);

        self.main.drain(start..end)
    }

    fn truncate(&mut self, slot: RawStackSlot) {
        // Values are drained on drop.
        let _ = self.drain(slot..);
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

    fn remove(&mut self, slot: RawStackSlot) -> Option<WeakValue<Ty>> {
        if slot < self.main.next_key() {
            self.drain(slot..=slot).next()
        } else {
            None
        }
    }

    fn pop(&mut self) -> Option<WeakValue<Ty>> {
        self.main
            .last_key()
            .and_then(|slot| self.drain(slot..).next())
    }

    fn adjust_height(&mut self, height: RawStackSlot) -> std::vec::Drain<'_, WeakValue<Ty>> {
        match height.0.checked_sub(self.main.len()) {
            None | Some(0) => (),
            Some(n) => {
                self.main.extend(std::iter::repeat(Value::Nil).take(n));
            }
        }

        self.drain(height..)
    }

    fn extend(&mut self, iter: impl IntoIterator<Item = WeakValue<Ty>>, is_rooted: bool) {
        let start = self.main.next_key();
        if !is_rooted {
            self.mark_transient(start);
        }
        self.mark_unsync(start);

        self.main.extend(iter);
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
            .field("first_transient", &self.first_transient)
            .field("root", &self.root)
            .field("sync_point", &self.sync_point)
            .field("closures", &self.closures)
            .field("evicted_upvalues", &self.evicted_upvalues)
            .finish()
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

    pub fn transient_frame(&mut self) -> TransientStackFrame<'_, Ty> {
        // Enforce invariant of `.upvalue_mark`
        assert!(
            self.stack
                .upvalue_mark
                .get(self.boundary.0..)
                .unwrap_or_default()
                .not_any(),
            "transient frame should not contain on-stack upvalues"
        );
        self.stack.upvalue_mark.truncate(self.boundary.0);

        self.mark_unsync(StackSlot(0));
        self.mark_transient(StackSlot(0));

        let StackGuard { stack, boundary } = self;

        TransientStackFrame {
            stack,
            boundary: *boundary,
        }
    }

    pub fn frame<'s>(&'s mut self, heap: &'s mut Heap) -> StackFrame<'s, Ty> {
        StackFrame {
            guard: self.reborrow(),
            heap,
        }
    }

    pub fn as_slice(&self) -> &[WeakValue<Ty>] {
        self.stack.main[self.boundary..].raw.as_ref()
    }

    fn mark_unsync(&mut self, slot: StackSlot) {
        let slot = self.boundary + slot;
        self.stack.mark_unsync(slot);
    }

    fn mark_transient(&mut self, slot: StackSlot) {
        let slot = self.boundary + slot;
        self.stack.mark_transient(slot);
    }

    pub fn get_slot(&self, slot: StackSlot) -> Option<&WeakValue<Ty>> {
        let slot = self.boundary + slot;
        self.get_raw_slot(slot)
    }

    fn get_raw_slot(&self, slot: RawStackSlot) -> Option<&WeakValue<Ty>> {
        self.stack.main.get(slot)
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

    fn insert(&mut self, slot: StackSlot, value: WeakValue<Ty>, source: Source<StackSlot>) {
        let slot = self.boundary + slot;
        let source = source.map(|slot| self.boundary + slot);
        self.stack.insert(slot, value, source)
    }

    pub fn drain(
        &mut self,
        range: impl RangeBounds<StackSlot>,
    ) -> impl Iterator<Item = WeakValue<Ty>> + '_ {
        let range = map_range(range, self.boundary..self.stack.main.next_key());

        self.stack.drain(range)
    }

    pub fn truncate(&mut self, new_len: StackSlot) {
        let new_len = self.boundary + new_len;
        self.stack.truncate(new_len)
    }

    pub fn clear(&mut self) {
        self.truncate(StackSlot(0))
    }

    pub(super) fn boundary(&self) -> RawStackSlot {
        self.boundary
    }

    pub fn pop(&mut self) -> Option<WeakValue<Ty>> {
        if self.stack.len() <= self.boundary.0 {
            return None;
        }

        self.stack.pop()
    }

    pub fn remove(&mut self, slot: StackSlot) -> Option<WeakValue<Ty>> {
        let slot = self.boundary + slot;
        self.stack.remove(slot)
    }

    /// Set stack to specified height.
    ///
    /// If `len < height` it will get filled with default values.
    /// If `len > height` extra values will be removed.
    ///
    /// The resulting iterator will return excess values if any.
    pub fn adjust_height(&mut self, height: StackSlot) -> std::vec::Drain<'_, WeakValue<Ty>> {
        let height = self.boundary + height;
        self.stack.adjust_height(height)
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (StackSlot, &WeakValue<Ty>)> {
        (0..).map(StackSlot).zip(self.iter())
    }

    fn evict_upvalues(&mut self, range: impl RangeBounds<StackSlot>) {
        let range = map_range(range, self.boundary..self.stack.main.next_key());
        self.stack.evict_upvalues(range)
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
            let upvalue_mark = ' ';

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
    type Target = [WeakValue<Ty>];

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

pub(super) struct LuaStackFrame<'a, Ty>(StackGuard<'a, Ty>)
where
    Ty: CoreTypes;

impl<'a, Ty> LuaStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(super) fn sync_transient(&mut self, heap: &mut Heap) {
        self.0.stack.sync_transient(heap)
    }

    pub(super) fn sync_upvalues(&mut self, heap: &mut Heap) {
        self.0.stack.sync_upvalues(heap)
    }

    pub(super) fn sync_full(&mut self, heap: &mut Heap) {
        self.0.stack.sync_full(heap);
    }

    pub(super) fn register_closure(&mut self, closure_ref: &Root<Closure<Ty>>, heap: &Heap) {
        self.0.stack.register_closure(closure_ref, heap)
    }
}

impl<'a, Ty> LuaStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(super) fn as_slice(&self) -> &[WeakValue<Ty>] {
        self.0.as_slice()
    }

    pub(super) fn get_slot(&self, slot: StackSlot) -> Option<&WeakValue<Ty>> {
        self.0.get_slot(slot)
    }

    pub(super) fn get_raw_slot(&self, slot: RawStackSlot) -> Option<&WeakValue<Ty>> {
        self.0.get_raw_slot(slot)
    }

    pub(super) fn push(&mut self, value: WeakValue<Ty>, source: Source<StackSlot>) {
        self.0.push(value, source)
    }

    pub(super) fn push_raw(&mut self, value: WeakValue<Ty>, source: Source<RawStackSlot>) {
        self.0.stack.push(value, source);
    }

    pub(super) fn set(&mut self, slot: StackSlot, value: WeakValue<Ty>, source: Source<StackSlot>) {
        self.0.set(slot, value, source)
    }

    pub(super) fn set_raw(
        &mut self,
        slot: RawStackSlot,
        value: WeakValue<Ty>,
        source: Source<StackSlot>,
    ) {
        let source = source.map(|slot| self.boundary() + slot);
        self.0.stack.set(slot, value, source)
    }

    pub(super) fn next_slot(&self) -> StackSlot {
        self.0.next_slot()
    }

    pub(super) fn insert(
        &mut self,
        slot: StackSlot,
        value: WeakValue<Ty>,
        source: Source<StackSlot>,
    ) {
        self.0.insert(slot, value, source)
    }

    pub(super) fn drain(
        &mut self,
        range: impl RangeBounds<StackSlot>,
    ) -> impl Iterator<Item = WeakValue<Ty>> + '_ {
        self.0.drain(range)
    }

    // pub(super) fn truncate(&mut self, new_len: StackSlot) {
    //     self.0.truncate(new_len)
    // }

    pub(super) fn clear(&mut self) {
        self.0.clear()
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

    pub(super) fn evict_upvalues(&mut self, range: impl RangeBounds<StackSlot>) {
        self.0.evict_upvalues(range)
    }

    /// Set stack to specified height.
    ///
    /// If `len < height` it will get filled with default values.
    /// If `len > height` extra values will be removed.
    ///
    /// The resulting iterator will return excess values if any.
    pub(super) fn adjust_height(
        &mut self,
        height: StackSlot,
    ) -> impl Iterator<Item = WeakValue<Ty>> + '_ {
        self.0.adjust_height(height)
    }

    pub(super) fn extend(
        &mut self,
        iter: impl IntoIterator<Item = WeakValue<Ty>>,
        is_rooted: bool,
    ) {
        self.0.stack.extend(iter, is_rooted)
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
                let _ = self.adjust_height(StackSlot(1));
            }
            // Ops resulting in single value + coercion to bool.
            Eq | Lt | LtEq => {
                let _ = self.adjust_height(StackSlot(1));
                let value = self.pop().unwrap();
                self.push(Value::Bool(value.to_bool()), Source::TrustedIsRooted(false));
            }
            // Not-equal additionally needs to inverse the resulting boolean.
            Neq => {
                let _ = self.adjust_height(StackSlot(1));
                let value = self.pop().unwrap();
                self.push(
                    Value::Bool(!value.to_bool()),
                    Source::TrustedIsRooted(false),
                );
            }
            // Index getter results in single value.
            Index => {
                let _ = self.adjust_height(StackSlot(1));
            }
            // Index setter results in no values.
            NewIndex => {
                let _ = self.adjust_height(StackSlot(0));
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
    type Target = [WeakValue<Ty>];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

pub struct TransientStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    stack: &'a mut Stack<Ty>,
    boundary: RawStackSlot,
}

impl<'a, Ty> TransientStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    pub fn as_slice(&self) -> &[WeakValue<Ty>] {
        &self.stack.main[self.boundary..].raw
    }

    pub fn as_mut_slice(&mut self) -> &mut [WeakValue<Ty>] {
        &mut self.stack.main[self.boundary..].raw
    }

    pub fn next_slot(&self) -> StackSlot {
        self.stack.main.next_key() - self.boundary
    }

    pub fn push(&mut self, value: WeakValue<Ty>) {
        self.stack.main.push(value)
    }

    pub fn pop(&mut self) -> Option<WeakValue<Ty>> {
        if self.stack.main.next_key() > self.boundary {
            self.stack.main.pop()
        } else {
            None
        }
    }

    pub fn insert(&mut self, slot: StackSlot, value: WeakValue<Ty>) {
        let slot = self.boundary + slot;
        self.stack.main.insert(slot, value)
    }

    pub fn remove(&mut self, slot: StackSlot) -> WeakValue<Ty> {
        let slot = self.boundary + slot;
        self.stack.main.remove(slot)
    }

    pub fn drain(
        &mut self,
        range: impl RangeBounds<StackSlot>,
    ) -> std::vec::Drain<'_, WeakValue<Ty>> {
        let range = map_range(range, self.boundary..self.stack.main.next_key());
        self.stack.main.drain(range)
    }

    pub fn truncate(&mut self, new_len: StackSlot) {
        let new_len = self.boundary + new_len;
        self.stack.main.truncate(new_len.0)
    }

    pub fn clear(&mut self) {
        self.stack.main.clear()
    }

    pub fn adjust_height(&mut self, height: StackSlot) -> std::vec::Drain<'_, WeakValue<Ty>> {
        let height = self.boundary + height;

        match height.0.checked_sub(self.stack.main.len()) {
            None | Some(0) => (),
            Some(n) => {
                self.stack
                    .main
                    .extend(std::iter::repeat(Value::Nil).take(n));
            }
        }

        self.stack.main.drain(height..)
    }

    pub fn sync(&mut self, heap: &mut Heap) {
        // There are no on-stack upvalues by construction.
        self.stack.sync_transient(heap)
    }
}

impl<'a, Ty> Deref for TransientStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    type Target = [WeakValue<Ty>];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<'a, Ty> DerefMut for TransientStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<'a, Ty> Extend<WeakValue<Ty>> for TransientStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    fn extend<T: IntoIterator<Item = WeakValue<Ty>>>(&mut self, iter: T) {
        self.stack.main.extend(iter)
    }
}

impl<'a, Ty> Debug for TransientStackFrame<'a, Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TransientStackFrame")
            .field(&self.as_slice())
            .finish()
    }
}

pub struct StackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    guard: StackGuard<'a, Ty>,
    heap: &'a mut Heap,
}

impl<'a, Ty> StackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    pub fn get_mut(&mut self, slot: StackSlot) -> Option<SlotProxy<'_, Ty>> {
        let value = self.get_slot(slot)?.clone();
        let slot = self.boundary + slot;

        let r = SlotProxy {
            slot,
            value,
            stack: self.guard.stack,
        };

        Some(r)
    }

    pub fn push(&mut self, value: WeakValue<Ty>) {
        self.guard.push(value, Source::TrustedIsRooted(false));
    }

    pub fn insert(&mut self, slot: StackSlot, value: WeakValue<Ty>) {
        self.guard
            .insert(slot, value, Source::TrustedIsRooted(false))
    }
}

impl<'a, Ty> Deref for StackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    type Target = StackGuard<'a, Ty>;

    fn deref(&self) -> &Self::Target {
        &self.guard
    }
}

impl<'a, Ty> DerefMut for StackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.guard
    }
}

impl<'a, Ty> Drop for StackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    fn drop(&mut self) {
        self.guard.stack.sync_transient(self.heap);
    }
}

pub struct SlotProxy<'a, Ty>
where
    Ty: CoreTypes,
{
    slot: RawStackSlot,
    value: WeakValue<Ty>,
    stack: &'a mut Stack<Ty>,
}

impl<'a, Ty> Deref for SlotProxy<'a, Ty>
where
    Ty: CoreTypes,
{
    type Target = WeakValue<Ty>;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'a, Ty> DerefMut for SlotProxy<'a, Ty>
where
    Ty: CoreTypes,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<'a, Ty> Drop for SlotProxy<'a, Ty>
where
    Ty: CoreTypes,
{
    fn drop(&mut self) {
        self.stack
            .set(self.slot, self.value.take(), Source::TrustedIsRooted(false));
    }
}

fn map_range(
    range: impl RangeBounds<StackSlot>,
    bounds: Range<RawStackSlot>,
) -> Range<RawStackSlot> {
    debug_assert!(bounds.start <= bounds.end);

    let start = match range.start_bound() {
        Bound::Unbounded => bounds.start,
        Bound::Included(&slot) => bounds.start + slot,
        Bound::Excluded(&slot) => bounds.start + slot + StackSlot(1),
    };

    let end = match range.end_bound() {
        Bound::Unbounded => bounds.end,
        Bound::Included(&slot) => bounds.start + slot + StackSlot(1),
        Bound::Excluded(&slot) => bounds.start + slot,
    };

    let end = end.min(bounds.end);

    start..end
}
