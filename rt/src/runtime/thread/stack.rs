use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::{Add, Bound, Deref, DerefMut, Range, RangeBounds, Sub};

use bitvec::vec::BitVec;
use gc::{GcCell, RootCell};
use repr::index::StackSlot;
use repr::tivec::TiVec;

use super::frame::Event;
use crate::error::opcode::MissingArgsError;
use crate::gc::{DisplayWith, Heap};
use crate::runtime::closure::{Closure, UpvaluePlace};
use crate::value::{CoreTypes, Value, WeakValue};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
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

/// Struct tracking change between main portion of the stack and rooted mirror.
///
/// # Invariants
///
/// ```ignore
/// assert!(unsync <= transient);
/// ```
#[derive(Debug, Default)]
struct Diff {
    /// Index of first non-rooted value on main stack.
    ///
    /// It can point past the last element to indicate that there are no transient values.
    transient: RawStackSlot,

    /// Index of first unsynchronized value on main stack.
    ///
    /// It can point past the last element to indicate that there are no unsync values.
    unsync: RawStackSlot,
}

impl Diff {
    fn modify(&mut self, slot: RawStackSlot, is_transient: bool) {
        self.unsync = self.unsync.min(slot);

        if is_transient {
            self.transient = self.transient.min(slot);
        }
    }

    fn push(&mut self, slot: RawStackSlot, is_transient: bool) {
        self.unsync = self.unsync.min(slot);

        // Update transient if it was pointing past the main.
        if !is_transient && self.transient == slot {
            self.transient.0 += 1;
        }
    }

    fn insert(&mut self, slot: RawStackSlot, is_transient: bool) {
        self.unsync = self.unsync.min(slot);

        if is_transient {
            self.transient = self.transient.min(slot);
        } else if slot <= self.transient {
            self.transient.0 += 1;
        }
    }

    fn extend(&mut self, range: Range<RawStackSlot>, is_transient: bool) {
        debug_assert!(range.start <= range.end);
        debug_assert!(self.transient <= range.start);

        self.unsync = self.unsync.min(range.start);

        if !is_transient && self.transient == range.start {
            self.transient = range.end;
        }
    }

    fn remove_range(&mut self, range: Range<RawStackSlot>) {
        debug_assert!(range.start <= range.end);

        self.unsync = range.start;

        if self.transient < range.start {
        } else if self.transient < range.end {
            self.transient = range.start;
        } else {
            self.transient.0 -= range.end.0 - range.start.0;
        }
    }

    fn sync_on(&mut self, slot: RawStackSlot) {
        self.transient = slot;
        self.unsync = slot;
    }

    fn has_transient(&self, len: RawStackSlot) -> bool {
        self.transient < len
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
    root: RootCell<Vec<WeakValue<Ty>>>,

    /// Diff between main and rooted mirror.
    ///
    /// # Invariants
    ///
    /// ```ignore
    /// assert!(diff.unsync <= diff.transient);
    /// assert!(diff.transient <= main.len());
    /// assert!(diff.unsync <= root.len());
    /// ```
    diff: Diff,

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
    /// * It is relatively rare for upvalues to get evicted,
    ///    so most of the time it will boil down to shuffling `false` on and off.
    /// * It simplifies implementation of `TransientStackFrame` as it doesn't need to care about markers anymore.
    upvalue_mark: BitVec,

    /// Closures that keep upvalues on the stack.
    ///
    /// Keep weak references here:
    /// we only care about updating upvalues for alive closures
    /// so it doesn't matter whether closure is dropped or not.
    ///
    /// Upvalues referencing stack slots need to be updated when those are detached.
    closures: Vec<GcCell<Closure<Ty>>>,

    /// Upvalues that got detached from the stack,
    /// but that change was not yet propagated to affected closures.
    ///
    /// Those should never be directly accessed,
    /// a synchronization pass is required before those values become available to their respective closures.
    ///
    /// We only keep one value per stack slot.
    /// Constructing new closure requires allocation,
    /// therefore stack needs to be synced before that happens.
    /// It implies that any detached upvalues got updated.
    detached_upvalues: HashMap<RawStackSlot, WeakValue<Ty>>,
}

impl<Ty> Stack<Ty>
where
    Ty: CoreTypes,
{
    pub fn new(heap: &mut Heap<Ty>) -> Self {
        Stack {
            main: Default::default(),
            root: heap.alloc_cell(Default::default()),
            diff: Default::default(),
            upvalue_mark: Default::default(),
            closures: Default::default(),
            detached_upvalues: Default::default(),
        }
    }

    /// Synchronize real portion of the stack with rooted mirror.
    ///
    /// This function is guaranteed to not allocate.
    fn sync_stack_cache(&mut self, heap: &mut Heap<Ty>) {
        let mirror = &mut heap[&self.root];
        mirror.truncate(self.diff.unsync.0);
        mirror.extend_from_slice(self.main[self.diff.unsync..].as_ref());

        self.diff.sync_on(self.main.next_key());
    }

    /// Move detached upvalues to heap and update all affected closures.
    ///
    /// This function *may allocate*.
    /// You need to ensure that all objects that need to stay alive are properly rooted before calling this function.
    fn sync_upvalue_cache(&mut self, heap: &mut Heap<Ty>) {
        heap.pause(|heap| {
            let evicted_upvalues = std::mem::take(&mut self.detached_upvalues);

            let reallocated: HashMap<_, _> = evicted_upvalues
                .into_iter()
                .map(|(slot, value)| {
                    let value = heap.alloc_cell(value).downgrade();

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

    /// Move detached upvalues onto heap and update affected closures.
    ///
    /// Other bookkeeping (such as syncing stack with mirror) will be done if necessary
    /// but are not guaranteed to be performed.
    fn sync_upvalues(&mut self, heap: &mut Heap<Ty>) {
        if self.detached_upvalues.is_empty() {
            return;
        }

        if self.diff.has_transient(self.main.next_key()) {
            self.sync_stack_cache(heap);
        }

        self.sync_upvalue_cache(heap);
    }

    /// Sync the main portion of the stack and flush detached upvalues.
    pub(crate) fn sync(&mut self, heap: &mut Heap<Ty>) {
        if self.diff.has_transient(self.main.next_key()) {
            self.sync_stack_cache(heap);
        }

        if !self.detached_upvalues.is_empty() {
            self.sync_upvalue_cache(heap);
        }
    }

    fn register_closure(&mut self, closure_ref: &RootCell<Closure<Ty>>, heap: &Heap<Ty>) {
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
}

impl<Ty> Stack<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn guard(&mut self, boundary: RawStackSlot) -> Option<StackGuard<Ty>> {
        if self.len() < boundary {
            return None;
        }

        let r = StackGuard {
            stack: self,
            boundary,
        };

        Some(r)
    }

    pub(crate) fn full_guard(&mut self) -> StackGuard<Ty> {
        StackGuard {
            stack: self,
            boundary: RawStackSlot(0),
        }
    }

    fn push(&mut self, value: WeakValue<Ty>) -> RawStackSlot {
        debug_assert!(self.upvalue_mark.len() <= self.main.len());

        let slot = self.main.next_key();
        let is_transient = value.is_transient();
        self.diff.push(slot, is_transient);

        self.main.push_and_get_key(value)
    }

    fn set(&mut self, slot: RawStackSlot, value: WeakValue<Ty>) {
        if let Some(place) = self.main.get_mut(slot) {
            let is_transient = value.is_transient();
            self.diff.modify(slot, is_transient);

            *place = value;
        }
    }

    pub(crate) fn len(&self) -> RawStackSlot {
        RawStackSlot(self.main.len())
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
            .map(|slot| (slot, self.main[slot]));

        for (slot, value) in iter {
            let old_value = self.detached_upvalues.insert(slot, value);
            debug_assert!(old_value.is_none());
        }

        if start.0 < self.upvalue_mark.len() {
            self.upvalue_mark.truncate(start.0);
        }
    }

    fn insert(&mut self, slot: RawStackSlot, value: WeakValue<Ty>) {
        let is_transient = value.is_transient();
        self.diff.insert(slot, is_transient);

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

        self.evict_upvalues(start..);
        self.diff.remove_range(start..end);
        self.main.drain(start..end)
    }

    pub(crate) fn truncate(&mut self, slot: RawStackSlot) {
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
        self.main.extend(iter);
        let end = self.main.next_key();

        self.diff.extend(start..end, !is_rooted);
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
            .field("root", &self.root)
            .field("diff", &self.diff)
            .field("upvalue_mark", &self.upvalue_mark)
            .field("closures", &self.closures)
            .field("detached_upvalues", &self.detached_upvalues)
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
    // pub(super) fn guard(&mut self, protected_size: RawStackSlot) -> Option<StackGuard<'_, Ty>> {
    //     if self.stack.len() < protected_size.0 {
    //         return None;
    //     }

    //     let r = StackGuard {
    //         stack: self.stack,
    //         boundary: protected_size,
    //     };

    //     Some(r)
    // }

    pub(crate) fn reborrow(&mut self) -> StackGuard<'_, Ty> {
        let StackGuard { stack, boundary } = self;

        StackGuard {
            stack,
            boundary: *boundary,
        }
    }

    pub(crate) fn raw_guard_at(&mut self, slot: RawStackSlot) -> Option<StackGuard<Ty>> {
        if self.stack.len() < slot {
            return None;
        }

        let StackGuard { stack, boundary: _ } = self;

        let r = StackGuard {
            stack,
            boundary: slot,
        };

        Some(r)
    }

    pub fn guard_at(&mut self, slot: StackSlot) -> Option<StackGuard<Ty>> {
        let slot = self.boundary() + slot;
        self.raw_guard_at(slot)
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

        // Reset diff to boundary.
        self.stack.diff.modify(self.boundary, true);

        let StackGuard { stack, boundary } = self;

        TransientStackFrame {
            stack,
            boundary: *boundary,
        }
    }

    pub fn frame<'s>(&'s mut self, heap: &'s mut Heap<Ty>) -> StackFrame<'s, Ty> {
        StackFrame {
            guard: self.reborrow(),
            heap,
        }
    }

    pub fn as_slice(&self) -> &[WeakValue<Ty>] {
        self.stack.main[self.boundary..].raw.as_ref()
    }

    pub fn get(&self, slot: StackSlot) -> Option<&WeakValue<Ty>> {
        let slot = self.boundary + slot;
        self.get_raw(slot)
    }

    fn get_raw(&self, slot: RawStackSlot) -> Option<&WeakValue<Ty>> {
        self.stack.main.get(slot)
    }

    fn push(&mut self, value: WeakValue<Ty>) {
        self.stack.push(value);
    }

    fn set(&mut self, slot: StackSlot, value: WeakValue<Ty>) {
        let slot = self.boundary + slot;
        self.stack.set(slot, value)
    }

    pub fn next_slot(&self) -> StackSlot {
        self.stack.len() - self.boundary
    }

    fn insert(&mut self, slot: StackSlot, value: WeakValue<Ty>) {
        let slot = self.boundary + slot;
        self.stack.insert(slot, value)
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
        if self.stack.len() <= self.boundary {
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
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    pub fn emit_pretty(
        &self,
        writer: &mut impl std::fmt::Write,
        heap: &Heap<Ty>,
    ) -> Result<(), std::fmt::Error> {
        if self.is_empty() {
            return write!(writer, "[]");
        }

        writeln!(writer, "[")?;
        for (slot, value) in self.iter_enumerated() {
            let upvalue_mark = ' ';

            writeln!(
                writer,
                "    {upvalue_mark}[{slot:>2}] {value:#}",
                value = value.display(heap)
            )?;
        }

        writeln!(writer, "]")?;

        Ok(())
    }

    pub fn to_pretty_string(&self, heap: &Heap<Ty>) -> String {
        let mut r = String::new();
        self.emit_pretty(&mut r, heap).unwrap();
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
    /// Move detached upvalues to heap.
    ///
    /// Internally when a slot associated with upvalue is removed from stack
    /// it is not moved to heap immediately but placed into special cache.
    /// This function ensures that any upvalues stuck in this cache make their way to heap
    /// and are properly reattached to their respective closures.
    ///
    /// When it comes to upvalue handling there is one invariant that we need to enforce:
    ///
    /// * Upvalues on *existing* frames report correct memory locations.
    ///
    /// A nice part about this rule that after a frame is constructed,
    /// for that specific frame invariant will stay true as long as the frame exists.
    /// This is because any on-stack upvalues will be positioned somewhere on the stack below
    /// that frame's stack and are impossible to remove, ergo cannot be detached.
    /// So, travelling up the call stack cannot violate the invariant.
    ///
    /// However, entering new frames needs to be done with caution:
    /// closure used to construct the frame might have some of its upvalues still detached and stuck in the cache.
    ///
    /// Therefore, the only place where we must sync upvalue cache is inside [`Frame::new`](super::Frame::new).
    pub(super) fn sync_upvalues(&mut self, heap: &mut Heap<Ty>) {
        self.0.stack.sync_upvalues(heap)
    }

    /// Ensure that all values on the stack are rooted and move any detached upvalues to the heap.
    pub(super) fn sync(&mut self, heap: &mut Heap<Ty>) {
        self.0.stack.sync(heap);
    }

    pub(super) fn register_closure(
        &mut self,
        closure_ref: &RootCell<Closure<Ty>>,
        heap: &Heap<Ty>,
    ) {
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
        self.0.get(slot)
    }

    pub(super) fn get_raw_slot(&self, slot: RawStackSlot) -> Option<&WeakValue<Ty>> {
        self.0.get_raw(slot)
    }

    pub(super) fn push(&mut self, value: WeakValue<Ty>) {
        self.0.push(value)
    }

    pub(super) fn push_raw(&mut self, value: WeakValue<Ty>) {
        self.0.stack.push(value);
    }

    pub(super) fn set(&mut self, slot: StackSlot, value: WeakValue<Ty>) {
        self.0.set(slot, value)
    }

    pub(super) fn set_raw(&mut self, slot: RawStackSlot, value: WeakValue<Ty>) {
        self.0.stack.set(slot, value)
    }

    pub(super) fn next_slot(&self) -> StackSlot {
        self.0.next_slot()
    }

    pub(super) fn insert(&mut self, slot: StackSlot, value: WeakValue<Ty>) {
        self.0.insert(slot, value)
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

    pub(super) fn evict_upvalues(&mut self) {
        self.0.evict_upvalues(..)
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
                self.push(Value::Bool(value.to_bool()));
            }
            // Not-equal additionally needs to inverse the resulting boolean.
            Neq => {
                let _ = self.adjust_height(StackSlot(1));
                let value = self.pop().unwrap();
                self.push(Value::Bool(!value.to_bool()));
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
}

impl<'a, Ty> TransientStackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    pub fn sync(&mut self, heap: &mut Heap<Ty>) {
        // There are no on-stack upvalues by construction.
        self.stack.sync(heap)
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
    heap: &'a mut Heap<Ty>,
}

impl<'a, Ty> StackFrame<'a, Ty>
where
    Ty: CoreTypes,
{
    pub fn get_mut(&mut self, slot: StackSlot) -> Option<SlotProxy<'_, Ty>> {
        let value = *self.get(slot)?;
        let slot = self.boundary + slot;

        let r = SlotProxy {
            slot,
            value,
            stack: self.guard.stack,
        };

        Some(r)
    }

    pub fn push(&mut self, value: WeakValue<Ty>) {
        self.guard.push(value);
    }

    pub fn insert(&mut self, slot: StackSlot, value: WeakValue<Ty>) {
        self.guard.insert(slot, value)
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
        // Since frame holds mutable borrow to heap there are no possible gc passes that can happen.
        // We only need to sync stack when `StackFrame` goes out of scope.
        self.guard.stack.sync(self.heap);
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
        self.stack.set(self.slot, self.value.take());
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
