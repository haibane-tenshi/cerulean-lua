use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::{Index, IndexMut};

use bitvec::vec::BitVec;

use crate::arena::{Arena, ArenaStore};
use crate::trace::Trace;
use crate::{Gc, GcCell, MutAccess, RefAccess, Root, RootCell, Rooted};

/// Backing store for garbage-collected objects.
///
/// This type hosts all objects that are allocated within garbage collector.
/// Creating or dereferencing any objects requires access to heap:
///
/// ```
/// # use gc::{Heap};
/// let mut heap = Heap::new();
/// let a = heap.alloc_cell(3_usize);
///
/// assert_eq!(heap[&a], 3);
/// ```
///
/// Garbage collection can be triggered automatically on [`alloc_cell`](Heap::alloc_cell), [`alloc`](Heap::alloc)
/// or manually via [`gc`](Heap::gc) method.
/// We are using standard mark-and-sweep strategy.
///
/// Newely allocated objects return [`RootCell<T>`](RootCell)/[`Root<T>`](Root) which is a strong reference,
/// but can be downgraded into [`GcCell<T>`](GcCell)/[`Gc<T>`](Gc) which is a weak reference.
///
/// Result of dereferencing a pointer constructed in a different heap is unspecified,
/// but guaranteed to be *safe*.
/// It may or may not return a valid reference if an object is found at the location
/// or invoke panic.
#[derive(Default)]
pub struct Heap {
    arenas: HashMap<TypeId, Box<dyn Arena>>,
    status: Status,
}

impl Heap {
    /// Construct fresh heap.
    pub fn new() -> Self {
        Default::default()
    }

    fn arena<T>(&self) -> Option<&ArenaStore<T>>
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();
        let arena = self.arenas.get(&id)?;
        arena.as_any().downcast_ref()
    }

    fn arena_mut<T>(&mut self) -> Option<&mut ArenaStore<T>>
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();
        let arena = self.arenas.get_mut(&id)?;
        arena.as_any_mut().downcast_mut()
    }

    fn arena_or_insert<T>(&mut self) -> &mut ArenaStore<T>
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();
        let arena = self
            .arenas
            .entry(id)
            .or_insert_with(|| Box::new(ArenaStore::<T>::new()));
        arena.as_any_mut().downcast_mut().unwrap()
    }

    /// Allocate an object.
    ///
    /// This function can potentially trigger a garbage collection phase.
    ///
    /// The object can later be mutated through any reference as if it was wrapped in `RefCell`.
    /// If this behavior is undesirable consider using [`alloc`](Heap::alloc) instead.
    ///
    /// The function immediately roots the object it allocates so you don't need to worry
    /// that inner references will get dropped if gc is triggered as part of the call.
    pub fn alloc_cell<T>(&mut self, value: T) -> RootCell<T>
    where
        T: Trace,
    {
        let arena = self.arena_or_insert();
        match arena.try_insert(value) {
            Ok(ptr) => ptr,
            Err(value) => self.alloc_slow(value),
        }
    }

    /// Allocate an object.
    ///
    /// This function can potentially trigger a garbage collection phase.
    ///
    /// It is not possible to acquire mutable reference to the allocated object.
    /// If you want to mutate the object later consider using [`alloc_cell`](Heap::alloc_cell)
    /// instead of wrapping it in `Cell` or `RefCell`.
    ///
    /// The function immediately roots the object it allocates so you don't need to worry
    /// that inner references will get dropped if gc is triggered as part of the call.
    pub fn alloc<T>(&mut self, value: T) -> Root<T>
    where
        T: Trace,
    {
        let RootCell {
            addr,
            counter,
            _marker,
        } = self.alloc_cell(value);

        Root {
            addr,
            counter,
            _marker,
        }
    }

    #[inline(never)]
    fn alloc_slow<T>(&mut self, value: T) -> RootCell<T>
    where
        T: Trace,
    {
        self.gc_with(&value);
        self.arena_mut().unwrap().insert(value)
    }

    /// Get `&T` out of weak reference such as [`GcCell`].
    ///
    /// Returned reference is wrapped in `Option`
    /// because it is possible that the object since was deallocated.
    pub fn get<T>(&self, ptr: impl RefAccess<T>) -> Option<&T>
    where
        T: Trace,
    {
        self.arena()?.get(ptr.addr())
    }

    /// Get `&mut T` out of weak reference such as [`GcCell`].
    ///
    /// Returned reference is wrapped in `Option`
    /// because it is possible that the object since was deallocated.
    pub fn get_mut<T>(&mut self, ptr: impl MutAccess<T>) -> Option<&mut T>
    where
        T: Trace,
    {
        self.arena_mut()?.get_mut(ptr.addr())
    }

    /// Get `&T` out of strong reference such as [`RootCell`].
    ///
    /// Method returnes reference directly
    /// because strong references prevent object from being deallocated.
    pub fn get_root<T>(&self, ptr: &(impl RefAccess<T> + Rooted)) -> &T
    where
        T: Trace,
    {
        self.arena()
            .and_then(|arena| arena.get(ptr.addr()))
            .expect("rooted object was deallocated")
    }

    /// Get `&mut T` out of strong reference such as [`RootCell`].
    ///
    /// Method returnes reference directly
    /// because strong references prevent object from being deallocated.
    pub fn get_root_mut<T>(&mut self, ptr: &(impl MutAccess<T> + Rooted)) -> &mut T
    where
        T: Trace,
    {
        self.arena_mut()
            .and_then(|arena| arena.get_mut(ptr.addr()))
            .expect("rooted object was deallocated")
    }

    /// Upgrade weak reference into strong reference.
    ///
    /// The function will return `None` if object was since deallocated.
    pub fn upgrade_cell<T>(&self, ptr: GcCell<T>) -> Option<RootCell<T>>
    where
        T: Trace,
    {
        let GcCell { addr, _marker } = ptr;

        let counter = self.arena::<T>()?.upgrade(addr)?;

        let r = RootCell {
            addr,
            counter,
            _marker,
        };

        Some(r)
    }

    /// Upgrade weak reference into strong reference.
    ///
    /// The function will return `None` if object was since deallocated.
    pub fn upgrade<T>(&self, ptr: Gc<T>) -> Option<Root<T>>
    where
        T: Trace,
    {
        let Gc { addr, _marker } = ptr;

        let counter = self.arena::<T>()?.upgrade(addr)?;

        let r = Root {
            addr,
            counter,
            _marker,
        };

        Some(r)
    }

    fn collector(&self) -> (Collector, Collector) {
        let masks = self
            .arenas
            .iter()
            .map(|(key, arena)| {
                let enqueued = arena.roots();

                (*key, enqueued)
            })
            .collect();

        let queue = Collector { masks };

        let masks = queue
            .masks
            .iter()
            .map(|(key, mask)| {
                let processed = BitVec::repeat(false, mask.len());
                (*key, processed)
            })
            .collect();

        let processed = Collector { masks };

        (queue, processed)
    }

    /// Trigger garbage collection phase.
    pub fn gc(&mut self) {
        self.gc_with(&())
    }

    fn gc_with(&mut self, keep_alive: &dyn Trace) {
        if !self.status.is_enabled() {
            self.status.trigger_gc();
            return;
        }

        let processed = {
            let (mut queue, mut processed) = self.collector();
            keep_alive.trace(&mut queue);

            let mut last_missed_id = None;
            for (id, arena) in self.arenas.iter().cycle() {
                let mut enqueued = queue.take(id).unwrap();
                let processed = processed.masks.get_mut(id).unwrap();

                if !enqueued.any() || {
                    // Filter the bits that are not yet processed.
                    // In principle this should be
                    //
                    //     enqueued = enqueued & !processed
                    //
                    // But we invert queue to avoid cloning processed bitvec.
                    enqueued = !enqueued;
                    enqueued |= processed.as_bitslice();

                    // Queue is still inverted here.
                    enqueued.all()
                } {
                    match last_missed_id {
                        Some(prev_id) if id == prev_id => break,
                        Some(_) => (),
                        None => last_missed_id = Some(id),
                    }
                    continue;
                } else {
                    last_missed_id = None
                }

                // Invert queue back to normal.
                let enqueued = !enqueued;
                *processed |= &enqueued;

                arena.trace(&enqueued, &mut queue);
            }

            processed
        };

        for (id, arena) in self.arenas.iter_mut() {
            let mask = processed.masks.get(id).unwrap();
            arena.retain(mask);
        }
    }

    /// Execute closure with paused garbage collection.
    pub fn pause<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let was_running = self.status.is_running();

        if was_running {
            self.status.pause();
        }

        let r = f(self);

        if was_running && self.status.unpause() {
            self.gc();
        }

        r
    }
}

impl Debug for Heap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Heap").finish_non_exhaustive()
    }
}

impl<T> Index<&RootCell<T>> for Heap
where
    T: Trace,
{
    type Output = T;

    fn index(&self, index: &RootCell<T>) -> &Self::Output {
        self.get_root(index)
    }
}

impl<T> Index<&Root<T>> for Heap
where
    T: Trace,
{
    type Output = T;

    fn index(&self, index: &Root<T>) -> &Self::Output {
        self.get_root(index)
    }
}

impl<T> IndexMut<&RootCell<T>> for Heap
where
    T: Trace,
{
    fn index_mut(&mut self, index: &RootCell<T>) -> &mut Self::Output {
        self.get_root_mut(index)
    }
}

#[derive(Debug, Clone, Copy)]
enum PauseStatus {
    Resumed,
    Paused,
    PausedPendingGc,
}

impl PauseStatus {
    fn is_running(self) -> bool {
        match self {
            PauseStatus::Paused | PauseStatus::PausedPendingGc => false,
            PauseStatus::Resumed => true,
        }
    }

    fn pending_gc(self) -> bool {
        match self {
            PauseStatus::Paused | PauseStatus::Resumed => false,
            PauseStatus::PausedPendingGc => true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Status {
    is_enabled: bool,
    pause: PauseStatus,
}

impl Status {
    fn is_enabled(self) -> bool {
        self.pause.is_running() && self.is_enabled
    }

    fn is_running(self) -> bool {
        self.pause.is_running()
    }

    fn pause(&mut self) {
        self.pause = PauseStatus::Paused;
    }

    fn unpause(&mut self) -> bool {
        let r = self.pause.pending_gc();
        self.pause = PauseStatus::Resumed;

        r
    }

    fn trigger_gc(&mut self) {
        if !self.is_running() {
            self.pause = PauseStatus::PausedPendingGc;
        }
    }
}

impl Default for Status {
    fn default() -> Self {
        Status {
            is_enabled: true,
            pause: PauseStatus::Resumed,
        }
    }
}

/// Intermediary keeping track of visited objects during [`Trace`]ing.
pub struct Collector {
    masks: HashMap<TypeId, BitVec>,
}

impl Collector {
    /// Mark an object as reachable.
    pub fn mark_cell<T>(&mut self, ptr: GcCell<T>)
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();

        let Some(mask) = self.masks.get_mut(&id) else {
            return;
        };

        let Some(mut bit) = mask.get_mut(ptr.addr.index) else {
            return;
        };

        *bit = true;
    }

    /// Mark an object as reachable.
    pub fn mark<T>(&mut self, ptr: Gc<T>)
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();

        let Some(mask) = self.masks.get_mut(&id) else {
            return;
        };

        let Some(mut bit) = mask.get_mut(ptr.addr.index) else {
            return;
        };

        *bit = true;
    }

    fn take(&mut self, id: &TypeId) -> Option<BitVec> {
        let mask = self.masks.get_mut(id)?;

        let empty = BitVec::repeat(false, mask.len());
        let mask = std::mem::replace(mask, empty);

        Some(mask)
    }
}
