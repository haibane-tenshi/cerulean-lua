pub(crate) mod arena;
pub(crate) mod store;
pub(crate) mod userdata_store;

use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::{Index, IndexMut};

use bitvec::vec::BitVec;
use typed_index_collections::TiVec;

use crate::index::sealed_upgrade::Sealed;
use crate::index::{Allocated, Gc, GcCell, MutAccess, RefAccess, Root, RootCell, Rooted, Weak};
use crate::trace::Trace;
use crate::userdata::{Dispatcher, FullUserdata, Params, Userdata};
use arena::Arena;
use store::{Addr, Counter, Store};

/// Backing store for garbage-collected objects.
///
/// This type hosts all objects that are allocated within garbage collector.
/// Creating or dereferencing any objects requires access to heap:
///
/// ```
/// # use gc::{Heap};
/// let mut heap = Heap::new();
/// let a = heap.alloc(3_usize);
///
/// assert_eq!(heap[&a], 3);
/// ```
///
/// Allocated objects require to implement [`Trace`] trait
/// which allows garbage collector to discover transitive references through an object.
/// Note that `Trace` requires `'static`: this is due to internal use of [`Any`](std::any::Any)
/// so unfortunately you cannot allocate objects containing non-static references.
///
/// # Garbage collection
///
/// Garbage collection can be triggered automatically on [`alloc_cell`](Heap::alloc_cell), [`alloc`](Heap::alloc)
/// or manually via [`gc`](Heap::gc) method.
///
/// Implementation uses standard mark-and-sweep strategy.
pub struct Heap<M, P> {
    type_map: HashMap<TypeId, Indices>,
    arenas: TiVec<TypeIndex, Box<dyn Arena<M, P>>>,
    status: Status,
}

impl<M, P> Heap<M, P>
where
    P: Params,
{
    /// Construct fresh heap.
    pub fn new() -> Self {
        Default::default()
    }

    fn concrete_arena_or_insert<T>(&mut self) -> (TypeIndex, &mut dyn Arena<M, P>)
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();
        let indices = self.type_map.entry(id).or_default();
        let index = *indices
            .concrete
            .get_or_insert_with(|| self.arenas.push_and_get_key(Box::new(Store::<T>::new())));
        let store = self.arenas.get_mut(index).unwrap().as_mut();
        (index, store)
    }

    fn light_arena_or_insert<T>(&mut self) -> (TypeIndex, &mut dyn Arena<M, P>)
    where
        T: Trace,
    {
        use userdata_store::UserdataStore;

        let id = TypeId::of::<T>();
        let indices = self.type_map.entry(id).or_default();
        let index = *indices.userdata.get_or_insert_with(|| {
            self.arenas
                .push_and_get_key(Box::new(UserdataStore::<T, P>::new()))
        });

        let store = self.arenas.get_mut(index).unwrap().as_mut();
        (index, store)
    }

    fn full_arena_or_insert<T>(&mut self) -> (TypeIndex, &mut dyn Arena<M, P>)
    where
        M: Trace,
        T: Trace,
    {
        use userdata_store::FullUserdataStore;

        let id = TypeId::of::<T>();
        let indices = self.type_map.entry(id).or_default();
        let index = *indices.userdata.get_or_insert_with(|| {
            self.arenas
                .push_and_get_key(Box::new(FullUserdataStore::<T, M, P>::new()))
        });

        let store = self.arenas.get_mut(index).unwrap().as_mut();
        (index, store)
    }

    #[inline(never)]
    fn alloc_slow<T>(&mut self, ty: TypeIndex, value: T) -> Result<(Addr, Counter), T>
    where
        T: Trace,
    {
        self.gc_with(&value);
        self.arenas.get_mut(ty).unwrap().insert(value)
    }

    fn alloc_inner<T>(&mut self, ty: TypeIndex, value: T) -> Result<(Addr, Counter), T>
    where
        T: Trace,
    {
        let arena = self.arenas.get_mut(ty).unwrap();
        match arena.try_insert(value) {
            ptr @ Ok(_) => ptr,
            Err(value) => self.alloc_slow(ty, value),
        }
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
        let (ty, _) = self.concrete_arena_or_insert::<T>();
        let Ok((addr, counter)) = self.alloc_inner(ty, value) else {
            unreachable!()
        };

        RootCell::new(addr, ty, counter)
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
        let (ty, _) = self.concrete_arena_or_insert::<T>();
        let Ok((addr, counter)) = self.alloc_inner(ty, value) else {
            unreachable!()
        };

        Root::new(addr, ty, counter)
    }

    /// Allocate an object as light userdata.
    ///
    /// Userdata is mechanism for Lua to represent foreign types.
    /// Every userdata requires three key pieces to function:
    /// object itself, method dispatcher function and metatable.
    ///
    /// Method dispatcher function translates Lua function invocations into actual Rust function calls.
    /// By design all values of the same type share dispatcher function
    /// and it can be set using [`Heap::set_dispatcher`].
    /// When unset default dispatcher (which does nothing) will be used.
    ///
    /// Note that **light** userdata does not carry its own metatable!
    /// It is assumed that appropriate metatable will be provided by an external source.
    /// In case of our Lua runtime that is done through metatable registry.
    pub fn alloc_userdata<T>(&mut self, value: T) -> Root<dyn Userdata<P>>
    where
        T: Trace,
    {
        let (ty, _) = self.light_arena_or_insert::<T>();
        let Ok((addr, counter)) = self.alloc_inner(ty, value) else {
            unreachable!()
        };

        Root::new(addr, ty, counter)
    }

    pub fn alloc_full_userdata<T>(
        &mut self,
        value: T,
        metatable: Option<M>,
    ) -> Root<dyn FullUserdata<M, P>>
    where
        T: Trace,
        M: Trace,
    {
        let (ty, _) = self.full_arena_or_insert::<T>();
        let Ok((addr, counter)) = self.alloc_inner(ty, value) else {
            unreachable!()
        };

        let temp = GcCell::<dyn FullUserdata<M, P>>::new(addr, ty);
        let value = self.get_mut(temp).unwrap();
        let _ = value.set_metatable(metatable);

        Root::new(addr, ty, counter)
    }

    /// Get `&T` out of weak reference such as [`GcCell`] or [`Gc`].
    ///
    /// This function will also accept [`RootCell`] and [`Root`],
    /// but you might want to use [`get_root`](Heap::get_root) method instead.
    ///
    /// Returned reference is wrapped in `Option`
    /// because it is possible that the object behind weak reference was since deallocated.
    ///
    /// # Panic
    ///
    /// This function never panics.
    ///
    /// Result of dereferencing a reference created from a different heap is unspecified but *safe*.
    /// You are likely to get `None` but it might return a reference if a suitable object is found.
    pub fn get<T>(&self, ptr: impl RefAccess<T>) -> Option<&T>
    where
        T: Allocated<M, P> + ?Sized,
    {
        use crate::index::sealed_allocated::{Addr, ArenaRef, Sealed};

        let ty = ptr.type_index().0;
        let addr = ptr.addr().0;

        let arena = self.arenas.get(ty)?.as_ref();

        <T as Sealed<M, P>>::get_ref(ArenaRef(arena), Addr(addr))
    }

    /// Get `&mut T` out of weak reference such as [`GcCell`].
    ///
    /// This function will also accept [`RootCell`],
    /// but you might want to use [`get_root_mut`](Heap::get_root_mut) method instead.
    ///
    /// Returned reference is wrapped in `Option`
    /// because it is possible that the object behind weak reference was since deallocated.
    ///
    /// # Panic
    ///
    /// This function never panics.
    ///
    /// Result of dereferencing a reference created from a different heap is unspecified but *safe*.
    /// You are likely to get `None` but it might return a reference if a suitable object is found.
    pub fn get_mut<T>(&mut self, ptr: impl MutAccess<T>) -> Option<&mut T>
    where
        T: Allocated<M, P> + ?Sized,
    {
        use crate::index::sealed_allocated::{Addr, ArenaMut, Sealed};

        let ty = ptr.type_index().0;
        let addr = ptr.addr().0;

        let arena = self.arenas.get_mut(ty)?.as_mut();

        <T as Sealed<M, P>>::get_mut(ArenaMut(arena), Addr(addr))
    }

    /// Get `&T` out of strong reference such as [`RootCell`] or [`Root`].
    ///
    /// Method returnes reference directly
    /// because strong references prevent object from being deallocated.
    ///
    /// # Panic
    ///
    /// This function panics if `ptr` is created from a different heap
    /// but otherwise it never will.
    pub fn get_root<T>(&self, ptr: impl RefAccess<T> + Rooted) -> &T
    where
        T: Allocated<M, P> + ?Sized,
    {
        let ty = ptr.type_index().0;
        let counter = ptr.counter().0;

        match self.arenas.get(ty) {
            Some(arena) if arena.validate(counter) => (),
            _ => panic!("attempt to dereference a pointer created from a different heap"),
        };

        self.get(ptr)
            .expect("rooted object should never be deallocated")
    }

    /// Get `&mut T` out of strong reference such as [`RootCell`].
    ///
    /// Method returnes reference directly
    /// because strong references prevent object from being deallocated.
    ///
    /// # Panic
    ///
    /// This function panics if `ptr` is created from a different heap
    /// but otherwise it never will.
    pub fn get_root_mut<T>(&mut self, ptr: impl MutAccess<T> + Rooted) -> &mut T
    where
        T: Allocated<M, P> + ?Sized,
    {
        let ty = ptr.type_index().0;
        let counter = ptr.counter().0;

        match self.arenas.get(ty) {
            Some(arena) if arena.validate(counter) => (),
            _ => panic!("attempt to dereference a pointer created from a different heap"),
        };

        self.get_mut(ptr)
            .expect("rooted object should never be deallocated")
    }

    /// Upgrade weak reference ([`Gc`]/[`GcCell`]) into corresponding strong reference ([`Root`]/[`RootCell`]).
    ///
    /// The function will return `None` if object was since deallocated.
    pub fn upgrade<Ptr>(&self, ptr: Ptr) -> Option<<Ptr as Sealed>::Target>
    where
        Ptr: Weak,
    {
        use crate::index::sealed_upgrade::Counter;

        let ty = ptr.type_index().0;
        let addr = ptr.addr().0;

        let arena = self.arenas.get(ty)?;
        let counter = arena.upgrade(addr)?;
        let ptr = ptr.upgrade(Counter(counter));
        Some(ptr)
    }

    /// Set dispatcher method for the type.
    ///
    /// By design all values of the same type share dispacher function.
    /// This change will affect all future and existing userdata of this type.
    pub fn set_dispatcher<T>(&mut self, dispatcher: Dispatcher<T, P>)
    where
        T: Trace,
    {
        let (_, arena) = self.light_arena_or_insert::<T>();
        arena.set_dispatcher(&dispatcher)
    }

    fn collector(&self) -> (Collector, Collector) {
        let masks = self.arenas.iter().map(|arena| arena.roots()).collect();

        let queue = Collector { masks };

        let masks = queue
            .masks
            .iter()
            .map(|mask| BitVec::repeat(false, mask.len()))
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
            for (id, arena) in self.arenas.iter_enumerated().cycle() {
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

        for (id, arena) in self.arenas.iter_mut_enumerated() {
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

impl<M, P> Default for Heap<M, P> {
    fn default() -> Self {
        Self {
            type_map: Default::default(),
            arenas: Default::default(),
            status: Default::default(),
        }
    }
}

impl<M, P> Debug for Heap<M, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Heap").finish_non_exhaustive()
    }
}

impl<T, M, P> Index<&RootCell<T>> for Heap<M, P>
where
    T: Trace,
    P: Params,
{
    type Output = T;

    fn index(&self, index: &RootCell<T>) -> &Self::Output {
        self.get_root(index)
    }
}

impl<T, M, P> Index<&Root<T>> for Heap<M, P>
where
    T: Trace,
    P: Params,
{
    type Output = T;

    fn index(&self, index: &Root<T>) -> &Self::Output {
        self.get_root(index)
    }
}

impl<T, M, P> IndexMut<&RootCell<T>> for Heap<M, P>
where
    T: Trace,
    P: Params,
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
    masks: TiVec<TypeIndex, BitVec>,
}

impl Collector {
    /// Mark an object as reachable.
    pub fn mark_cell<T>(&mut self, ptr: GcCell<T>)
    where
        T: Trace,
    {
        let Some(mask) = self.masks.get_mut(ptr.ty()) else {
            return;
        };

        // This is slightly incorrect.
        // It is possible that weak reference we got here is dead and
        // location it points to was reallocated for another object.
        // This marks the new object as reachable which sometimes may cause it live longer that expected.
        // It *probably* doesn't matter much: we shouldn't receive dead references here
        // unless there are horribly wrong `Trace` implementations or deliberately malformed input.
        // Solutions are either to check every object for liveness (which requires heap reference),
        // or track what was the latest generation that was reached in every slot.
        let Some(mut bit) = mask.get_mut(ptr.addr().index()) else {
            return;
        };

        *bit = true;
    }

    /// Mark an object as reachable.
    pub fn mark<T>(&mut self, ptr: Gc<T>)
    where
        T: Trace,
    {
        let Some(mask) = self.masks.get_mut(ptr.ty()) else {
            return;
        };

        // This is slightly incorrect.
        // It is possible that weak reference we got here is dead and
        // location it points to was reallocated for another object.
        // This marks the new object as reachable which sometimes may cause it live longer that expected.
        // It *probably* doesn't matter much: we shouldn't receive dead references here
        // unless there are horribly wrong `Trace` implementations or deliberately malformed input.
        // Solutions are either to check every object for liveness (which requires heap reference),
        // or track what was the latest generation that was reached in every slot.
        let Some(mut bit) = mask.get_mut(ptr.addr().index()) else {
            return;
        };

        *bit = true;
    }

    fn take(&mut self, index: TypeIndex) -> Option<BitVec> {
        let mask = self.masks.get_mut(index)?;

        let empty = BitVec::repeat(false, mask.len());
        let mask = std::mem::replace(mask, empty);

        Some(mask)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub(crate) struct TypeIndex(usize);

impl From<usize> for TypeIndex {
    fn from(value: usize) -> Self {
        TypeIndex(value)
    }
}

impl From<TypeIndex> for usize {
    fn from(value: TypeIndex) -> Self {
        let TypeIndex(value) = value;
        value
    }
}

#[derive(Debug, Default)]
pub(crate) struct Indices {
    concrete: Option<TypeIndex>,
    userdata: Option<TypeIndex>,
}
