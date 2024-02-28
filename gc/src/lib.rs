//! Entirely safe generic garbage collector implementation.
//!
//! This crate contains no unsafe code.
//!
//! Design of this garbage collector is heavily inspired by [`safe-rs`][docs.rs:safe-rs].
//!
//! [docs.rs:safe-rs]: https://docs.rs/safe-gc/latest/safe_gc/
//!
//! # Overview
//!
//! [`Heap`] is the core type which manages memory and allocated objects.
//! The memory itself is represented by a vec with potential "holes" left after deallocated objects.
//!
//! There are two reference types:
//!
//! * [`RootCell<T>`](RootCell) is a *strong* reference which keeps the pointed-to object alive.
//!
//!     Internally `RootCell`s are counted which makes them similar to [`Rc`].
//!     Because of this roots implement `Clone` but not `Copy`.
//!
//!     You are expected to keep roots outside of the heap so it doesn't implement [`Trace`].
//!
//! * [`GcCell<T>`](GcCell) is a *weak* reference.
//!     It doesn't prevent pointed-to object from being deallocated unless it can trace to a root.
//!
//!     `GcCell` is implemented as a lightweight index and is `Copy`able
//!     which makes it different from [`Weak`](std::rc::Weak) references.
//!
//!     To keep behavior consistent `GcCell`s also contain *generation* tag,
//!     so weak reference will dangle once the object is deallocated
//!     even if another object is later placed in exact same memory.
//!
//! Tracing of weak references is expressed via [`Trace`] trait.
//! An object is required to implement `Trace` in order to get allocated inside heap.
//!
//! The trait is *safe* to implement.
//! There is no memory unsafety or undefined behavior incorrect implementation may cause:
//! this crate uses no unsafe code.
//! It doesn't mean correctness is irrelevant -
//! missed objects will be considered garbage and potentially collected while still in use.
//!
//! The primary limitation of purely safe approach is how allocated objects are accessed.
//! Dereferencing our pointers *requires access to heap*:
//!
//! ```
//! # use gc::{Heap};
//! # let mut heap = Heap::new();
//! let ptr = heap.alloc_cell(3_usize);
//!
//! // Need heap to dereference
//! assert_eq!(heap[&ptr], 3);
//! ```
//!
//! With this it might be better to imagine `Heap` as a sort of map (analogous to `HashMap` or `BTreeMap`)
//! of reference-counted `(GcCell<T>, T)` pairs.
//!
//! ## Differences from `safe-rs`
//!
//! * Dereferencing weak pointers returns `Option<&T>`.
//!     It is expected that lookup might fail and produce no reference instead of panicking.
//! * Some internal implementation details are quite different
//!     in particular handling of strong references and presence of generation tag.
//! * Overall API surface is more developed due to its actual use in a Lua interpreter.
//!
//! # Quick start
//!
//! You need to implement [`Trace`] trait for your type so that gc can traverse it:
//!
//! ```
//! use gc::{GcCell, Trace, Collector};
//!
//! #[derive(Default)]
//! struct BinaryTree {
//!     left: Option<GcCell<Self>>,
//!     right: Option<GcCell<Self>>,
//! }
//!
//! impl Trace for BinaryTree {
//!     fn trace(&self, collector: &mut Collector) {
//!         let BinaryTree {
//!             left,
//!             right
//!         } = self;
//!
//!         left.trace(collector);
//!         right.trace(collector);
//!     }
//! }
//! ```
//!
//! Currently there is no derive macro to do it for you yet.
//!
//! Now objects of this type can be allocated:
//!
//! ```
//! # use gc::{GcCell, Trace, Collector};
//! #
//! # #[derive(Default)]
//! # struct BinaryTree {
//! #     left: Option<GcCell<Self>>,
//! #     right: Option<GcCell<Self>>,
//! # }
//! #
//! # impl Trace for BinaryTree {
//! #     fn trace(&self, collector: &mut Collector) {
//! #         let BinaryTree {
//! #             left,
//! #             right
//! #         } = self;
//! #
//! #         left.trace(collector);
//! #         right.trace(collector);
//! #     }
//! # }
//! use gc::Heap;
//!
//! let mut heap = Heap::new();
//!
//! let a = heap.alloc_cell(BinaryTree::default());
//! let b = heap.alloc_cell(BinaryTree::default());
//!
//! heap[&a].left = Some(b.downgrade());
//! heap[&b].right = Some(b.downgrade());
//!
//! // Oops, this is no longer a tree :(
//! heap[&b].left = Some(a.downgrade());
//! ```
//!
//! [`Heap::alloc_cell`] returns a [`RootCell`] - a strong reference that prevents object from being deallocated.
//! You can downgrade them into weak references - [`GcCell`],
//! but if an object is not reachable from one of the roots it will be considered garbage and collected:
//!
//! ```
//! # use gc::{Heap, GcCell};
//! # let mut heap = Heap::new();
//! # let a = heap.alloc_cell(3_usize);
//! # let b = heap.alloc_cell(3_usize);
//! // Drop strong references leaving only weak ones.
//! let weak_a: GcCell<_> = a.downgrade();
//! let weak_b: GcCell<_> = b.downgrade();
//!
//! drop(a);
//! drop(b);
//!
//! heap.gc();
//!
//! assert!(matches!(heap.get(weak_a), None));
//! assert!(matches!(heap.get(weak_b), None));
//! ```
//!
//! Notice how dereferencing weak pointer returns `Option<&T>` and not `&T` directly.
//! It is expected that weak references might get deallocated while you are not looking.
//!
//! # Common pitfalls: constructing complex objects
//!
//! Garbage collection triggered by [`Heap::alloc_cell`] may happen during construction
//! of a complex object.
//! Any non-rooted `GcCell`s inside such an object may be spuriously deallocated
//! before the object is placed into heap.
//!
//! ```rust,should_panic
//! # use gc::{Heap, Trace, Collector, GcCell};
//! #
//! struct Complex {
//!     a: GcCell<u64>,
//!     b: GcCell<i64>,
//! }
//!
//! # impl Trace for Complex {
//! #     fn trace(&self, collector: &mut Collector) {
//! #         let Complex { a, b, } = self;
//! #         a.trace(collector);
//! #         b.trace(collector);
//! #     }
//! # }
//! #
//! # let mut heap = Heap::new();
//! #
//! let a = heap.alloc_cell(3).downgrade();
//!
//! // Oops, this allocation happened to trigger garbage collection.
//! // `a` is not rooted and will be deallocated.
//! # heap.gc();
//! let b = heap.alloc_cell(-3).downgrade();
//!
//! let complex = heap.alloc_cell(Complex {
//!     a,
//!     b,
//! });
//!
//! assert_eq!(heap.get(heap[&complex].a), Some(&3), "a is dropped :(");
//! ```
//!
//! To avoid this issue you can temporarily pause garbage collection using [`Heap::pause`]:
//!
//! ```
//! # use gc::{Heap, Trace, Collector, GcCell};
//! #
//! # struct Complex {
//! #     a: GcCell<u64>,
//! #     b: GcCell<i64>,
//! # }
//! #
//! # impl Trace for Complex {
//! #     fn trace(&self, collector: &mut Collector) {
//! #         let Complex { a, b, } = self;
//! #         a.trace(collector);
//! #         b.trace(collector);
//! #     }
//! # }
//! #
//! # let mut heap = Heap::new();
//! #
//! let weak = heap.alloc_cell('a').downgrade();
//!
//! // Garbage allocation will be paused inside the closure.
//! let complex = heap.pause(|heap| {
//!     let a = heap.alloc_cell(3).downgrade();
//!  
//!     // Even manual triggers are paused.   
//!     heap.gc();
//!     let b = heap.alloc_cell(-3).downgrade();
//!
//!     heap.alloc_cell(Complex {
//!         a,
//!         b,
//!     })
//! }); // If any gc pass was triggered it will happen after closure have exited.
//!     // `weak` have no roots left so it will be collected here.
//!
//! assert_eq!(heap.get(heap[&complex].a), Some(&3), "a is dropped :(");
//! assert_eq!(heap.get(heap[&complex].b), Some(&-3), "b is dropped :(");
//! assert_eq!(heap.get(weak), None);
//! ```
//!
//! Also [`Heap::alloc_cell`] immediately roots the object it allocates so you don't need to worry
//! that inner references will get dropped if gc is triggered as part of the call.

#![forbid(unsafe_code)]

mod arena;
mod trace;
mod vec_list;

use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::{Debug, Pointer};
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use bitvec::vec::BitVec;

use arena::{Arena, ArenaStore, Counter};

pub use trace::{Trace, Untrace};

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
/// Garbage collection can be triggered automatically on [`Heap::alloc_cell`]
/// or manually via [`Heap::gc`].
/// We are using standard mark-and-sweep strategy.
///
/// Newely allocated objects return [`RootCell<T>`](RootCell) which is a strong reference,
/// but can be downgraded into [`GcCell<T>`](GcCell) which is a weak reference.
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

    #[inline(never)]
    fn alloc_slow<T>(&mut self, value: T) -> RootCell<T>
    where
        T: Trace,
    {
        self.gc_with(&value);
        self.arena_mut().unwrap().insert(value)
    }

    /// Get `&T` out of weak reference.
    ///
    /// [`GcCell`] is a weak reference so it is possible that the object since was deallocated.
    pub fn get<T>(&self, ptr: GcCell<T>) -> Option<&T>
    where
        T: Trace,
    {
        self.arena()?.get(ptr.addr)
    }

    /// Get `&mut T` out of weak reference.
    ///
    /// [`GcCell`] is a weak reference so it is possible that the object since was deallocated.
    pub fn get_mut<T>(&mut self, ptr: GcCell<T>) -> Option<&mut T>
    where
        T: Trace,
    {
        self.arena_mut()?.get_mut(ptr.addr)
    }

    /// Get `&T` out of strong reference.
    ///
    /// [`RootCell`] is a strong reference and prevents objects from being deallocated.
    pub fn get_root<T>(&self, ptr: &RootCell<T>) -> &T
    where
        T: Trace,
    {
        self.arena()
            .and_then(|arena| arena.get(ptr.addr))
            .expect("rooted object was deallocated")
    }

    /// Get `&T` out of strong reference.
    ///
    /// [`RootCell`] is a strong reference and prevents objects from being deallocated.
    pub fn get_root_mut<T>(&mut self, ptr: &RootCell<T>) -> &mut T
    where
        T: Trace,
    {
        self.arena_mut()
            .and_then(|arena| arena.get_mut(ptr.addr))
            .expect("rooted object was deallocated")
    }

    /// Upgrade weak reference into strong reference.
    ///
    /// The function will return `None` if object was since deallocated.
    pub fn upgrade<T>(&self, ptr: GcCell<T>) -> Option<RootCell<T>>
    where
        T: Trace,
    {
        self.arena()?.upgrade(ptr)
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
    pub fn mark<T>(&mut self, ptr: GcCell<T>)
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

/// A weak reference to gc-allocated value.
///
/// This reference is an equivalent of `Weak<RefCell<T>>`:
///
/// * The reference is *weak* as in it alone won't prevent value from being dropped.
///     However when part of certainly alive object
///     it will indicate that referencee is also alive through [`Trace`] trait.
///
/// * Internal value can be mutated through any existing reference.
///     In this regard it behaves as if it was wrapped in `RefCell` - hence `Cell` in the name.
///
///     Note that there is no actual internal mutability involved:
///     to acquire `&T` or `&mut T` you need to borrow from heap
///     so normal borrow checker rules ensure that aliasing rules are not violated.
///
/// Unlike [`RootCell`] this type implements [`Copy`].
///
/// Even though it claims to be a reference type there is no way to [dereference](#dereference)
/// `GcCell` directly into `&T`.
/// As such it provides none of the related traits that you might expect like
/// `Deref<Target = T>` or `AsRef<T>`.
/// Also `GcCell` doesn't implement `PartialEq`, `PartialOrd` or `Hash` to avoid ambiguity.
/// Normal references and smart pointers defer implementations of those traits to referenced value
/// which is not possible for the type.
///
/// If you are looking for a way to provide equivalents of `Eq`, `Ord` or `Hash`
/// as if applied to an underlying pointer consider using [`GcCell::addr`].
///
/// # Construct
///
/// Common way to construct [`GcCell`] is to downgrade [`RootCell`]:
///
/// ```
/// # use gc::{Heap, GcCell, RootCell};
/// # let mut heap = Heap::new();
/// let strong = heap.alloc_cell(3);
/// let weak: GcCell<usize> = strong.downgrade();
/// ```
///
/// Converting `GcCell` back to `RootCell` requires access to heap:
///
/// ```
/// # use gc::{Heap, GcCell, RootCell};
/// # let mut heap = Heap::new();
/// # let weak: GcCell<usize> = heap.alloc_cell(3).downgrade();
/// let strong: RootCell<usize> = heap.upgrade(weak).expect("object is still alive");
/// ```
///
/// # Dereference
///
/// Recovering reference to allocated object requires access to heap.
/// Since `GcCell` doesn't guarantee that an object stays alive dereference returns `Option`:
///
/// ```
/// # use gc::{Heap, GcCell, RootCell};
/// # let mut heap = Heap::new();
/// let weak = heap.alloc_cell(3_usize).downgrade();
/// assert_eq!(heap.get(weak), Some(&3));
///
/// // Object have no strong references left so it will be collected.
/// heap.gc();
/// assert_eq!(heap.get(weak), None);
/// ```
///
/// You can also recover mutable reference although it requires *mutable* access to heap:
///
/// ```
/// # use gc::{Heap, GcCell, RootCell};
/// # let mut heap = Heap::new();
/// let weak = heap.alloc_cell(3_usize).downgrade();
/// assert_eq!(heap.get(weak), Some(&3));
///
/// let ref_mut: &mut usize = heap.get_mut(weak).expect("object is still alive");
/// *ref_mut = 4;
/// assert_eq!(heap.get(weak), Some(&4));
/// ```
pub struct GcCell<T> {
    addr: Location,
    _marker: PhantomData<T>,
}

impl<T> GcCell<T> {
    /// Return location of referenced object.
    ///
    /// See [`Location`] struct for more information.
    pub fn addr(self) -> Location {
        self.addr
    }

    /// Whether pointers refer to the same object.
    ///
    /// This is equivalent to comparing their locations for equality:
    ///
    /// ```
    /// # use gc::{Heap, GcCell};
    /// # let mut heap = Heap::new();
    /// # let a = heap.alloc_cell(1_usize).downgrade();
    /// # let b = heap.alloc_cell(2_usize).downgrade();
    /// assert_eq!(GcCell::ptr_eq(a, b), a.addr() == b.addr());
    /// ```
    pub fn ptr_eq(self, other: Self) -> bool {
        self.addr == other.addr
    }
}

impl<T> Debug for GcCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Gc")
            .field("addr", &self.addr)
            .finish_non_exhaustive()
    }
}

impl<T> Pointer for GcCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.addr)
    }
}

impl<T> Clone for GcCell<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for GcCell<T> {}

/// A strong reference to gc-allocated value.
///
/// This reference is an equivalent of `Rc<RefCell<T>>`:
///
/// * The reference is *strong* as in it will prevent the value from being dropped
///     while the reference exists.
///     Internally it is implemented through reference counting which makes it similar to [`Rc`].
///     However unlike `Rc` we don't count weak references which allows [`GcCell`] to be copyable.
///
/// * Internal value can be mutated through any existing reference.
///     In this regard it behaves as if it was wrapped in `RefCell` - hence `Cell` in the name.
///
///     Note that there is no actual internal mutability involved:
///     to acquire `&T` or `&mut T` you need to borrow from heap
///     so normal borrow checker rules ensure that aliasing rules are not violated.
///
/// Even though it claims to be a reference type there is no way to [dereference](#dereference)
/// `RootCell` directly into `&T`.
/// As such it provides none of the related traits that you might expect like
/// `Deref<Target = T>` or `AsRef<T>`.
/// Also `RootCell` doesn't implement `PartialEq`, `PartialOrd` or `Hash` to avoid ambiguity.
/// Normal references and smart pointers defer implementations of those traits to referenced value
/// which is not possible for the type.
///
/// If you are looking for a way to provide equivalents of `Eq`, `Ord` or `Hash`
/// as if applied to an underlying pointer consider using [`RootCell::addr`].
///
/// # Construct
///
/// [`Heap`] naturally returns [`RootCell`] after allocating a value:
///
/// ```
/// # use gc::{Heap, RootCell};
/// # let mut heap = Heap::new();
/// let strong: RootCell<usize> = heap.alloc_cell(3);
/// ```
///
/// You can also clone already existing roots or upgrade weak references:
///
/// ```
/// # use gc::{Heap, RootCell};
/// # let mut heap = Heap::new();
/// # let strong: RootCell<usize> = heap.alloc_cell(3_usize);
/// let weak = strong.downgrade();
/// let strong = heap.upgrade(weak).expect("object is still alive");
/// ```
///
/// # Dereference
///
/// Recovering reference to allocated object requires access to heap.
/// Since [`RootCell`] guarantees that object stays alive methods return reference directly:
///
/// ```
/// # use gc::{Heap, RootCell};
/// # let mut heap = Heap::new();
/// let strong = heap.alloc_cell(3_usize);
/// assert_eq!(heap.get_root(&strong), &3);
///
/// // Root prevents object from being collected.
/// heap.gc();
/// assert_eq!(heap.get_root(&strong), &3);
///
/// *heap.get_root_mut(&strong) = 4;
/// assert_eq!(heap.get_root(&strong), &4);
/// ```
///
/// Alternatively [`Heap`] can be indexed using `&RootCell<T>`:
///
/// ```
/// # use gc::{Heap, RootCell};
/// # let mut heap = Heap::new();
/// # let strong = heap.alloc_cell(4_usize);
/// assert_eq!(heap[&strong], 4);
///
/// heap[&strong] = 3;
/// assert_eq!(heap[&strong], 3);
/// ```
pub struct RootCell<T> {
    addr: Location,
    counter: Counter,
    _marker: PhantomData<T>,
}

impl<T> RootCell<T> {
    /// Downgrade into weak reference.
    pub fn downgrade(&self) -> GcCell<T> {
        let RootCell {
            addr,
            counter: _,
            _marker,
        } = self;

        GcCell {
            addr: *addr,
            _marker: PhantomData,
        }
    }

    /// Return location of referenced object.
    ///
    /// See [`Location`] struct for more information.
    pub fn addr(&self) -> Location {
        self.addr
    }

    /// Whether pointers refer to the same object.
    ///
    /// This is equivalent to comparing their locations for equality:
    ///
    /// ```
    /// # use gc::{Heap, RootCell};
    /// # let mut heap = Heap::new();
    /// # let a = heap.alloc_cell(1_usize);
    /// # let b = heap.alloc_cell(2_usize);
    /// assert_eq!(RootCell::ptr_eq(&a, &b), a.addr() == b.addr());
    /// ```
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.addr == other.addr
    }
}

impl<T> Debug for RootCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Root")
            .field("addr", &self.addr)
            .finish_non_exhaustive()
    }
}

impl<T> Pointer for RootCell<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.addr())
    }
}

impl<T> Clone for RootCell<T> {
    fn clone(&self) -> Self {
        let RootCell {
            addr,
            counter,
            _marker,
        } = self;

        Self {
            addr: *addr,
            counter: counter.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> From<RootCell<T>> for GcCell<T> {
    fn from(value: RootCell<T>) -> Self {
        value.downgrade()
    }
}

/// A memory location of garbage-collected object.
///
/// Since Lua defines that certain objects are equal if and only if
/// they are the same object in the memory
/// we need some means to compare memory locations for gc-allocated entities.
/// This type provides such functionality.
///
/// The struct relates to [`GcCell<T>`](GcCell) and [`RootCell<T>`](RootCell)
/// as `usize` address to `*const T`.
/// It uniquely (with some caveats) identifies allocated object.
/// However unlike `usize`,
/// location comparisons are *only well defined between pointers of the same type*.
/// Since `Location` does not include type information
/// the result of comparing locations constructed out of pointers to different types
/// is unspecified and meaningless.
///
/// Heap uses generations to tag pointers (and therefore `Location`s),
/// so even if a new value is allocated at exact same memory spot
/// `Location` of the old object will be unequal to the `Location` of the new object.
///
/// With this `Location` gaurantees (as long as both `lhs` and `rhs` are produced from pointers to the same type):
///
/// * `Eq` implementation, such that
///
///     `lhs == rhs` <=> both pointers are pointing to the same object
///     or in other words trace to the same `Heap::alloc` call.
///
/// * `Ord` implementation
///
///     Actual order is unspecified but consistent with `Eq` implementation.
///
/// * `Hash` implementation
///
/// Lastly, besides provided functionality `Location` is opaque
/// (as it contains implementation-specific details)
/// and there is no way to reconstruct [`GcCell`] out of it.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    index: usize,
    gen: usize,
}

impl Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Location")
            .field("index", &self.index)
            .finish_non_exhaustive()
    }
}

impl Pointer for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.index as *const ())
    }
}
