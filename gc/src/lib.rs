//! Entirely safe generic garbage collector implementation
//!
//! This crate contains no unsafe code.
//!
//! # Quick start
//!
//! You need to implement [`Trace`] trait for your type so that gc can traverse it:
//!
//! ```
//! use gc::{Gc, Trace, Collector};
//!
//! #[derive(Default)]
//! struct BinaryTree {
//!     left: Option<Gc<Self>>,
//!     right: Option<Gc<Self>>,
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
//! Note that this is a *safe* trait.
//! Even if you implement it incorrectly there is no way for you to cause memory unsafety.
//! An incorrect implementation may cause some objects to be collected too early,
//! but this will be [exposed](Gc#dereference) to you when you try to recover references from heap.
//!
//! Any type implementing `Trace` can be allocated:
//!
//! ```
//! # use gc::{Gc, Trace, Collector};
//! #
//! # #[derive(Default)]
//! # struct BinaryTree {
//! #     left: Option<Gc<Self>>,
//! #     right: Option<Gc<Self>>,
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
//! let a = heap.alloc(BinaryTree::default());
//! let b = heap.alloc(BinaryTree::default());
//!
//! heap[&a].left = Some(b.downgrade());
//! heap[&b].right = Some(b.downgrade());
//!
//! // Oops, this is no longer a tree :(
//! heap[&b].left = Some(a.downgrade());
//! ```
//!
//! [`Heap::alloc`] returns a [`Root`] - a strong reference that prevents object from being deallocated.
//! You can downgrade them into weak references - [`Gc`],
//! but if an object is not reachable from one of the roots it will be considered garbage and collected:
//!
//! ```
//! # use gc::{Heap, Gc};
//! # let mut heap = Heap::new();
//! # let a = heap.alloc(3_usize);
//! # let b = heap.alloc(3_usize);
//! // Drop strong references leaving only weak ones.
//! let weak_a: Gc<_> = a.downgrade();
//! let weak_b: Gc<_> = b.downgrade();
//!
//! drop(a);
//! drop(b);
//!
//! heap.gc();
//!
//! assert!(matches!(heap.get(weak_a), None));
//! assert!(matches!(heap.get(weak_b), None));
//! ```

#![forbid(unsafe_code)]

mod arena;
mod trace;
mod vec_list;

use std::any::TypeId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Pointer};
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};
use std::rc::Rc;

use bitvec::vec::BitVec;

use arena::{Arena, ArenaStore, Counter, StrongCounters};

pub use trace::{Trace, Untrace};

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
/// Garbage collection can be triggered automatically on [`Heap::alloc`]
/// or manually via [`Heap::gc`].
/// We are using standard mark-and-sweep strategy.
///
/// Newely allocated objects return [`Root<T>`](Root) which is a strong reference,
/// but can be downgraded into [`Gc<T>`](Gc) which is a weak reference.
///
/// Result of dereferencing a pointer constructed in a different heap is unspecified,
/// but guaranteed to be *safe*.
/// It may or may not return a valid reference if an object is found at the location
/// or invoke panic.
#[derive(Default)]
pub struct Heap {
    arenas: HashMap<TypeId, Box<dyn Arena>>,
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
    pub fn alloc<T>(&mut self, value: T) -> Root<T>
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
    fn alloc_slow<T>(&mut self, value: T) -> Root<T>
    where
        T: Trace,
    {
        self.gc_with(&value);
        self.arena_mut().unwrap().insert(value)
    }

    /// Get `&T` out of weak reference.
    ///
    /// [`Gc`] is a weak reference so it is possible that the object since was deallocated.
    pub fn get<T>(&self, ptr: Gc<T>) -> Option<&T>
    where
        T: Trace,
    {
        self.arena()?.get(ptr.addr)
    }

    /// Get `&mut T` out of weak reference.
    ///
    /// [`Gc`] is a weak reference so it is possible that the object since was deallocated.
    pub fn get_mut<T>(&mut self, ptr: Gc<T>) -> Option<&mut T>
    where
        T: Trace,
    {
        self.arena_mut()?.get_mut(ptr.addr)
    }

    /// Get `&T` out of strong reference.
    ///
    /// [`Root`] is a strong reference and prevents objects from being deallocated.
    pub fn get_root<T>(&self, ptr: &Root<T>) -> &T
    where
        T: Trace,
    {
        self.arena()
            .and_then(|arena| arena.get(ptr.addr))
            .expect("rooted object was deallocated")
    }

    /// Get `&T` out of strong reference.
    ///
    /// [`Root`] is a strong reference and prevents objects from being deallocated.
    pub fn get_root_mut<T>(&mut self, ptr: &Root<T>) -> &mut T
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
    pub fn upgrade<T>(&mut self, ptr: Gc<T>) -> Option<Root<T>>
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

impl<T> IndexMut<&Root<T>> for Heap
where
    T: Trace,
{
    fn index_mut(&mut self, index: &Root<T>) -> &mut Self::Output {
        self.get_root_mut(index)
    }
}

/// Intermediary keeping track of visited objects during [`Trace`]ing.
pub struct Collector {
    masks: HashMap<TypeId, BitVec>,
}

impl Collector {
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

/// A weak reference to gc-allocated value.
///
/// This reference is *weak* as in it alone won't prevent value from being dropped.
/// However when part of certainly alive object
/// it will indicate that referencee is also alive through [`Trace`] trait.
///
/// Unlike [`Root`] this type implements [`Copy`].
///
/// Even though it claims to be a reference type there is no way to [dereference](#dereference)
/// `Gc` directly into `&T`.
/// As such it provides none of the related traits that you might expect like
/// `Deref<Target = T>` or `AsRef<T>`.
/// Also `Gc` doesn't implement `PartialEq`, `PartialOrd` or `Hash` to avoid ambiguity.
/// Normal references and smart pointers defer implementations of those traits to referenced value
/// which is not possible for the type.
///
/// If you are looking for a way to provide equivalents of `Eq`, `Ord` or `Hash`
/// as if applied to an underlying pointer consider using [`Gc::addr`].
///
/// # Construct
///
/// Common way to construct [`Gc`] is to downgrade [`Root`]:
///
/// ```
/// # use gc::{Heap, Gc, Root};
/// # let mut heap = Heap::new();
/// let strong = heap.alloc(3);
/// let weak: Gc<usize> = strong.downgrade();
/// ```
///
/// Converting `Gc` back to `Root` requires access to heap:
///
/// ```
/// # use gc::{Heap, Gc, Root};
/// # let mut heap = Heap::new();
/// # let weak: Gc<usize> = heap.alloc(3).downgrade();
/// let strong: Root<usize> = heap.upgrade(weak).expect("object is still alive");
/// ```
///
/// # Dereference
///
/// Recovering reference to allocated object requires access to heap.
/// Since `Gc` doesn't guarantee that an object stays alive dereference returns `Option`:
///
/// ```
/// # use gc::{Heap, Gc, Root};
/// # let mut heap = Heap::new();
/// let weak = heap.alloc(3_usize).downgrade();
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
/// # use gc::{Heap, Gc, Root};
/// # let mut heap = Heap::new();
/// let weak = heap.alloc(3_usize).downgrade();
/// assert_eq!(heap.get(weak), Some(&3));
///
/// let ref_mut: &mut usize = heap.get_mut(weak).expect("object is still alive");
/// *ref_mut = 4;
/// assert_eq!(heap.get(weak), Some(&4));
/// ```
pub struct Gc<T> {
    addr: Location,
    _marker: PhantomData<T>,
}

impl<T> Gc<T> {
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
    /// # use gc::{Heap, Gc};
    /// # let mut heap = Heap::new();
    /// # let a = heap.alloc(1_usize).downgrade();
    /// # let b = heap.alloc(2_usize).downgrade();
    /// assert_eq!(Gc::ptr_eq(a, b), a.addr() == b.addr());
    /// ```
    pub fn ptr_eq(self, other: Self) -> bool {
        self.addr == other.addr
    }
}

impl<T> Pointer for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.addr)
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Gc<T> {}

/// A strong reference to gc-allocated value.
///
/// This reference is *strong* as in it will prevent the value from being dropped
/// while the reference exists.
/// Internally it is implemented through reference counting which makes it similar to [`Rc`].
/// However unlike `Rc` we don't count weak references which allows [`Gc`] to be copyable.
///
/// Even though it claims to be a reference type there is no way to [dereference](#dereference)
/// `Root` directly into `&T`.
/// As such it provides none of the related traits that you might expect like
/// `Deref<Target = T>` or `AsRef<T>`.
/// Also `Root` doesn't implement `PartialEq`, `PartialOrd` or `Hash` to avoid ambiguity.
/// Normal references and smart pointers defer implementations of those traits to referenced value
/// which is not possible for the type.
///
/// If you are looking for a way to provide equivalents of `Eq`, `Ord` or `Hash`
/// as if applied to an underlying pointer consider using [`Root::addr`].
///
/// # Construct
///
/// [`Heap`] naturally returns [`Root`] after allocating a value:
///
/// ```
/// # use gc::{Heap, Root};
/// # let mut heap = Heap::new();
/// let strong: Root<usize> = heap.alloc(3);
/// ```
///
/// You can also clone already existing roots or upgrade weak references:
///
/// ```
/// # use gc::{Heap, Root};
/// # let mut heap = Heap::new();
/// # let strong: Root<usize> = heap.alloc(3_usize);
/// let weak = strong.downgrade();
/// let strong = heap.upgrade(weak).expect("object is still alive");
/// ```
///
/// # Dereference
///
/// Recovering reference to allocated object requires access to heap.
/// Since [`Root`] guarantees that object stays alive methods return reference directly:
///
/// ```
/// # use gc::{Heap, Root};
/// # let mut heap = Heap::new();
/// let strong = heap.alloc(3_usize);
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
/// Alternatively [`Heap`] can be indexed using `&Root<T>`:
///
/// ```
/// # use gc::{Heap, Root};
/// # let mut heap = Heap::new();
/// # let strong = heap.alloc(4_usize);
/// assert_eq!(heap[&strong], 4);
///
/// heap[&strong] = 3;
/// assert_eq!(heap[&strong], 3);
/// ```
pub struct Root<T> {
    addr: Location,
    counter: Counter,
    strong_counters: Rc<RefCell<StrongCounters>>,
    _marker: PhantomData<T>,
}

impl<T> Root<T> {
    /// Downgrade into weak reference.
    pub fn downgrade(&self) -> Gc<T> {
        let Root {
            addr,
            counter: _,
            strong_counters: _,
            _marker,
        } = self;

        Gc {
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
    /// # use gc::{Heap, Root};
    /// # let mut heap = Heap::new();
    /// # let a = heap.alloc(1_usize);
    /// # let b = heap.alloc(2_usize);
    /// assert_eq!(Root::ptr_eq(&a, &b), a.addr() == b.addr());
    /// ```
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.addr == other.addr
    }
}

impl<T> Pointer for Root<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.addr())
    }
}

impl<T> Clone for Root<T> {
    fn clone(&self) -> Self {
        let Root {
            addr,
            counter,
            strong_counters,
            _marker,
        } = self;

        strong_counters.borrow_mut().increment(*counter);

        Root {
            addr: *addr,
            counter: *counter,
            strong_counters: strong_counters.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> Drop for Root<T> {
    fn drop(&mut self) {
        self.strong_counters.borrow_mut().decrement(self.counter);
    }
}

impl<T> From<Root<T>> for Gc<T> {
    fn from(value: Root<T>) -> Self {
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
/// The struct relates to [`Gc<T>`](Gc) and [`Root<T>`](Root)
/// as `usize` address to `*const T`.
/// It uniquely (with some caveats, read further) identifies position of allocated value,
/// as such it implements `Ord + Hash`.
/// However unlike `usize`,
/// location comparisons are *only well defined between pointers of the same type*.
/// Since `Location` does not include type information
/// the result of comparing locations constructed out of pointers to different types
/// is unspecified and meaningless.
///
/// Lastly, besides provided functionality `Location` is opaque
/// (as it contains implementation-specific details)
/// and there is no way to reconstruct [`Gc`] out of it.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    index: usize,
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
