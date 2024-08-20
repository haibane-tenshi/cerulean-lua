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
//! * [`RootCell<T>`](RootCell)/[`Root<T>`](Root) is a *strong* reference which keeps the pointed-to object alive.
//!
//!     Internally roots are counted which makes them similar to [`Rc`](std::rc::Rc).
//!     Because of this roots implement `Clone` but not `Copy`.
//!
//!     You are expected to keep roots outside of the heap so it doesn't implement [`Trace`].
//!
//! * [`GcCell<T>`](GcCell)/[`Gc<T>`](Gc) is a *weak* reference.
//!     It doesn't prevent pointed-to object from being deallocated unless it can trace to a root.
//!
//!     Both types are implemented as a lightweight index and is `Copy`able
//!     which makes it different from [`Weak`](std::rc::Weak) references.
//!
//!     To keep behavior consistent `GcCell`/`Gc` also contain *generation* tag,
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
//! With this it might be better to think of `Heap` as *object interner* rather than bona fide garbage collector.
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
//! There is also non-cell version: [`Heap::alloc`] which works with [`Root`]/[`Gc`] respectively.
//! The biggest difference is that you cannot acquire mutable reference to objects behind `Root`/`Gc`:
//!
//! ```compile_fail
//! # use gc::{Heap, Root};
//! # let heap = Heap::new();
//! let num = heap.alloc(3_usize);
//!
//! // You cannot acquire mutable reference to the value:
//! *heap.get_root_mut(&num) = 4;
//! ```
//!
//! Conversely, you can get `&mut T` to objects behind `RootCell`/`GcCell` - hence `Cell` in the name.
//! Note that there is no actual internal mutability is involved:
//! constructing `&T`/`&mut T` requires a borrow from heap and
//! borrow checker will prevent any aliasing issues from arising.
//! Rather, `Cell` in the name is a reminder that referenced value *behaves as if it was inside a cell*,
//! that is it can get mutated through any reference at any point you are not looking at it.
//!
//! # Common pitfalls: constructing complex objects
//!
//! Garbage collection triggered by [`Heap::alloc_cell`]/[`Heap::alloc`] may happen during construction
//! of a complex object.
//! Any non-rooted `GcCell`/`Gc` inside such an object may be spuriously deallocated
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

mod heap;
pub mod index;
mod trace;
pub mod userdata;
mod vec_list;

pub use heap::{Collector, Heap};
pub use index::{Gc, GcCell, Root, RootCell};
pub use trace::{Trace, Untrace};
