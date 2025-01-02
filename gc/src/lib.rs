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
//! # use gc::{Heap, Root};
//! # use gc::userdata::UnitParams;
//! # let mut heap = Heap::<(), UnitParams>::new();
//! let ptr = heap.alloc(3);
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
//! # use gc::Heap;
//! # use gc::userdata::UnitParams;
//! #
//! # let mut heap = Heap::<_, _>::new();
//! # let mut heap = std::convert::identity::<Heap<(), UnitParams>>(heap);
//! #
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
//! # use gc::{Heap, GcCell, RootCell};
//! # use gc::userdata::UnitParams;
//! # let mut heap = Heap::<(), UnitParams>::new();
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
//! Alternatively you can allocate using [`Heap::alloc`] which returns [`Root`] instead.
//! The biggest difference is that you cannot acquire mutable reference to objects behind `Root`/`Gc`:
//!
//! ```compile_fail
//! # use gc::{Heap, Root};
//! # use gc::userdata::UnitParams;
//! # let heap = Heap::<(), UnitParams>::new();
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
//! that is it can get mutated through any other reference at any point you are not looking at it.
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
//! # use gc::userdata::UnitParams;
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
//! # let mut heap = Heap::<(), UnitParams>::new();
//! #
//! let a = heap.alloc_cell(3).downgrade();
//!
//! // Oops, this allocation happened to trigger garbage collection.
//! // `a` is not rooted and will be deallocated.
//! # heap.gc();
//! let b = heap.alloc_cell(-3).downgrade();
//!
//! let complex = heap.alloc(Complex {
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
//! # use gc::userdata::UnitParams;
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
//! # let mut heap = Heap::<(), UnitParams>::new();
//! #
//! let weak = heap.alloc('a').downgrade();
//!
//! // Garbage allocation will be paused inside the closure.
//! let complex = heap.pause(|heap| {
//!     let a = heap.alloc_cell(3).downgrade();
//!  
//!     // Even manual triggers are paused.   
//!     heap.gc();
//!     let b = heap.alloc_cell(-3).downgrade();
//!
//!     heap.alloc(Complex {
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
//! Also both [`Heap::alloc`] and [`Heap::alloc_cell`] immediately root the object it allocates
//! so you don't need to worry that inner references will get dropped
//! if gc is triggered as part of the call.

#![forbid(unsafe_code)]

mod heap;
pub mod index;
mod trace;
pub mod userdata;
mod vec_list;

pub use gc_derive::Trace;
pub use heap::{Collector, Heap};
pub use index::{Gc, GcCell, Interned, Root, RootCell};
pub use trace::{Trace, Untrace};

#[cfg(test)]
mod test {
    use super::{Heap, Trace};
    use crate::index::{Allocated, Gc, GcCell, Root, RootCell};
    use crate::userdata::{FullUserdata, Params, UnitParams, Userdata};
    use std::fmt::Debug;

    struct Custom;

    impl Trace for Custom {
        fn trace(&self, _: &mut super::Collector) {}
    }

    fn assert_root_ptr_eq<T, M, P>(heap: &mut Heap<M, P>, ptr: &Root<T>)
    where
        T: Allocated<Heap<M, P>> + ?Sized,
        P: Params,
    {
        let p0 = heap.get_root(ptr) as *const _;
        let p1 = heap.get(ptr.downgrade()).unwrap() as *const _;
        let p2 = &heap[ptr] as *const _;

        assert_eq!(p0, p1);
        assert_eq!(p0, p2);
    }

    fn assert_root_cell_ptr_eq<T, M, P>(heap: &mut Heap<M, P>, ptr: &RootCell<T>)
    where
        T: Allocated<Heap<M, P>> + ?Sized,
        P: Params,
    {
        let p0 = heap.get_root(ptr) as *const _;
        let p1 = heap.get_root_mut(ptr) as *const _;
        let p2 = heap.get(ptr.downgrade()).unwrap() as *const _;
        let p3 = heap.get_mut(ptr.downgrade()).unwrap() as *const _;
        let p4 = &heap[ptr] as *const _;
        let p5 = &mut heap[ptr] as *const _;

        assert_eq!(p0, p1);
        assert_eq!(p0, p2);
        assert_eq!(p0, p3);
        assert_eq!(p0, p4);
        assert_eq!(p0, p5);
    }

    fn assert_gc_ptr_eq<T, M, P>(heap: &mut Heap<M, P>, ptr: &Gc<T>)
    where
        T: Allocated<Heap<M, P>> + ?Sized,
        P: Params,
    {
        let _p1 = heap.get(*ptr).unwrap() as *const _;
    }

    fn assert_gc_cell_ptr_eq<T, M, P>(heap: &mut Heap<M, P>, ptr: &GcCell<T>)
    where
        T: Allocated<Heap<M, P>> + ?Sized,
        P: Params,
    {
        let p0 = heap.get(*ptr).unwrap() as *const _;
        let p1 = heap.get_mut(*ptr).unwrap() as *const _;

        assert_eq!(p0, p1);
    }

    fn assert_root_value_eq<T, M, P>(heap: &mut Heap<M, P>, ptr: &Root<T>, value: &T)
    where
        T: Debug + Eq + Allocated<Heap<M, P>> + ?Sized,
        P: Params,
    {
        assert_eq!(heap.get_root(ptr), value);
        assert_eq!(heap.get(ptr.downgrade()), Some(value));
        assert_eq!(&heap[ptr], value);
    }

    fn assert_root_cell_value_eq<T, M, P>(heap: &mut Heap<M, P>, ptr: &RootCell<T>, value: &T)
    where
        T: Debug + Eq + Allocated<Heap<M, P>> + ?Sized,
        P: Params,
    {
        use std::ops::{Index, IndexMut};

        assert_eq!(heap.get_root(ptr), value);
        assert_eq!(heap.get_root_mut(ptr), value);
        assert_eq!(heap.get(ptr.downgrade()), Some(value));
        assert_eq!(heap.get_mut(ptr.downgrade()).map(|t| &*t), Some(value));
        assert_eq!(heap.index(ptr), value);
        assert_eq!(*heap.index_mut(ptr), *value);
    }

    fn assert_gc_value_eq<T, M, P>(heap: &mut Heap<M, P>, ptr: &Gc<T>, value: &T)
    where
        T: Debug + Eq + Allocated<Heap<M, P>> + ?Sized,
        P: Params,
    {
        assert_eq!(heap.get(*ptr), Some(value));
    }

    fn assert_gc_cell_value_eq<T, M, P>(heap: &mut Heap<M, P>, ptr: &GcCell<T>, value: &T)
    where
        T: Debug + Eq + Allocated<Heap<M, P>> + ?Sized,
        P: Params,
    {
        assert_eq!(heap.get(*ptr), Some(value));
        assert_eq!(heap.get_mut(*ptr).map(|t| &*t), Some(value));
    }

    #[test]
    fn alloc_t() {
        let mut heap = Heap::<(), UnitParams>::new();

        let _: Root<u32> = heap.alloc(3);
        let _: RootCell<Custom> = heap.alloc_cell(Custom);
    }

    #[test]
    fn try_alloc_t() {
        let mut heap = Heap::<(), UnitParams>::new();

        let _: Result<Root<u32>, _> = heap.try_alloc(3);
        let _: Result<RootCell<u32>, _> = heap.try_alloc_cell(3);

        let a = heap.alloc(3).downgrade();

        for i in 1..30 {
            if heap.try_alloc(i).is_err() {
                break;
            }
        }

        assert!(heap.get(a).is_some());
    }

    #[test]
    fn intern() {
        let mut heap = Heap::<(), UnitParams>::new();

        let a = heap.intern("test");
        let b = heap.intern("test");

        assert_eq!(a.location(), b.location());
    }

    #[test]
    fn intern_from() {
        use crate::index::Interned;

        let mut heap = Heap::<(), UnitParams>::new();

        let a: Root<Interned<String>> = heap.intern_from("test");
        let b = heap.intern("test".to_string());

        assert_eq!(a.location(), b.location());
    }

    #[test]
    fn index_root_t() {
        let mut heap = Heap::<(), UnitParams>::new();

        let ptr: Root<u32> = heap.alloc(5);
        assert_root_value_eq(&mut heap, &ptr, &5);
        assert_root_ptr_eq(&mut heap, &ptr);

        let s: &'static str = "this is a test string";
        let ptr: Root<&'static str> = heap.alloc(s);
        assert_root_value_eq(&mut heap, &ptr, &s);
        assert_root_ptr_eq(&mut heap, &ptr);
    }

    #[test]
    fn index_root_cell_t() {
        let mut heap = Heap::<(), UnitParams>::new();

        let ptr: RootCell<u32> = heap.alloc_cell(5);
        assert_root_cell_value_eq(&mut heap, &ptr, &5);
        assert_root_cell_ptr_eq(&mut heap, &ptr);

        *heap.get_root_mut(&ptr) = 10;
        assert_root_cell_value_eq(&mut heap, &ptr, &10);
        assert_root_cell_ptr_eq(&mut heap, &ptr);

        heap[&ptr] = 15;
        assert_root_cell_value_eq(&mut heap, &ptr, &15);
        assert_root_cell_ptr_eq(&mut heap, &ptr);
    }

    #[test]
    fn index_gc_t() {
        let mut heap = Heap::<(), UnitParams>::new();

        let root: Root<u32> = heap.alloc(5);
        let ptr = root.downgrade();

        assert_gc_value_eq(&mut heap, &ptr, &5);
        assert_gc_ptr_eq(&mut heap, &ptr);

        heap.gc();

        assert_gc_value_eq(&mut heap, &ptr, &5);
        assert_gc_ptr_eq(&mut heap, &ptr);

        drop(root);
        heap.gc();

        assert_eq!(heap.get(ptr), None);
    }

    #[test]
    fn index_gc_cell_t() {
        let mut heap = Heap::<(), UnitParams>::new();

        let root: RootCell<u32> = heap.alloc_cell(5);
        let ptr = root.downgrade();

        assert_gc_cell_value_eq(&mut heap, &ptr, &5);
        assert_gc_cell_ptr_eq(&mut heap, &ptr);

        heap.gc();

        assert_gc_cell_value_eq(&mut heap, &ptr, &5);
        assert_gc_cell_ptr_eq(&mut heap, &ptr);

        *heap.get_mut(ptr).unwrap() = 6;

        assert_gc_cell_value_eq(&mut heap, &ptr, &6);
        assert_gc_cell_ptr_eq(&mut heap, &ptr);

        drop(root);
        heap.gc();

        assert_eq!(heap.get(ptr), None);
        assert_eq!(heap.get_mut(ptr), None);
    }

    #[test]
    fn alloc_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let _: Root<dyn Userdata<_>> = heap.alloc_as(3);
        let _: RootCell<dyn Userdata<_>> = heap.alloc_as(Custom);
    }

    #[test]
    fn index_root_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let ptr: Root<dyn Userdata<_>> = heap.alloc_as(3);
        assert_root_ptr_eq(&mut heap, &ptr);

        let s: &'static str = "this is a test string";
        let ptr: Root<dyn Userdata<_>> = heap.alloc_as(s);
        assert_root_ptr_eq(&mut heap, &ptr);
    }

    #[test]
    fn index_root_cell_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let ptr: RootCell<dyn Userdata<_>> = heap.alloc_as(5);
        assert_root_cell_ptr_eq(&mut heap, &ptr);
    }

    #[test]
    fn index_gc_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let root: Root<dyn Userdata<_>> = heap.alloc_as(5);
        let ptr = root.downgrade();
        assert_gc_ptr_eq(&mut heap, &ptr);

        drop(root);
        heap.gc();

        assert!(heap.get(ptr).is_none());
    }

    #[test]
    fn index_gc_cell_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let root: RootCell<dyn Userdata<_>> = heap.alloc_as(5);
        let ptr = root.downgrade();
        assert_gc_cell_ptr_eq(&mut heap, &ptr);

        drop(root);
        heap.gc();

        assert!(heap.get(ptr).is_none());
        assert!(heap.get_mut(ptr).is_none());
    }

    #[test]
    fn alloc_full_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let _: Root<dyn FullUserdata<_, _>> = heap.alloc_as(3);
        let _: RootCell<dyn FullUserdata<_, _>> = heap.alloc_as(Custom);
    }

    #[test]
    fn index_root_full_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let ptr: Root<dyn FullUserdata<_, _>> = heap.alloc_as(3);
        assert_root_ptr_eq(&mut heap, &ptr);

        let s: &'static str = "this is a test string";
        let ptr: Root<dyn FullUserdata<_, _>> = heap.alloc_as(s);
        assert_root_ptr_eq(&mut heap, &ptr);
    }

    #[test]
    fn index_root_cell_full_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let ptr: RootCell<dyn FullUserdata<_, _>> = heap.alloc_as(5);
        assert_root_cell_ptr_eq(&mut heap, &ptr);
    }

    #[test]
    fn index_gc_full_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let root: Root<dyn FullUserdata<_, _>> = heap.alloc_as(5);
        let ptr = root.downgrade();
        assert_gc_ptr_eq(&mut heap, &ptr);

        drop(root);
        heap.gc();

        assert!(heap.get(ptr).is_none());
    }

    #[test]
    fn index_gc_cell_full_userdata() {
        let mut heap = Heap::<(), UnitParams>::new();

        let root: RootCell<dyn FullUserdata<_, _>> = heap.alloc_as(5);
        let ptr = root.downgrade();
        assert_gc_cell_ptr_eq(&mut heap, &ptr);

        drop(root);
        heap.gc();

        assert!(heap.get(ptr).is_none());
        assert!(heap.get_mut(ptr).is_none());
    }

    #[test]
    fn mixed_alloc() {
        let mut heap = Heap::<(), UnitParams>::new();

        let a: Root<u32> = heap.alloc(5);
        let b: Root<dyn Userdata<_>> = heap.alloc_as(5);
        let c: Root<dyn FullUserdata<_, _>> = heap.alloc_as(5);

        assert_root_value_eq(&mut heap, &a, &5);
        assert_root_ptr_eq(&mut heap, &a);
        assert_root_ptr_eq(&mut heap, &b);
        assert_root_ptr_eq(&mut heap, &c);

        let a: Root<u32> = heap.alloc(6);
        let b: Root<dyn Userdata<_>> = heap.alloc_as(5);
        let c: Root<dyn FullUserdata<_, _>> = heap.alloc_as(5);

        assert_root_value_eq(&mut heap, &a, &6);
        assert_root_ptr_eq(&mut heap, &a);
        assert_root_ptr_eq(&mut heap, &b);
        assert_root_ptr_eq(&mut heap, &c);
    }

    #[test]
    fn downcast() {
        let mut heap = Heap::<(), UnitParams>::new();

        let a: Root<dyn Userdata<_>> = heap.alloc_as(5_usize);
        let _: Gc<usize> = heap.downcast(a.downgrade()).unwrap();

        let a: Root<dyn FullUserdata<_, _>> = heap.alloc_as(5_usize);
        let _: Gc<usize> = heap.downcast(a.downgrade()).unwrap();
    }

    struct ReturnU32;

    impl Params for ReturnU32 {
        type Id<'id> = ();
        type Rt<'rt> = ();
        type Res = u32;
    }

    #[test]
    fn dispatcher() {
        let mut heap = Heap::<(), ReturnU32>::new();

        let a: Root<dyn Userdata<_>> = heap.alloc_as(3_u32);
        let b: Root<dyn FullUserdata<_, _>> = heap.alloc_as(5_u32);
        let c: Root<dyn Userdata<_>> = heap.alloc_as(7_u64);

        assert_eq!(heap.get_root(&a).method((), ()), None);
        assert_eq!(heap.get_root(&b).method((), ()), None);
        assert_eq!(heap.get_root(&c).method((), ()), None);

        heap.set_dispatcher::<u32>(|value, _, _| Some(*value));

        assert_eq!(heap.get_root(&a).method((), ()), Some(3));
        assert_eq!(heap.get_root(&b).method((), ()), Some(5));
        assert_eq!(heap.get_root(&c).method((), ()), None);

        heap.set_dispatcher::<u64>(|_, _, _| Some(0));

        assert_eq!(heap.get_root(&a).method((), ()), Some(3));
        assert_eq!(heap.get_root(&b).method((), ()), Some(5));
        assert_eq!(heap.get_root(&c).method((), ()), Some(0));

        heap.set_dispatcher::<u32>(|value, _, _| Some(*value + 1));

        assert_eq!(heap.get_root(&a).method((), ()), Some(4));
        assert_eq!(heap.get_root(&b).method((), ()), Some(6));
        assert_eq!(heap.get_root(&c).method((), ()), Some(0));

        let d: Root<dyn Userdata<_>> = heap.alloc_as(9_u32);

        assert_eq!(heap.get_root(&d).method((), ()), Some(10));
    }

    #[test]
    fn metatable() {
        let mut heap = Heap::<u32, UnitParams>::new();

        let a: Root<dyn FullUserdata<_, _>> = heap.alloc_as(3_u32);
        let b: Root<dyn FullUserdata<_, _>> = heap.alloc_as(7_u64);

        assert_eq!(heap.get_root(&a).metatable(), None);
        assert_eq!(heap.get_root(&b).metatable(), None);

        heap.set_metatable::<u32>(Some(2));
        assert_eq!(heap.metatable_of::<u32>(), Some(&2));

        let c: Root<dyn FullUserdata<_, _>> = heap.alloc_as(5_u32);
        let d: Root<dyn FullUserdata<_, _>> = heap.alloc_as(9_u64);

        assert_eq!(heap.get_root(&a).metatable(), None);
        assert_eq!(heap.get_root(&b).metatable(), None);
        assert_eq!(heap.get_root(&c).metatable(), Some(&2));
        assert_eq!(heap.get_root(&d).metatable(), None);

        let e: RootCell<dyn FullUserdata<_, _>> = heap.alloc_full_userdata(11_u64, Some(10));

        assert_eq!(heap.get_root(&a).metatable(), None);
        assert_eq!(heap.get_root(&b).metatable(), None);
        assert_eq!(heap.get_root(&c).metatable(), Some(&2));
        assert_eq!(heap.get_root(&d).metatable(), None);
        assert_eq!(heap.get_root(&e).metatable(), Some(&10));

        heap.set_metatable::<u64>(Some(15));
        assert_eq!(heap.metatable_of::<u64>(), Some(&15));

        heap.get_root_mut(&e).set_metatable(Some(12));

        let f: RootCell<dyn FullUserdata<_, _>> = heap.alloc_as(11_u64);

        assert_eq!(heap.get_root(&a).metatable(), None);
        assert_eq!(heap.get_root(&b).metatable(), None);
        assert_eq!(heap.get_root(&c).metatable(), Some(&2));
        assert_eq!(heap.get_root(&d).metatable(), None);
        assert_eq!(heap.get_root(&e).metatable(), Some(&12));
        assert_eq!(heap.get_root(&f).metatable(), Some(&15));

        let prev = heap.set_metatable::<u32>(None);

        assert_eq!(prev, Some(2));
        assert_eq!(heap.metatable_of::<u32>(), None);
    }

    #[test]
    fn ptr_sizes() {
        use std::mem::size_of;

        assert_eq!(size_of::<Gc<()>>(), 8);
        assert_eq!(size_of::<Gc<dyn Userdata<UnitParams>>>(), 8);
        assert_eq!(size_of::<Gc<dyn FullUserdata<(), UnitParams>>>(), 8);

        assert_eq!(size_of::<Gc<()>>(), size_of::<Option<Gc<()>>>());

        assert_eq!(size_of::<Root<()>>(), 24);
        assert_eq!(size_of::<Root<dyn Userdata<UnitParams>>>(), 24);
        assert_eq!(size_of::<Root<dyn FullUserdata<(), UnitParams>>>(), 24);

        assert_eq!(size_of::<Root<()>>(), size_of::<Option<Root<()>>>());
    }
}
