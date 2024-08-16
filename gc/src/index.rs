use std::fmt::{Debug, Pointer};
use std::hash::Hash;
use std::marker::PhantomData;

use crate::arena::{Addr, Arena, Counter};
use crate::heap::TypeIndex;

/// A weak reference to gc-allocated mutable value.
///
/// This reference is an equivalent of `Weak<RefCell<T>>`:
///
/// * The reference is *weak* as in it alone won't prevent value from being dropped.
///     However when part of certainly alive object
///     it will indicate that referencee is also alive through [`Trace`](crate::Trace) trait.
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
/// as if applied to an underlying pointer consider using [`GcCell::location`].
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
    addr: Addr,
    ty: TypeIndex,
    _marker: PhantomData<T>,
}

impl<T> GcCell<T> {
    pub(crate) fn ty(self) -> TypeIndex {
        self.ty
    }

    pub(crate) fn addr(self) -> Addr {
        self.addr
    }

    /// Return location of referenced object.
    ///
    /// See [`Location`] struct for more information.
    pub fn location(self) -> Location<T> {
        let GcCell { addr, ty, _marker } = self;

        Location { addr, ty, _marker }
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
    /// assert_eq!(GcCell::ptr_eq(a, b), a.location() == b.location());
    /// ```
    pub fn ptr_eq(self, other: Self) -> bool {
        self.location() == other.location()
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
        write!(f, "{:p}", self.location())
    }
}

impl<T> Clone for GcCell<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for GcCell<T> {}

/// A strong reference to gc-allocated mutable value.
///
/// This reference is an equivalent of `Rc<RefCell<T>>`:
///
/// * The reference is *strong* as in it will prevent the value from being dropped
///     while the reference exists.
///     Internally it is implemented through reference counting which makes it similar to [`Rc`](std::rc::Rc).
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
/// as if applied to an underlying pointer consider using [`RootCell::location`].
///
/// # Construct
///
/// [`Heap`](crate::Heap) naturally returns [`RootCell`] after allocating a value:
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
/// Alternatively [`Heap`](crate::Heap) can be indexed using `&RootCell<T>`:
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
    addr: Addr,
    ty: TypeIndex,
    counter: Counter,
    _marker: PhantomData<T>,
}

impl<T> RootCell<T> {
    pub(crate) fn new(addr: Addr, ty: TypeIndex, counter: Counter) -> Self {
        RootCell {
            addr,
            ty,
            counter,
            _marker: PhantomData,
        }
    }

    /// Downgrade into weak reference.
    pub fn downgrade(&self) -> GcCell<T> {
        let RootCell {
            addr,
            ty,
            counter: _,
            _marker: _,
        } = self;

        GcCell {
            addr: *addr,
            ty: *ty,
            _marker: PhantomData,
        }
    }

    /// Return location of referenced object.
    ///
    /// See [`Location`] struct for more information.
    pub fn location(&self) -> Location<T> {
        self.downgrade().location()
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
    /// assert_eq!(RootCell::ptr_eq(&a, &b), a.location() == b.location());
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
        write!(f, "{:p}", self.location())
    }
}

impl<T> Clone for RootCell<T> {
    fn clone(&self) -> Self {
        let RootCell {
            addr,
            ty,
            counter,
            _marker: _,
        } = self;

        Self {
            addr: *addr,
            ty: *ty,
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

/// A weak reference to gc-allocated immutable value.
///
/// This reference is an equivalent of `Weak<T>`:
///
/// * The reference is *weak* as in it alone won't prevent value from being dropped.
///     However when part of certainly alive object
///     it will indicate that referencee is also alive through [`Trace`](crate::Trace) trait.
///
/// * Internal value cannot be mutably accessed after allocation.
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
/// as if applied to an underlying pointer consider using [`Gc::location`].
///
/// If you intend to mutate object later, consider using [`GcCell`] instead of wrapping value in `Cell`/`RefCell`.
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
pub struct Gc<T> {
    addr: Addr,
    ty: TypeIndex,
    _marker: PhantomData<T>,
}

impl<T> Gc<T> {
    pub(crate) fn ty(self) -> TypeIndex {
        self.ty
    }

    pub(crate) fn addr(self) -> Addr {
        self.addr
    }

    /// Return location of referenced object.
    ///
    /// See [`Location`] struct for more information.
    pub fn location(self) -> Location<T> {
        let Gc { addr, ty, _marker } = self;

        Location { addr, ty, _marker }
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
    /// assert_eq!(Gc::ptr_eq(a, b), a.location() == b.location());
    /// ```
    pub fn ptr_eq(self, other: Self) -> bool {
        self.location() == other.location()
    }
}

impl<T> Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Gc")
            .field("addr", &self.addr)
            .finish_non_exhaustive()
    }
}

impl<T> Pointer for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.location())
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Gc<T> {}

/// A strong reference to gc-allocated immutable value.
///
/// This reference is an equivalent of `Rc<T>`:
///
/// * The reference is *strong* as in it will prevent the value from being dropped
///     while the reference exists.
///     Internally it is implemented through reference counting which makes it similar to [`Rc`](std::rc::Rc).
///     However unlike `Rc` we don't count weak references which allows [`Gc`] to be copyable.
///
/// * Internal value cannot be mutably accessed after allocation.
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
/// as if applied to an underlying pointer consider using [`Root::location`].
///
/// If you intend to mutate object later, consider using [`RootCell`] instead of wrapping value in `Cell`/`RefCell`.
///
/// # Construct
///
/// [`Heap`](crate::Heap) naturally returns [`Root`] after allocating a value:
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
/// ```
///
/// Alternatively [`Heap`](crate::Heap) can be indexed using `&Root<T>`:
///
/// ```
/// # use gc::{Heap, Root};
/// # let mut heap = Heap::new();
/// # let strong = heap.alloc(3_usize);
/// assert_eq!(heap[&strong], 3);
/// ```
pub struct Root<T> {
    addr: Addr,
    ty: TypeIndex,
    counter: Counter,
    _marker: PhantomData<T>,
}

impl<T> Root<T> {
    pub(crate) fn new(addr: Addr, ty: TypeIndex, counter: Counter) -> Self {
        Root {
            addr,
            ty,
            counter,
            _marker: PhantomData,
        }
    }

    /// Downgrade into weak reference.
    pub fn downgrade(&self) -> Gc<T> {
        let Root {
            addr,
            ty,
            counter: _,
            _marker: _,
        } = self;

        Gc {
            addr: *addr,
            ty: *ty,
            _marker: PhantomData,
        }
    }

    /// Return location of referenced object.
    ///
    /// See [`Location`] struct for more information.
    pub fn location(&self) -> Location<T> {
        self.downgrade().location()
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
    /// assert_eq!(Root::ptr_eq(&a, &b), a.location() == b.location());
    /// ```
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.addr == other.addr
    }
}

impl<T> Debug for Root<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Root")
            .field("addr", &self.addr)
            .finish_non_exhaustive()
    }
}

impl<T> Pointer for Root<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.location())
    }
}

impl<T> Clone for Root<T> {
    fn clone(&self) -> Self {
        let Root {
            addr,
            ty,
            counter,
            _marker: _,
        } = self;

        Self {
            addr: *addr,
            ty: *ty,
            counter: counter.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> From<Root<T>> for Gc<T> {
    fn from(value: Root<T>) -> Self {
        value.downgrade()
    }
}

/// A memory location of garbage-collected object.
///
/// This garbage collector was originally written to back up Lua runtime needs.
/// Since Lua defines that certain objects are equal if and only if
/// they are the same object in the memory
/// we need some means to compare memory locations for gc-allocated entities.
/// This type provides such functionality.
///
/// The struct relates to [`GcCell<T>`](GcCell), [`RootCell<T>`](RootCell), [`Gc<T>`](Gc) and [`Root<T>`](Root)
/// as `*const T`/`*mut T` pointer to `&T`/`&mut T`.
/// It uniquely identifies potentially allocated object
/// and provides implementation of commonplace Rust traits as if applied to an actual pointer:
///
/// * `Eq` implementation, such that
///
///     `lhs == rhs` <=> both pointers are pointing to the same object
///     or in other words trace to the same `Heap::alloc`/`Heap::alloc_cell` call.
///
/// * `Ord` implementation
///
///     Actual order is unspecified but deterministic and consistent with `Eq` implementation.
///
/// * `Hash` implementation
///
/// Heap uses generations to tag pointers (and therefore `Location`s),
/// so even if a new value is allocated at exact same memory spot
/// `Location` of the old object will be unequal to the `Location` of the new object.
///
/// Lastly, besides provided functionality `Location` is opaque
/// (as it contains implementation-specific details)
/// and there is no way to reconstruct [`GcCell`]/[`Gc`] out of it.
pub struct Location<T: ?Sized> {
    addr: Addr,
    ty: TypeIndex,
    _marker: PhantomData<T>,
}

impl<T> Clone for Location<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Location<T> {}

impl<T> PartialEq for Location<T> {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr && self.ty == other.ty && self._marker == other._marker
    }
}

impl<T> Eq for Location<T> {}

impl<T> PartialOrd for Location<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Location<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;

        match self.addr.cmp(&other.addr) {
            Ordering::Equal => (),
            ord => return ord,
        };

        match self.ty.cmp(&other.ty) {
            Ordering::Equal => (),
            ord => return ord,
        };

        self._marker.cmp(&other._marker)
    }
}

impl<T> Hash for Location<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.addr.hash(state);
        self.ty.hash(state);
        self._marker.hash(state);
    }
}

impl<T> Debug for Location<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Location")
            .field("index", &self.addr.index())
            .field("gen", &self.addr.gen())
            .finish_non_exhaustive()
    }
}

impl<T> Pointer for Location<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:p}:{}",
            self.addr.index() as *const (),
            self.addr.gen()
        )
    }
}

pub(crate) mod sealed {
    use super::{Gc, GcCell, Root, RootCell};

    #[doc(hidden)]
    pub struct Addr(pub(crate) super::Addr);

    #[doc(hidden)]
    pub struct TypeIndex(pub(crate) super::TypeIndex);

    pub trait Sealed {
        #[doc(hidden)]
        fn addr(&self) -> Addr;

        #[doc(hidden)]
        fn type_index(&self) -> TypeIndex;
    }

    impl<T> Sealed for GcCell<T> {
        fn addr(&self) -> Addr {
            Addr(self.addr)
        }

        fn type_index(&self) -> TypeIndex {
            TypeIndex(self.ty)
        }
    }

    impl<T> Sealed for RootCell<T> {
        fn addr(&self) -> Addr {
            Addr(self.addr)
        }

        fn type_index(&self) -> TypeIndex {
            TypeIndex(self.ty)
        }
    }

    impl<T> Sealed for Gc<T> {
        fn addr(&self) -> Addr {
            Addr(self.addr)
        }

        fn type_index(&self) -> TypeIndex {
            TypeIndex(self.ty)
        }
    }

    impl<T> Sealed for Root<T> {
        fn addr(&self) -> Addr {
            Addr(self.addr)
        }

        fn type_index(&self) -> TypeIndex {
            TypeIndex(self.ty)
        }
    }
}

mod sealed_root {
    use super::{Root, RootCell};

    #[doc(hidden)]
    pub struct Counter<'a>(pub(crate) &'a super::Counter);

    pub trait Sealed {
        #[doc(hidden)]
        fn counter(&self) -> Counter;
    }

    impl<T> Sealed for RootCell<T> {
        fn counter(&self) -> Counter {
            Counter(&self.counter)
        }
    }

    impl<T> Sealed for Root<T> {
        fn counter(&self) -> Counter {
            Counter(&self.counter)
        }
    }
}

pub(crate) mod sealed_upgrade {
    use super::{Gc, GcCell, Root, RootCell};

    #[doc(hidden)]
    pub struct Counter(pub(crate) super::Counter);

    pub trait Sealed {
        type Target;

        #[doc(hidden)]
        fn upgrade(self, counter: Counter) -> Self::Target;
    }

    impl<T> Sealed for Gc<T> {
        type Target = Root<T>;

        fn upgrade(self, counter: Counter) -> Self::Target {
            let Counter(counter) = counter;
            let Gc {
                addr,
                ty,
                _marker: _,
            } = self;
            Root::new(addr, ty, counter)
        }
    }

    impl<T> Sealed for GcCell<T> {
        type Target = RootCell<T>;

        fn upgrade(self, counter: Counter) -> Self::Target {
            let Counter(counter) = counter;
            let GcCell {
                addr,
                ty,
                _marker: _,
            } = self;
            RootCell::new(addr, ty, counter)
        }
    }
}

/// Marker trait permitting by-reference (`&T`) access.
///
/// Purpose of this trait is to serve as bound in [`Heap`](crate::Heap)'s getter methods.
/// You probably shouldn't use it for anything else or at all.
pub trait RefAccess<T>: sealed::Sealed {}

/// Marker trait permitting by-mut-reference (`&mut T`) access.
///
/// Purpose of this trait is to serve as bound in [`Heap`](crate::Heap)'s getter methods.
/// You probably shouldn't use it for anything else or at all.
pub trait MutAccess<T>: RefAccess<T> {}

/// Marker trait for strong references.
///
/// Purpose of this trait is to serve as bound in [`Heap`](crate::Heap)'s getter methods.
/// You probably shouldn't use it for anything else or at all.
pub trait Rooted: sealed_root::Sealed {}

/// Marker trait for weak references.
///
/// Purpose of this trait is to serve as bound in [`Heap`](crate::Heap)'s getter methods.
/// You probably shouldn't use it for anything else or at all.
pub trait Upgradeable: sealed::Sealed + sealed_upgrade::Sealed {}

impl<T> RefAccess<T> for GcCell<T> {}
impl<T> MutAccess<T> for GcCell<T> {}
impl<T> Upgradeable for GcCell<T> {}

impl<T> RefAccess<T> for RootCell<T> {}
impl<T> MutAccess<T> for RootCell<T> {}
impl<T> Rooted for RootCell<T> {}

impl<T> RefAccess<T> for Gc<T> {}
impl<T> Upgradeable for Gc<T> {}

impl<T> RefAccess<T> for Root<T> {}
impl<T> Rooted for Root<T> {}

pub(crate) trait Index<T: ?Sized> {
    fn get<'a>(&self, arena: &'a dyn Arena) -> Option<&'a T>;
    fn get_mut<'a>(&self, arena: &'a mut dyn Arena) -> Option<&'a mut T>;
}

impl<Item, T> Index<Item> for T
where
    Item: 'static,
    T: sealed::Sealed,
{
    fn get<'a>(&self, arena: &'a dyn Arena) -> Option<&'a Item> {
        use crate::arena::{Get, Store};

        arena
            .as_any()
            .downcast_ref::<Store<Item>>()?
            .get(self.addr().0)
    }

    fn get_mut<'a>(&self, arena: &'a mut dyn Arena) -> Option<&'a mut Item> {
        use crate::arena::{Get, Store};

        arena
            .as_any_mut()
            .downcast_mut::<Store<Item>>()?
            .get_mut(self.addr().0)
    }
}
