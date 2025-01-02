//! Smart pointers and related traits.
//!
//! # Limitations
//!
//! It is important to remember that both [`Gc`] and [`GcCell`] are effectively glorified index.
//! As such they are not aware of internal state of the `Heap` and cannot respond to its changes.
//! Their representation imposes boundaries on `Heap` capabilities.
//!
//! Currently it is guaranteed that all weak references are 64-bit wide:
//!
//! ```
//! # use std::mem::size_of;
//! # use gc::{Gc, GcCell};
//! assert_eq!(size_of::<Gc<()>>(), 8);
//! assert_eq!(size_of::<GcCell<()>>(), 8);
//! ```
//!
//! Those are allocated between three parts:
//!
//! * 32 bits are value index, which denotes memory slot within their respective arena
//! * 16 bits are [generation tag](#generation-tags) of target memory slot
//! * 16 bits are [type index](#type-indices)
//!
//! Additionally it is guaranteed that all pointer types (`Gc`, `GcCell`, `Root` and `RootCell`) have a *niche*.
//! In practice it means that they will have the same size when wrapped in `Option`:
//!
//! ```
//! # use std::mem::size_of;
//! # use gc::{Gc, GcCell, Root, RootCell};
//! assert_eq!(size_of::<Gc<()>>(), size_of::<Option<Gc<()>>>());
//! assert_eq!(size_of::<GcCell<()>>(), size_of::<Option<GcCell<()>>>());
//! assert_eq!(size_of::<Root<()>>(), size_of::<Option<Root<()>>>());
//! assert_eq!(size_of::<RootCell<()>>(), size_of::<Option<RootCell<()>>>());
//! ```
//!
//! There are no guarantees about layout of strong pointers.
//! You can reasonably expect that to be bigger that weak pointer by at least a `usize` -
//! strong pointers keep an actual memory pointer internally.
//!
//! ## Generation tags
//!
//! It is possible that the value weak reference points to was deallocated
//! but then a new value was allocated in the same memory slot.
//! Logically, existing weak references should die and refuse to return references to the new value.
//! In practice this is achieved through generation tags.
//!
//! Whenever a value is allocated in a given slot it is assigned a new generation tag.
//! Weak references remember gen tag of their own value so
//! if they find that the memory slot is occupied using a different tag,
//! pointer can deduce that their value was deallocated.
//!
//! It is guaranteed that once value is deallocated all weak references become dead
//! and will never "resurrect".
//! However, combined with the fact that `Gc`/`GcCell` can exist arbitrarily long
//! it also means that generation tags cannot be ever reused.
//! In particular it implies that every memory slot can support at most `u16::MAX` allocations,
//! after which it will be marked as *dead*.
//! In effect dead memory slots leak memory as it is basically impossible to deallocate it
//! without sophisticated measures.
//!
//! The rundown here is that there is only finite (albeit quite big) number of allocation that can be performed in one heap.
//! Over long period of operation heap will slowly but inevitably leak memory.
//!
//! ## Type indices
//!
//! All references additionally preserve a type index.
//! Those indices are decided at runtime and specific to each heap.
//! It is required for normal operation of `Gc<dyn Userdata<_>>`, `Gc<dyn FullUserdata<_, _>>` and other variants.
//! Trait object versions erase the type of underlying value and we need to be able to navigate back to correct arena.
//!
//! Type indices are limited to `u16`.
//! This means you cannot allocate more that `u16::MAX` different types in one heap.
//! Note that this includes `Userdata` and `FullUserdata` variations for each type!
//! Userdata requires additional data to be functional so it is allocated separately from "normal" instances of `T`.
//! Internally when allocating `T` as userdata heap converts it to blessed types (like `LightUserdata<T>` and `FullUserdata<T>`)
//! and each of those receives its own arena.
//!
//! Technically type index is redundant for references to `Sized` types, but nonetheless useful.
//! On normal occasions it saves an extra `HashTable` lookup (`T` -> type index),
//! but also lends itself to reference downcasting.
//!
//! Reference downcasting allows you to have have both `Gc<T>` and `Gc<dyn Userdata<_>>` pointing to the same value.
//! This is somewhat tricky to achieve.
//! Because of this feature `Gc<T>` may point to one of three different arenas:
//! one for normal values, one for light userdata and one for full userdata.
//! Owning type index directly allows for very straightforward resolution to this situation.

use std::fmt::{Debug, Pointer};
use std::hash::Hash;
use std::marker::PhantomData;

use crate::heap::store::{Addr, Counter, Gen, Index};
use crate::heap::{Heap, TypeIndex};
use crate::trace::Trace;
use crate::userdata::{FullUserdata, Params, Userdata};

pub use crate::heap::interned::Interned;

mod sealed_access {
    use super::{Mut, Ref};

    pub trait Sealed {}

    impl Sealed for Ref {}

    impl Sealed for Mut {}
}

/// Trait for marking possible access permissions of a smart pointer.
pub trait Access: sealed_access::Sealed {}

/// Marker trait permitting by-reference (`&T`) access.
///
/// Purpose of this trait is to serve as bound in [`Heap`](crate::Heap)'s getter methods.
/// You probably shouldn't use it for anything else or at all.
pub trait RefAccess: Access {}

/// Marker trait permitting by-mut-reference (`&mut T`) access.
///
/// Purpose of this trait is to serve as bound in [`Heap`](crate::Heap)'s getter methods.
/// You probably shouldn't use it for anything else or at all.
pub trait MutAccess: RefAccess {}

/// Marker type denoting immutable-only access permissions of smart pointer.
pub struct Ref;

impl Access for Ref {}
impl RefAccess for Ref {}

/// Marker type denoting mutable and immutable access permissions of smart pointer.
pub struct Mut;

impl Access for Mut {}
impl RefAccess for Mut {}
impl MutAccess for Mut {}

/// Common type for all weak references.
///
/// Exact behavior of this type will differ based on access parameter.
/// For explanations and examples see [`Gc`] or [`GcCell`].
pub struct GcPtr<T: ?Sized, A: Access> {
    // We need to unpack `Addr` struct here so that rustc generates good layout.
    index: Index,
    gen: Gen,
    ty: TypeIndex,
    _type: PhantomData<T>,
    _access: PhantomData<A>,
}

impl<T, A> GcPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    pub(crate) fn new(addr: Addr, ty: TypeIndex) -> Self {
        let Addr { index, gen } = addr;

        GcPtr {
            index,
            gen,
            ty,
            _type: PhantomData,
            _access: PhantomData,
        }
    }

    pub(crate) fn ty(self) -> TypeIndex {
        self.ty
    }

    pub(crate) fn addr(self) -> Addr {
        let GcPtr { index, gen, .. } = self;

        Addr { index, gen }
    }

    pub(crate) fn transmute<U>(self) -> GcPtr<U, A>
    where
        U: ?Sized,
    {
        let GcPtr {
            index,
            gen,
            ty,
            _type,
            _access,
        } = self;

        GcPtr {
            index,
            gen,
            ty,
            _type: PhantomData,
            _access,
        }
    }

    pub(crate) fn upgrade_with(self, counter: Counter) -> RootPtr<T, A> {
        let addr = self.addr();
        let ty = self.ty();

        RootPtr::new(addr, ty, counter)
    }

    /// Upgrade weak reference into strong one.
    ///
    /// This function is a convenience wrapper around [`Heap::upgrade`].
    pub fn upgrade<M, P>(self, heap: &Heap<M, P>) -> Option<RootPtr<T, A>>
    where
        P: Params,
    {
        heap.upgrade(self)
    }

    /// Return location of referenced object.
    ///
    /// See [`Location`] struct for more information.
    pub fn location(self) -> Location<T> {
        let GcPtr {
            index,
            gen,
            ty,
            _type: _,
            _access: _,
        } = self;

        Location {
            index,
            gen,
            ty,
            _marker: PhantomData,
        }
    }

    /// Whether pointers refer to the same object.
    ///
    /// This is equivalent to comparing their locations for equality:
    ///
    /// ```
    /// # use gc::{Heap, Gc};
    /// # use gc::userdata::UnitParams;
    /// #
    /// # let mut heap = Heap::<(), UnitParams>::new();
    /// # let a = heap.alloc(1_usize).downgrade();
    /// # let b = heap.alloc(2_usize).downgrade();
    /// assert_eq!(Gc::ptr_eq(a, b), a.location() == b.location());
    /// ```
    pub fn ptr_eq(self, other: Self) -> bool {
        self.location() == other.location()
    }
}

impl<T, A> Pointer for GcPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.location())
    }
}

impl<T, A> Clone for GcPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, A> Copy for GcPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
}

impl<T, A> AsRef<Self> for GcPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    fn as_ref(&self) -> &Self {
        self
    }
}

/// Common type for all strong references.
///
/// Exact behavior of this type will differ based on access parameter.
/// For explanations and examples see [`Root`] or [`RootCell`].
pub struct RootPtr<T: ?Sized, A: Access> {
    // We need to unpack `Addr` struct here so that rustc generates good layout.
    index: Index,
    gen: Gen,
    ty: TypeIndex,
    counter: Counter,
    _type: PhantomData<T>,
    _access: PhantomData<A>,
}

impl<T, A> RootPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    pub(crate) fn new(addr: Addr, ty: TypeIndex, counter: Counter) -> Self {
        let Addr { index, gen } = addr;

        RootPtr {
            index,
            gen,
            ty,
            counter,
            _type: PhantomData,
            _access: PhantomData,
        }
    }

    /// Downgrade into weak reference.
    pub fn downgrade(&self) -> GcPtr<T, A> {
        GcPtr::new(self.addr(), self.ty)
    }

    pub(crate) fn ty(&self) -> TypeIndex {
        self.ty
    }

    pub(crate) fn addr(&self) -> Addr {
        let RootPtr { index, gen, .. } = self;

        Addr {
            index: *index,
            gen: *gen,
        }
    }

    pub(crate) fn counter(&self) -> &Counter {
        &self.counter
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
    /// # use gc::userdata::UnitParams;
    /// #
    /// # let mut heap = Heap::<(), UnitParams>::new();
    /// # let a = heap.alloc(1_usize);
    /// # let b = heap.alloc(2_usize);
    /// assert_eq!(Root::ptr_eq(&a, &b), a.location() == b.location());
    /// ```
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.location() == other.location()
    }
}

impl<T, A> Pointer for RootPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.location())
    }
}

impl<T, A> Clone for RootPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    fn clone(&self) -> Self {
        let RootPtr {
            index,
            gen,
            ty,
            counter,
            _type: _,
            _access: _,
        } = self;

        Self {
            index: *index,
            gen: *gen,
            ty: *ty,
            counter: counter.clone(),
            _type: PhantomData,
            _access: PhantomData,
        }
    }
}

impl<T, A> From<RootPtr<T, A>> for GcPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    fn from(value: RootPtr<T, A>) -> Self {
        value.downgrade()
    }
}

impl<T, A> AsRef<Self> for RootPtr<T, A>
where
    T: ?Sized,
    A: Access,
{
    fn as_ref(&self) -> &Self {
        self
    }
}

/// A weak reference to gc-allocated mutable value.
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
/// as if applied to an underlying pointer consider using [`GcCell::location`].
///
/// # Construct
///
/// Common way to construct [`GcCell`] is to downgrade [`RootCell`]:
///
/// ```
/// # use gc::{Heap, GcCell, RootCell};
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// let strong = heap.alloc_cell(3);
/// let weak: GcCell<usize> = strong.downgrade();
/// ```
///
/// Converting `GcCell` back to `RootCell` requires access to heap:
///
/// ```
/// # use gc::{Heap, GcCell, RootCell};
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// # let weak = heap.alloc_cell(3_usize).downgrade();
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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
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
/// # use gc::{Heap, GcCell, RootCell};
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// let weak = heap.alloc_cell(3_usize).downgrade();
/// assert_eq!(heap.get(weak), Some(&3));
///
/// let ref_mut: &mut usize = heap.get_mut(weak).expect("object is still alive");
/// *ref_mut = 4;
/// assert_eq!(heap.get(weak), Some(&4));
/// ```
pub type GcCell<T> = GcPtr<T, Mut>;

impl<T> Debug for GcCell<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Gc")
            .field("addr", &self.addr())
            .finish_non_exhaustive()
    }
}

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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// let strong: RootCell<usize> = heap.alloc_cell(3);
/// ```
///
/// You can also clone already existing roots or upgrade weak references:
///
/// ```
/// # use gc::{Heap, RootCell};
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// # let strong = heap.alloc_cell(4_usize);
/// assert_eq!(heap[&strong], 4);
///
/// heap[&strong] = 3;
/// assert_eq!(heap[&strong], 3);
/// ```
pub type RootCell<T> = RootPtr<T, Mut>;

impl<T> Debug for RootCell<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Root")
            .field("addr", &self.addr())
            .finish_non_exhaustive()
    }
}

/// A weak reference to gc-allocated immutable value.
///
/// This reference is an equivalent of `Weak<T>`:
///
/// * The reference is *weak* as in it alone won't prevent value from being dropped.
///     However when part of certainly alive object
///     it will indicate that referencee is also alive through [`Trace`] trait.
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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// let strong = heap.alloc(3);
/// let weak: Gc<usize> = strong.downgrade();
/// ```
///
/// Converting `Gc` back to `Root` requires access to heap:
///
/// ```
/// # use gc::{Heap, Gc, Root};
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// let weak = heap.alloc(3_usize).downgrade();
/// assert_eq!(heap.get(weak), Some(&3));
///
/// // Object have no strong references left so it will be collected.
/// heap.gc();
/// assert_eq!(heap.get(weak), None);
/// ```
pub type Gc<T> = GcPtr<T, Ref>;

impl<T> Debug for Gc<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Gc")
            .field("addr", &self.addr())
            .finish_non_exhaustive()
    }
}

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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// let strong: Root<usize> = heap.alloc(3);
/// ```
///
/// You can also clone already existing roots or upgrade weak references:
///
/// ```
/// # use gc::{Heap, Root};
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
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
/// # use gc::userdata::UnitParams;
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// # let strong = heap.alloc(3_usize);
/// assert_eq!(heap[&strong], 3);
/// ```
pub type Root<T> = RootPtr<T, Ref>;

impl<T> Debug for Root<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Root")
            .field("addr", &self.addr())
            .finish_non_exhaustive()
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
    // We need to unpack `Addr` struct here so that rustc generates good layout.
    index: Index,
    gen: Gen,
    ty: TypeIndex,
    _marker: PhantomData<T>,
}

impl<T> Clone for Location<T>
where
    T: ?Sized,
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Location<T> where T: ?Sized {}

impl<T> PartialEq for Location<T>
where
    T: ?Sized,
{
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
            && self.gen == other.gen
            && self.ty == other.ty
            && self._marker == other._marker
    }
}

impl<T> Eq for Location<T> where T: ?Sized {}

impl<T> PartialOrd for Location<T>
where
    T: ?Sized,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Location<T>
where
    T: ?Sized,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering;

        match self.index.cmp(&other.index) {
            Ordering::Equal => (),
            ord => return ord,
        };

        match self.gen.cmp(&other.gen) {
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

impl<T> Hash for Location<T>
where
    T: ?Sized,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.gen.hash(state);
        self.ty.hash(state);
        self._marker.hash(state);
    }
}

impl<T> Debug for Location<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Location")
            .field("index", &self.index)
            .field("gen", &self.gen)
            .finish_non_exhaustive()
    }
}

impl<T> Pointer for Location<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}:{}", self.index, self.gen)
    }
}

pub(crate) mod sealed_allocated {
    use super::{FullUserdata, Heap, Params, Userdata};

    #[doc(hidden)]
    pub struct Ty(pub(crate) super::TypeIndex);

    #[doc(hidden)]
    pub struct Addr(pub(crate) super::Addr);

    pub trait Sealed<Heap> {
        #[doc(hidden)]
        fn get_ref(heap: &Heap, ty: Ty, addr: Addr) -> Option<&Self>;

        #[doc(hidden)]
        fn get_mut(heap: &mut Heap, ty: Ty, addr: Addr) -> Option<&mut Self>;
    }

    impl<T, M, P> Sealed<Heap<M, P>> for T
    where
        T: 'static,
        P: Params,
    {
        fn get_ref(heap: &Heap<M, P>, ty: Ty, addr: Addr) -> Option<&Self> {
            heap.arena(ty.0)?.get(addr.0)
        }

        fn get_mut(heap: &mut Heap<M, P>, ty: Ty, addr: Addr) -> Option<&mut Self> {
            heap.arena_mut(ty.0)?.get_mut(addr.0)
        }
    }

    impl<M, P> Sealed<Heap<M, P>> for dyn Userdata<P>
    where
        P: Params,
    {
        fn get_ref(heap: &Heap<M, P>, ty: Ty, addr: Addr) -> Option<&Self> {
            heap.arena(ty.0)?.get_userdata(addr.0)
        }

        fn get_mut(heap: &mut Heap<M, P>, ty: Ty, addr: Addr) -> Option<&mut Self> {
            heap.arena_mut(ty.0)?.get_userdata_mut(addr.0)
        }
    }

    impl<M, P> Sealed<Heap<M, P>> for dyn FullUserdata<M, P>
    where
        P: Params,
    {
        fn get_ref(heap: &Heap<M, P>, ty: Ty, addr: Addr) -> Option<&Self> {
            heap.arena(ty.0)?.get_full_userdata(addr.0)
        }

        fn get_mut(heap: &mut Heap<M, P>, ty: Ty, addr: Addr) -> Option<&mut Self> {
            heap.arena_mut(ty.0)?.get_full_userdata_mut(addr.0)
        }
    }
}

pub(crate) mod sealed_allocate_as {
    use super::{FullUserdata, Params, Trace, Userdata};
    use crate::heap::Heap;

    #[doc(hidden)]
    pub struct Addr(pub(crate) super::Addr);

    #[doc(hidden)]
    pub struct TypeIndex(pub(crate) super::TypeIndex);

    #[doc(hidden)]
    pub struct Counter(pub(crate) super::Counter);

    pub trait Sealed<T: ?Sized, Heap>: Sized {
        #[doc(hidden)]
        fn alloc_into(self, heap: &mut Heap) -> (Addr, TypeIndex, Counter);

        #[doc(hidden)]
        fn try_alloc_into(self, heap: &mut Heap) -> Result<(Addr, TypeIndex, Counter), Self>;
    }

    impl<T, M, P> Sealed<T, Heap<M, P>> for T
    where
        T: Trace,
        P: Params,
    {
        fn alloc_into(self, heap: &mut Heap<M, P>) -> (Addr, TypeIndex, Counter) {
            use crate::heap::userdata_store::Concrete;

            let value = Concrete::new(self);

            let (addr, ty, counter) = heap.alloc_inner(value);
            (Addr(addr), TypeIndex(ty), Counter(counter))
        }

        fn try_alloc_into(self, heap: &mut Heap<M, P>) -> Result<(Addr, TypeIndex, Counter), Self> {
            use crate::heap::userdata_store::Concrete;

            let value = Concrete::new(self);

            let (addr, ty, counter) = heap.try_alloc_inner(value).map_err(|t| t.value)?;
            Ok((Addr(addr), TypeIndex(ty), Counter(counter)))
        }
    }

    impl<T, M, P> Sealed<dyn Userdata<P>, Heap<M, P>> for T
    where
        T: Trace,
        P: Params,
    {
        fn alloc_into(self, heap: &mut Heap<M, P>) -> (Addr, TypeIndex, Counter) {
            use crate::heap::userdata_store::LightUd;

            let value = LightUd::new(self);

            let (addr, ty, counter) = heap.alloc_inner(value);
            (Addr(addr), TypeIndex(ty), Counter(counter))
        }

        fn try_alloc_into(self, heap: &mut Heap<M, P>) -> Result<(Addr, TypeIndex, Counter), Self> {
            use crate::heap::userdata_store::LightUd;

            let value = LightUd::new(self);

            let (addr, ty, counter) = heap.try_alloc_inner(value).map_err(|t| t.value)?;
            Ok((Addr(addr), TypeIndex(ty), Counter(counter)))
        }
    }

    impl<T, M, P> Sealed<dyn FullUserdata<M, P>, Heap<M, P>> for T
    where
        T: Trace,
        M: Trace + Clone,
        P: Params,
    {
        fn alloc_into(self, heap: &mut Heap<M, P>) -> (Addr, TypeIndex, Counter) {
            use crate::heap::userdata_store::FullUd;

            let metatable = heap.metatable_of::<T>();
            let value = FullUd::new(self, metatable.cloned());

            let (addr, ty, counter) = heap.alloc_inner(value);
            (Addr(addr), TypeIndex(ty), Counter(counter))
        }

        fn try_alloc_into(self, heap: &mut Heap<M, P>) -> Result<(Addr, TypeIndex, Counter), Self> {
            use crate::heap::userdata_store::FullUd;

            let metatable = heap.metatable_of::<T>();
            let value = FullUd::new(self, metatable.cloned());

            let (addr, ty, counter) = heap.try_alloc_inner(value).map_err(|t| t.value)?;
            Ok((Addr(addr), TypeIndex(ty), Counter(counter)))
        }
    }
}

/// Marker trait for types that can be retrieved from [`Heap`](crate::Heap).
///
/// Purpose of this trait is to serve as bound in `Heap`'s getter methods.
/// You probably shouldn't use it for anything else or at all.
///
/// The trait is implemented for all types references to which can be extracted out of heap,
/// that is for which you can go from `Gc<T>` to `&T` for example.
/// This is never a problem for *sized* types, however this is not true about *unsized* types.
/// To extract a reference to unsized type you either need to reconstruct it from raw parts
/// (which is impossible until Rust provides mechanism to interact with that)
/// or have dedicated internal API methods for each individual unsized type you want to extract.
/// The reason this trait exists is to mark those few blessed unsized types that we support.
pub trait Allocated<Heap>: sealed_allocated::Sealed<Heap> {}

/// Marker trait for types that can be allocated in `Heap`.
///
/// Purpose of this trait is to serve as bound in `Heap`'s allocation methods.
/// You probably shouldn't use it for anything else or at all.
pub trait AllocateAs<T: ?Sized, Heap>: sealed_allocate_as::Sealed<T, Heap> {}

impl<T, M, P> Allocated<Heap<M, P>> for T
where
    T: 'static,
    P: Params,
{
}

impl<M, P> Allocated<Heap<M, P>> for dyn Userdata<P> where P: Params {}

impl<M, P> Allocated<Heap<M, P>> for dyn FullUserdata<M, P> where P: Params {}

impl<T, M, P> AllocateAs<T, Heap<M, P>> for T
where
    T: Trace,
    P: Params,
{
}

impl<T, M, P> AllocateAs<dyn Userdata<P>, Heap<M, P>> for T
where
    T: Trace,
    P: Params,
{
}

impl<T, M, P> AllocateAs<dyn FullUserdata<M, P>, Heap<M, P>> for T
where
    T: Trace,
    M: Trace + Clone,
    P: Params,
{
}

pub trait ToOwned<Owned> {
    fn to_owned(&self) -> Owned;
}

impl<T> ToOwned<T> for T
where
    T: Clone,
{
    fn to_owned(&self) -> T {
        self.clone()
    }
}

impl ToOwned<String> for str {
    fn to_owned(&self) -> String {
        self.to_string()
    }
}

impl<T> ToOwned<Vec<T>> for [T]
where
    T: Clone,
{
    fn to_owned(&self) -> Vec<T> {
        self.into()
    }
}
