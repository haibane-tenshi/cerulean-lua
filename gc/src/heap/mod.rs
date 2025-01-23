pub(crate) mod arena;
pub(crate) mod interned;
pub(crate) mod store;
pub(crate) mod userdata_store;

use std::any::TypeId;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{Index, IndexMut};

use bitvec::vec::BitVec;
use typed_index_collections::TiVec;

use crate::index::{
    Access, AllocateAs, Allocated, GcPtr, MutAccess, RefAccess, Root, RootCell, RootPtr, ToOwned,
};
use crate::trace::Trace;
use crate::userdata::{Dispatcher, FullUserdata, Params};
use arena::{Arena, Insert};
use interned::{Interned, InternedStore};
use store::{Addr, Counter};

#[expect(unused_imports, reason = "frequently referenced in doc comments")]
use crate::index::{Gc, GcCell};

/// Backing store for garbage-collected objects.
///
/// Allocated objects require to implement [`Trace`] trait
/// which allows garbage collector to discover transitive references through an object.
/// Note that `Trace` requires `'static`: this is due to internal use of [`Any`](std::any::Any)
/// so unfortunately you cannot allocate objects containing non-static lifetimes.
///
/// # Generics
///
/// Heap takes two generic parameters.
/// Both are an unfortunate consequence of tight integration with Lua userdata.
///
/// `M` is the type of *metatable* used by [`FullUserdata`] trait.
/// There are little restrictions on it besides `'static` bounds.
/// Note that [`alloc_as`](Heap::alloc_as) requires `M: Clone` when allocating as `dyn FullUserdata` to clone default metatable.
///
/// `P` contains parameters configuring [`Userdata`](crate::userdata::Userdata) trait.
/// For this reason you will need to satisfy [`P: Params`](Params) bound.
/// It primarily exists to prevent spilling runtime-related details over garbage collector (and let it reside in a different crate).
///
/// # Allocating
///
/// There is a host of methods provided by heap to allocate values.
/// The simplest two are
///
/// * [`alloc`](Heap::alloc) - allocates `T` as [`Root<T>`]
/// * [`alloc_cell`](Heap::alloc_cell) - allocates `T` as [`RootCell<T>`]
///
/// Result differs in mutability of allocated value.
/// `Root`s are immutable:
///
/// ```rust
/// # use gc::{Heap, Root, RootCell};
/// # use gc::userdata::UnitParams;
/// # let mut heap = Heap::<_, _>::new();
/// # let mut heap = std::convert::identity::<Heap<(), UnitParams>>(heap);
/// let a: RootCell<usize> = heap.alloc_cell(3_usize);
/// assert_eq!(heap[&a], 3);
///
/// heap[&a] = 4;
/// assert_eq!(heap[&a], 4);
///
/// let b: Root<usize> = heap.alloc(3_usize);
/// assert_eq!(heap[&b], 3);
///
/// // Cannot mutate values behind Root/Gc
/// // heap[&b] = 4;
/// ```
///
/// See documentation of smart pointers for more details.
///
/// The most versatile allocation method is [`alloc_as`](Heap::alloc_as).
/// It can produce smart pointers of different types depending on type hints.
/// While normally you may want to allocate values as-is:
///
/// ```rust
/// # use gc::{Heap, Root};
/// # use gc::userdata::UnitParams;
/// # let mut heap = Heap::<_, _>::new();
/// # let mut heap = std::convert::identity::<Heap<(), UnitParams>>(heap);
/// let a: Root<usize> = heap.alloc_as(3_usize);
/// ```
///
/// It is also possible to allocate values as trait objects for a few blessed traits:
///
/// ```rust
/// # use gc::{Heap, Root};
/// # use gc::userdata::{UnitParams, FullUserdata};
/// # let mut heap = Heap::<_, _>::new();
/// # let mut heap = std::convert::identity::<Heap<(), UnitParams>>(heap);
/// let a: Root<dyn FullUserdata<_, _>> = heap.alloc_as(3_usize);
/// ```
///
/// `alloc_as` is versatile, but it also make it less convenient to use in many circumstances.
///
/// ## Handling userdata
///
/// Heap permits to allocate object as trait objects implementing Lua userdata traits.
/// There are a few peculiarities here:
///
/// *   *Any* type can be allocated as userdata.
///     This is because [`Userdata`](crate::userdata::Userdata)/[`FullUserdata`] are a surrogate traits as explained in [design doc](crate::userdata#userdata-design).
/// *   To complete userdata every type requires a *method dispatcher* that must be separately configured.
/// *   Additionally, `FullUserdata` requires a *metatable* to be allocated alongside it.
///     It can be either provided explicitly using [`alloc_full_userdata`](Heap::alloc_full_userdata) method,
///     or when using other allocation methods ([`alloc_as`](Heap::alloc_as)) cloned from default metatable that must be configured separately.
///
/// Default metatable can be set or acquired using the following methods:
///
/// * [`set_metatable_of`](Heap::set_metatable_of)
/// * [`metatable_of`](Heap::metatable_of)
///
/// Default metatable is as the name implies only used to initiate metatables of newly allocated objects
/// and have no impact on already existing objects.
///
/// You can set *method dispatcher* for a type using
///
/// * [`set_dispatcher_of`](Heap::set_dispatcher_of)
///
/// As noted in userdata design doc, **all values of the same type share single dispatcher**.
/// Replacing existing dispatcher will immediately affect all already allocated values of that type.
///
/// Currently, for technical reasons, dispatchers are required to be function pointers.
///
/// ## Interning objects
///
/// Heap has dedicated APIs to [`intern`](Heap::intern) objects.
/// Interned values are deduplicated:
///
/// ```
/// # use gc::{Heap, Root, Interned};
/// # use gc::userdata::{UnitParams, FullUserdata};
/// # let mut heap = Heap::<_, _>::new();
/// # let mut heap = std::convert::identity::<Heap<(), UnitParams>>(heap);
/// let a: Root<Interned<usize>> = heap.intern(3_usize);
/// let b: Root<Interned<usize>> = heap.intern(3_usize);
///
/// assert_eq!(a.location(), b.location());
/// ```
///
/// Because of this guarantee, interned object can only exist behind `Root`.
/// It is a logical error to modify interned objects in any way that affects `Eq` or `Hash` (e.g. through internal mutability).
///
/// There is also a secondary interning API, [`intern_from`](Heap::intern_from):
///
/// ```
/// # use gc::{Heap, Root, Interned};
/// # use gc::userdata::{UnitParams, FullUserdata};
/// # let mut heap = Heap::<_, _>::new();
/// # let mut heap = std::convert::identity::<Heap<(), UnitParams>>(heap);
/// let a: Root<Interned<String>> = heap.intern(String::from("foobar"));
/// let b: Root<Interned<String>> = heap.intern_from("foobar");
///
/// assert_eq!(a.location(), b.location());
/// ```
///
/// Commonly, object you are trying to intern might already exist,
/// however you still need to provide one to `intern` method - even if it will be immediately discarded,
/// which can be extremely wasteful.
///
/// `intern_from` instead takes *a reference* to another object and uses that to search for an already existing allocation.
/// The simplest way to use it is to pass `&T` and `T` will be cloned only when necessary.
/// Alternatively, by providing proper [`Borrow`] and [`ToOwned`] impls you can pass reference to some other type.
/// Those already exist for `str`/`String` and `[T]`/`Vec<T>` pairings.
///
/// # Retrieving objects
///
/// Dereferencing requires access to heap:
///
/// ```
/// # use gc::{Heap};
/// # use gc::userdata::UnitParams;
/// # let mut heap = Heap::<_, _>::new();
/// # let mut heap = std::convert::identity::<Heap<(), UnitParams>>(heap);
/// let a = heap.alloc(3_usize);
///
/// assert_eq!(heap[&a], 3);
/// ```
///
/// # Garbage collection
///
/// Garbage collection may be triggered automatically on any allocation function:
///
/// * [`alloc`](Heap::alloc)
/// * [`alloc_cell`](Heap::alloc_cell),
/// * [`alloc_as`](Heap::alloc_as)
/// * [`alloc_full_userdata`](Heap::alloc_full_userdata)
///
/// Alternatively it can be called manually via [`gc`](Heap::gc) method.
///
/// Implementation uses standard mark-and-sweep strategy.
///
/// ## Avoiding garbage collection
///
/// There are situations where you may not want to trigger a gc pass while allocating objects.
/// There are currently two APIs available to this purpose:
///
/// * [`pause`](Heap::pause) will suspend garbage collection during the execution of provided closure
/// * `try_*` APIs ([`try_alloc`](Heap::try_alloc), [`try_alloc_cell`](Heap::try_alloc_cell), [`try_alloc_as`](Heap::try_alloc_as))
///     are fallible but will never trigger gc pass.
///     Those function will attempt to reuse existing space (without any additional allocations) and fail otherwise.
pub struct Heap<M, P> {
    type_map: HashMap<TypeId, TypeIndex>,
    arenas: TiVec<TypeIndex, Box<dyn Arena<M, P>>>,
    metatables: HashMap<TypeId, M>,
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

    pub(crate) fn arena(&self, ty: TypeIndex) -> Option<&(dyn Arena<M, P> + 'static)> {
        self.arenas.get(ty).map(AsRef::as_ref)
    }

    pub(crate) fn arena_mut(&mut self, ty: TypeIndex) -> Option<&mut (dyn Arena<M, P> + 'static)> {
        self.arenas.get_mut(ty).map(AsMut::as_mut)
    }

    fn arena_or_insert_with<T, A>(&mut self, f: impl FnOnce() -> A) -> (TypeIndex, &mut A)
    where
        T: Trace,
        A: Arena<M, P> + 'static,
    {
        let id = TypeId::of::<T>();
        let index = *self
            .type_map
            .entry(id)
            .or_insert_with(|| self.arenas.push_and_get_key(Box::new(f())));
        let store = self.arenas.get_mut(index).unwrap().as_mut();
        let store: &mut A = store.downcast_mut().unwrap();
        (index, store)
    }

    #[inline(never)]
    fn alloc_slow<T, A>(&mut self, ty: TypeIndex, value: T) -> (Addr, Counter)
    where
        T: Trace,
        A: Insert<T> + 'static,
    {
        self.gc_with(&value, false);
        let arena: &mut A = self.arenas.get_mut(ty).unwrap().downcast_mut().unwrap();

        arena.insert(value)
    }

    fn alloc_inner<T, A>(&mut self, value: T, f: impl FnOnce() -> A) -> (Addr, TypeIndex, Counter)
    where
        T: Trace,
        A: Arena<M, P> + Insert<T> + 'static,
    {
        let (ty, arena) = self.arena_or_insert_with::<T, _>(f);
        let (addr, counter) = match arena.try_insert(value) {
            Ok(ptr) => ptr,
            Err(value) => self.alloc_slow::<_, A>(ty, value),
        };

        (addr, ty, counter)
    }

    fn try_alloc_inner<T, A>(
        &mut self,
        value: T,
        f: impl FnOnce() -> A,
    ) -> Result<(Addr, TypeIndex, Counter), T>
    where
        T: Trace,
        A: Arena<M, P> + Insert<T> + 'static,
    {
        let (ty, arena) = self.arena_or_insert_with::<T, _>(f);

        arena
            .try_insert(value)
            .map(|(addr, counter)| (addr, ty, counter))
    }

    fn pure_alloc<T, A>(&mut self, value: T, f: impl FnOnce() -> A) -> (Addr, TypeIndex, Counter)
    where
        T: Trace,
        A: Arena<M, P> + Insert<T> + 'static,
    {
        let (ty, arena) = self.arena_or_insert_with::<T, _>(f);
        let (addr, counter) = arena.insert(value);
        (addr, ty, counter)
    }

    pub(crate) fn alloc_verbatim<T>(&mut self, value: T) -> (Addr, TypeIndex, Counter)
    where
        T: Trace,
    {
        use store::Store;

        self.alloc_inner(value, || Store::<T>::new())
    }

    pub(crate) fn try_alloc_verbatim<T>(
        &mut self,
        value: T,
    ) -> Result<(Addr, TypeIndex, Counter), T>
    where
        T: Trace,
    {
        use store::Store;

        self.try_alloc_inner(value, || Store::<T>::new())
    }

    pub(crate) fn alloc_userdata<T>(
        &mut self,
        value: T,
        metatable: Option<M>,
    ) -> (Addr, TypeIndex, Counter)
    where
        T: Trace,
        M: Trace,
    {
        use crate::heap::userdata_store::{UserdataObject, UserdataStore};
        use crate::index::WeakRoot;

        let (addr, ty, counter) = self.alloc_verbatim(value);

        let root = WeakRoot::<T>::new(addr, ty, counter.weaken());
        let value = UserdataObject::new(root, metatable);

        self.alloc_inner(value, || UserdataStore::<T, P, M>::new())
    }

    pub(crate) fn try_alloc_userdata<T>(
        &mut self,
        value: T,
        metatable: Option<M>,
    ) -> Result<(Addr, TypeIndex, Counter), T>
    where
        T: Trace,
        M: Trace,
    {
        use crate::heap::userdata_store::{UserdataObject, UserdataStore};
        use crate::index::WeakRoot;

        let (addr, ty, counter) = self.try_alloc_verbatim(value)?;

        let root = WeakRoot::<T>::new(addr, ty, counter.weaken());
        let value = UserdataObject::new(root, metatable);

        // Unfortunately, failing this alloc is problematic, since the value itself is already embedded.
        // Instead we force non-gc allocation.
        // Possibly, can be done better by querying whether arena have free space instead.
        let r = self.pure_alloc(value, || UserdataStore::<T, P, M>::new());
        Ok(r)
    }

    /// Convenience function to allocate `T` as [`Root<T>`].
    ///
    /// `Root`s only permit immutable access to value.
    /// If you intend to mutate it later consider using [`Heap::alloc_cell`].
    ///
    /// If you intend to allocate value as userdata consider using [`Heap::alloc_as`] or [`Heap::alloc_full_userdata`].
    pub fn alloc<T>(&mut self, value: T) -> Root<T>
    where
        T: Trace,
    {
        self.alloc_as(value)
    }

    /// Convenience function to allocate `T` as [`RootCell<T>`].
    ///
    /// `RootCell`s permit both mutable and immutable access to value.
    /// If you don't intend to mutate it later consider using [`Heap::alloc`].
    ///
    /// If you intend to allocate value as userdata consider using [`Heap::alloc_as`] or [`Heap::alloc_full_userdata`].
    pub fn alloc_cell<T>(&mut self, value: T) -> RootCell<T>
    where
        T: Trace,
    {
        self.alloc_as(value)
    }

    /// Allocate an object.
    ///
    /// This function can output one of 6 different pointers:
    ///
    /// * `Root<T>`
    /// * `RootCell<T>`
    /// * `Root<dyn Userdata<P>>`
    /// * `RootCell<dyn Userdata<P>>`
    /// * `Root<dyn FullUserdata<M, P>>`
    /// * `RootCell<dyn FullUserdata<M, P>>`
    ///
    /// As such it requires type annotations when used since compiler cannot always deduce intended type.
    ///
    /// [`Root`]s reference immutable values, whereas [`RootCell`]s reference mutable values.
    ///
    /// # On userdata
    ///
    /// Userdata is mechanism for Lua to ineract with foreign types.
    /// See [module-level explanations](crate::userdata#userdata-design) for more details.
    ///
    /// When allocating userdata through this method other parts will be configured for you:
    ///
    /// * All values of one type share dispacher function by design.
    ///    Currently configured function will be used, otherwise a default "do nothing" dispacher will be set.
    ///    Dispacher method needs to be configured separately using [`set_dispatcher_of`](Heap::set_dispatcher_of) method.
    /// * `FullUserdata` will use metatable for the type if configured, otherwise no metatable will be set.
    ///
    /// Alternatively full userdata can be allocated using [`alloc_full_userdata`](Heap::alloc_full_userdata) which allows you to set its metatable immediately.
    /// Note that [`Metatable::set_metatable`](crate::userdata::Metatable::set_metatable) requires mutable access,
    /// so you will not be able to change metatable of `Root<dyn FullUserdata<_, _>>` once allocated.
    pub fn alloc_as<U, T, A>(&mut self, value: T) -> RootPtr<U, A>
    where
        T: Trace + AllocateAs<U, Self>,
        U: ?Sized,
        A: Access,
    {
        use crate::index::sealed_allocate_as::{Addr, Counter, TypeIndex};

        let (Addr(addr), TypeIndex(ty), Counter(counter)) = value.alloc_into(self);

        RootPtr::new(addr, ty, counter)
    }

    /// Allocate an object as full userdata.
    ///
    /// This function can output 2 different pointers depending on type hints:
    ///
    /// * `Root<dyn FullUserdata<M, P>>`
    /// * `RootCell<dyn FullUserdata<M, P>>`
    ///
    /// Unlike [`alloc_as`](Heap::alloc_as) this function allows you to immediately set metatable for the value.
    ///
    /// Note that [`Metatable::set_metatable`](crate::userdata::Metatable::set_metatable) requires mutable access,
    /// so you will not be able to change metatable of `Root<dyn FullUserdata<_, _>>` once allocated.
    pub fn alloc_full_userdata<T, A>(
        &mut self,
        value: T,
        metatable: Option<M>,
    ) -> RootPtr<dyn FullUserdata<M, P>, A>
    where
        T: Trace,
        M: Trace,
        A: Access,
    {
        let (addr, ty, counter) = self.alloc_userdata(value, metatable);
        RootPtr::new(addr, ty, counter)
    }

    /// Convenience function to allocate `T` as [`Root<T>`].
    ///
    /// This function will never trigger garbage collection.
    /// It will use preallocated free space to allocate the value,
    /// otherwise original object will be returned.
    ///
    /// `Root`s only permit immutable access to value.
    /// If you intend to mutate it later consider using [`Heap::try_alloc_cell`].
    ///
    /// If you intend to allocate value as userdata consider using [`Heap::try_alloc_as`] or [`Heap::alloc_full_userdata`].
    pub fn try_alloc<T>(&mut self, value: T) -> Result<Root<T>, T>
    where
        T: Trace,
    {
        self.try_alloc_as(value)
    }

    /// Convenience function to allocate `T` as [`RootCell<T>`].
    ///
    /// This function will never trigger garbage collection.
    /// It will use preallocated free space to allocate the value,
    /// otherwise original object will be returned.
    ///
    /// `RootCell`s permit both mutable and immutable access to value.
    /// If you don't intend to mutate it later consider using [`Heap::alloc`].
    ///
    /// If you intend to allocate value as userdata consider using [`Heap::alloc_as`] or [`Heap::alloc_full_userdata`].
    pub fn try_alloc_cell<T>(&mut self, value: T) -> Result<RootCell<T>, T>
    where
        T: Trace,
    {
        self.try_alloc_as(value)
    }

    /// Allocate an object.
    ///
    /// This function will never trigger garbage collection.
    /// It will use preallocated free space to allocate the value,
    /// otherwise original object will be returned.
    ///
    /// This function can output one of 6 different pointers:
    ///
    /// * `Root<T>`
    /// * `RootCell<T>`
    /// * `Root<dyn Userdata<P>>`
    /// * `RootCell<dyn Userdata<P>>`
    /// * `Root<dyn FullUserdata<M, P>>`
    /// * `RootCell<dyn FullUserdata<M, P>>`
    ///
    /// As such it requires type annotations when used since compiler cannot always deduce intended type.
    ///
    /// [`Root`]s reference immutable values, whereas [`RootCell`]s reference mutable values.
    ///
    /// # On userdata
    ///
    /// Userdata is mechanism for Lua to ineract with foreign types.
    /// See [module-level explanations](crate::userdata#userdata-design) for more details.
    ///
    /// When allocating userdata through this method other parts will be configured for you:
    ///
    /// * All values of one type share dispacher function by design.
    ///    Currently configured function will be used, otherwise a default "do nothing" dispacher will be set.
    ///    Dispacher method needs to be configured separately using [`set_dispatcher_of`](Heap::set_dispatcher_of) method.
    /// * `FullUserdata` will use metatable for the type if configured, otherwise no metatable will be set.
    ///
    /// Alternatively full userdata can be allocated using [`alloc_full_userdata`](Heap::alloc_full_userdata) which allows you to set its metatable immediately.
    /// Note that [`Metatable::set_metatable`](crate::userdata::Metatable::set_metatable) requires mutable access,
    /// so you will not be able to change metatable of `Root<dyn FullUserdata<_, _>>` once allocated.
    pub fn try_alloc_as<U, T, A>(&mut self, value: T) -> Result<RootPtr<U, A>, T>
    where
        T: Trace + AllocateAs<U, Self>,
        U: ?Sized,
        A: Access,
    {
        use crate::index::sealed_allocate_as::{Addr, Counter, TypeIndex};

        let (Addr(addr), TypeIndex(ty), Counter(counter)) = value.try_alloc_into(self)?;

        Ok(RootPtr::new(addr, ty, counter))
    }

    /// Intern a value.
    ///
    /// Interning deduplicates values, that is allocating two values that compare equal (in sense given by `Eq` trait)
    /// will result only in single actual allocation with both references pointing to it.
    /// Because of this it is not possible to acquire mutable reference to interned values:
    /// it is possible to modify an existing value in such way that it would be equal to another (already allocated) value, breaking the contract.
    ///
    /// Interning have some useful properties, for example, since all instances of the same value share the same location in memory,
    /// comparing the locations is equivalent to comparing values for equality.
    ///
    /// It is a logical error to modify allocated value in such way that affects implementation of `Hash` or `Eq` (e.g. via internal mutability).
    pub fn intern<T>(&mut self, value: T) -> Root<Interned<T>>
    where
        T: Trace + Hash + Eq,
    {
        let (ty, arena) = self.arena_or_insert_with::<Interned<T>, _>(|| InternedStore::<T>::new());
        let (addr, counter) = match arena.try_insert(value) {
            Ok(r) => r,
            Err(value) => {
                self.gc_with(&value, false);
                let arena: &mut InternedStore<T> =
                    self.arena_mut(ty).unwrap().downcast_mut().unwrap();
                arena.insert(value)
            }
        };

        Root::new(addr, ty, counter)
    }

    pub fn intern_from<U, T>(&mut self, value: &U) -> Root<Interned<T>>
    where
        U: Hash + Eq + ToOwned<T> + ?Sized,
        T: Trace + Hash + Eq + Borrow<U>,
    {
        let (ty, arena) = self.arena_or_insert_with::<Interned<T>, _>(|| InternedStore::<T>::new());
        let (addr, counter) = match arena.try_insert_from(value) {
            Ok(r) => r,
            Err(value) => {
                self.gc_with(&value, false);
                let arena: &mut InternedStore<T> =
                    self.arena_mut(ty).unwrap().downcast_mut().unwrap();
                arena.insert(value)
            }
        };

        Root::new(addr, ty, counter)
    }

    pub fn try_intern<T>(&mut self, value: T) -> Result<Root<Interned<T>>, T>
    where
        T: Trace + Hash + Eq,
    {
        let (ty, arena) = self.arena_or_insert_with::<Interned<T>, _>(|| InternedStore::<T>::new());
        let (addr, counter) = arena.try_insert(value)?;

        Ok(Root::new(addr, ty, counter))
    }

    pub fn try_intern_from<U, T>(&mut self, value: &U) -> Result<Root<Interned<T>>, T>
    where
        U: Hash + Eq + ToOwned<T> + ?Sized,
        T: Trace + Hash + Eq + Borrow<U>,
    {
        let (ty, arena) = self.arena_or_insert_with::<Interned<T>, _>(|| InternedStore::<T>::new());
        let (addr, counter) = arena.try_insert_from(value)?;

        Ok(Root::new(addr, ty, counter))
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
    pub fn get<T, A>(&self, ptr: GcPtr<T, A>) -> Option<&T>
    where
        T: Allocated<Self> + ?Sized,
        A: RefAccess,
    {
        use crate::index::sealed_allocated::{Addr, Sealed, Ty};

        <T as Sealed<Self>>::get_ref(self, Ty(ptr.ty()), Addr(ptr.addr()))
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
    pub fn get_mut<T, A>(&mut self, ptr: GcPtr<T, A>) -> Option<&mut T>
    where
        T: Allocated<Self> + ?Sized,
        A: MutAccess,
    {
        use crate::index::sealed_allocated::{Addr, Sealed, Ty};

        <T as Sealed<Self>>::get_mut(self, Ty(ptr.ty()), Addr(ptr.addr()))
    }

    /// Get `&T` out of strong reference such as [`RootCell`] or [`Root`].
    ///
    /// Method returns reference directly
    /// because strong references prevent object from being deallocated.
    ///
    /// # Panic
    ///
    /// This function panics if `ptr` is created from a different heap
    /// but otherwise it never will.
    pub fn get_root<T, A>(&self, ptr: &RootPtr<T, A>) -> &T
    where
        T: Allocated<Self> + ?Sized,
        A: RefAccess,
    {
        assert!(
            self.contains(ptr),
            "attempt to dereference a pointer created from a different heap"
        );

        self.get(ptr.downgrade())
            .expect("rooted object should never be deallocated")
    }

    /// Get `&mut T` out of strong reference such as [`RootCell`].
    ///
    /// Method returns reference directly
    /// because strong references prevent object from being deallocated.
    ///
    /// # Panic
    ///
    /// This function panics if `ptr` is created from a different heap
    /// but otherwise it never will.
    pub fn get_root_mut<T, A>(&mut self, ptr: &RootPtr<T, A>) -> &mut T
    where
        T: Allocated<Self> + ?Sized,
        A: MutAccess,
    {
        assert!(
            self.contains(ptr),
            "attempt to dereference a pointer created from a different heap"
        );

        self.get_mut(ptr.downgrade())
            .expect("rooted object should never be deallocated")
    }

    pub fn contains<T, A>(&self, ptr: &RootPtr<T, A>) -> bool
    where
        T: ?Sized,
        A: Access,
    {
        matches!(self.arenas.get(ptr.ty()), Some(arena) if arena.validate(ptr.counter()))
    }

    /// Upgrade weak reference ([`Gc`]/[`GcCell`]) into corresponding strong reference ([`Root`]/[`RootCell`]).
    ///
    /// The function will return `None` if object was since deallocated.
    pub fn upgrade<T, A>(&self, ptr: GcPtr<T, A>) -> Option<RootPtr<T, A>>
    where
        T: ?Sized,
        A: Access,
    {
        let arena = self.arenas.get(ptr.ty())?;
        let counter = arena.upgrade(ptr.addr())?;
        let ptr = ptr.upgrade_with(counter);

        Some(ptr)
    }

    /// Downcast wrapped type of weak reference to a concrete type.
    ///
    /// # Panics
    ///
    /// This function will never panic.
    pub fn downcast<T, U, A>(&self, ptr: GcPtr<U, A>) -> Option<GcPtr<T, A>>
    where
        M: 'static,
        T: 'static,
        U: ?Sized,
        A: Access,
    {
        use userdata_store::UserdataStore;

        let arena = self.arena(ptr.ty())?;
        let arena: &UserdataStore<T, P, M> = arena.downcast_ref()?;

        let ptr = arena.get(ptr.addr())?.value.downgrade();
        Some(ptr)
    }

    /// Downcast wrapped type of strong reference to a concrete type.
    ///
    /// # Panics
    ///
    /// This function will panic if provided pointer was allocated from another heap.
    pub fn downcast_root<T, U, A>(&self, ptr: &RootPtr<U, A>) -> Option<RootPtr<T, A>>
    where
        M: 'static,
        T: 'static,
        U: ?Sized,
        A: Access,
    {
        assert!(
            self.contains(ptr),
            "attempt to downcast a pointer created from a different heap"
        );

        let ptr = self.downcast(ptr.downgrade())?;
        let ptr = self.upgrade(ptr).unwrap();
        Some(ptr)
    }

    /// Set dispatcher method for the type.
    ///
    /// By design all values of the same type share dispacher function.
    /// This change will affect all future and existing userdata of this type.
    ///
    /// Currently, for technical reasons, dispatchers are required to be function pointers.
    pub fn set_dispatcher_of<T>(&mut self, dispatcher: Dispatcher<T, P>)
    where
        T: Trace,
        M: Trace,
    {
        use arena::Getters;
        use userdata_store::{UserdataObject, UserdataStore};

        let (_, arena) = self
            .arena_or_insert_with::<UserdataObject<T, P, M>, _>(|| UserdataStore::<T, P, M>::new());
        arena.set_dispatcher(&dispatcher);
    }

    /// Query default metatable for the type.
    ///
    /// Default metatable can be set using [`Heap::set_metatable_of`] for the type.
    /// This metatable will be used when value of type `T` is allocated as `dyn FullUserdata` in [`Heap::alloc_as`] method.
    /// If you want to control what metatable is set on creation consider using [`Heap::alloc_full_userdata`] instead.
    pub fn metatable_of<T>(&self) -> Option<&M>
    where
        T: 'static,
    {
        let id = TypeId::of::<T>();
        self.metatables.get(&id)
    }

    /// Set default metatable for the type.
    ///
    /// Default metatable will be used when value of type `T` is allocated as `dyn FullUserdata` in [`Heap::alloc_as`] method.
    /// If you want to control what metatable is set on creation consider using [`Heap::alloc_full_userdata`] instead.
    ///
    /// # Return
    ///
    /// A previously set metatable will be returned if one was set.
    pub fn set_metatable_of<T>(&mut self, mt: Option<M>) -> Option<M>
    where
        T: 'static,
    {
        let id = TypeId::of::<T>();

        match mt {
            Some(mt) => match self.metatables.get_mut(&id) {
                Some(place) => Some(std::mem::replace(place, mt)),
                None => {
                    self.metatables.insert(id, mt);
                    None
                }
            },
            None => self.metatables.remove(&id),
        }
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
        self.gc_with(&(), true)
    }

    fn gc_with(&mut self, keep_alive: &dyn Trace, forced: bool) {
        match &mut self.status {
            Status::Stopped => return,
            Status::Paused(pending) => {
                *pending |= match forced {
                    true => Pending::Forced,
                    false => Pending::Auto,
                };
                return;
            }
            Status::Running => (),
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

    /// Whether heap should trigger automatic garbage collection.
    ///
    /// When disabled only direct calls to [`Heap::gc`] will trigger garbage collection.
    ///
    /// This function have no effect while gc is paused (e.g. inside closure passed to [`pause`](Heap::pause)).
    pub fn enable_auto_gc(&mut self, enable: bool) {
        if matches!(self.status, Status::Paused(_)) {
            return;
        }

        self.status = match enable {
            true => Status::Running,
            false => Status::Stopped,
        };
    }

    /// Whether automatic garbage collection is enabled.
    ///
    /// This function will return false if gc is paused (e.g. inside closure, passed to [`pause`](Heap::pause)).
    pub fn is_auto_gc_enabled(&self) -> bool {
        matches!(self.status, Status::Running)
    }

    /// Execute closure with paused garbage collection.
    ///
    /// If the garbage collection is triggered during execution of the closure,
    /// gc pass will be delayed and executed only after closure exits.
    /// This function respects auto gc setting (set by [`enable_auto_gc`](Heap::enable_auto_gc)),
    /// so no garbage collection will happen if it is explicitly disabled.
    ///
    /// Calls to `pause` can be safely nested.
    ///
    /// Note that `enable_auto_gc` has no effect inside provided closure.
    pub fn pause<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let prev = std::mem::replace(&mut self.status, Status::Paused(Pending::None));

        let r = f(self);

        // Should be always reachable, but technically user can mem::swap from inside closure.
        // If that happens simply ignore, not our problem.
        if let Status::Paused(pending) = self.status {
            match (prev, pending) {
                (Status::Running, Pending::Forced | Pending::Auto)
                | (Status::Stopped, Pending::Forced) => {
                    self.status = prev;
                    self.gc();
                }
                (Status::Paused(prev), pending) => {
                    self.status = Status::Paused(prev | pending);
                }
                (Status::Stopped | Status::Running, _) => {
                    self.status = prev;
                }
            }
        }

        r
    }

    pub fn health_check(&self) -> HeapInfo {
        let mut occupied_bytes = 0;
        let mut reserved_bytes = 0;
        let mut dead_bytes = 0;

        for arena in self.arenas.iter() {
            let info = arena.health_check();
            let size = info.object_layout.pad_to_align().size();

            occupied_bytes += size * info.occupied;
            reserved_bytes += size * info.reserved;
            dead_bytes += size * info.dead;
        }

        HeapInfo {
            occupied_bytes,
            reserved_bytes,
            dead_bytes,
        }
    }
}

impl<M, P> Default for Heap<M, P> {
    fn default() -> Self {
        Self {
            type_map: Default::default(),
            arenas: Default::default(),
            metatables: Default::default(),
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
    T: Allocated<Self> + ?Sized,
    P: Params,
{
    type Output = T;

    fn index(&self, index: &RootCell<T>) -> &Self::Output {
        self.get_root(index)
    }
}

impl<T, M, P> Index<&Root<T>> for Heap<M, P>
where
    T: Allocated<Self> + ?Sized,
    P: Params,
{
    type Output = T;

    fn index(&self, index: &Root<T>) -> &Self::Output {
        self.get_root(index)
    }
}

impl<T, M, P> IndexMut<&RootCell<T>> for Heap<M, P>
where
    T: Allocated<Self> + ?Sized,
    P: Params,
{
    fn index_mut(&mut self, index: &RootCell<T>) -> &mut Self::Output {
        self.get_root_mut(index)
    }
}

#[derive(Debug, Clone, Copy)]
enum Pending {
    None,
    Auto,
    Forced,
}

impl std::ops::BitOr for Pending {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        use Pending::*;

        match (self, rhs) {
            (Forced, _) | (_, Forced) => Forced,
            (Auto, _) | (_, Auto) => Auto,
            _ => None,
        }
    }
}

impl std::ops::BitOrAssign for Pending {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

#[derive(Debug, Clone, Copy, Default)]
enum Status {
    #[default]
    Running,
    /// Auto gc is temporarily paused.
    ///
    /// Any garbage collection is delayed until paused status is lifted.
    Paused(Pending),
    Stopped,
}

/// Intermediary keeping track of visited objects during [`Trace`]ing.
pub struct Collector {
    masks: TiVec<TypeIndex, BitVec>,
}

impl Collector {
    /// Mark an object as reachable.
    pub fn mark<T, A>(&mut self, ptr: GcPtr<T, A>)
    where
        T: ?Sized,
        A: Access,
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
pub(crate) struct TypeIndex(u16);

impl From<usize> for TypeIndex {
    fn from(value: usize) -> Self {
        let value = value
            .try_into()
            .expect("reached limit of `u16::MAX` allocated types");

        TypeIndex(value)
    }
}

impl From<TypeIndex> for usize {
    fn from(value: TypeIndex) -> Self {
        value.0.into()
    }
}

impl Display for TypeIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Statistics on memory used by heap.
#[derive(Debug, Clone, Copy)]
pub struct HeapInfo {
    /// Total amount of bytes used by all alive objects.
    pub occupied_bytes: usize,

    /// Total amount of bytes reserved for future objects.
    pub reserved_bytes: usize,

    /// Total amount of bytes taken by dead memory slots.
    ///
    /// Dead slots exhausted their generation tags and cannot get allocated into ever again.
    pub dead_bytes: usize,
}
