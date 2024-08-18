//! Implementation of function pointer with a type-erased parameter.
//!
//! The core issue we are trying to solve is sending `fn` pointers with generics across virtualization boundary.
//! There two possible situations:
//! * Generic type `A` needs to be erased.
//! * Generic type `B` is forwarded through generics on trait object.
//!
//! Normally to erase the type we rely on [`TypeId`] struct, however it forces `A: 'static` bound.
//! Which is surprisingly a problem for `fn` pointers:
//! they **don't** `'static` by default and instead require all arguments and return type to be `'static` for that to be true.
//! Issue is described on rust-lang tracker in [issue#57325][rust-lang:issue57325].
//! As noted by maintaners this is actually intended behavior as described in [RFC][rust-lang:rfc1214] that implemented it.
//! So when any involved types are not `'static` we reach for the second approach and simply forward generics,
//! which allows us to avoid the issue altogether.
//!
//! Unfortunately either approach only works when all parameters fall into one one category or another.
//! In case you have some type parameters that need to be erased and some that have to be preserved
//! molding `fn` pointer into something that works becomes painful.
//!
//! This is the problem [`VirtualFn`] is trying to solve.
//! Internally it converts `fn` pointer to `*const ()`.
//! Type of `A` is forgotten but its `TypeId` is preserved,
//! types of other parameters are untouched.
//! Later the pointer can be recovered by supplying correct type for the first parameter.
//! This part requires `unsafe` but is basically equivalent to what [`Any`](std::any::Any) does internally.
//!
//! The type exists in its own crate to isolate `unsafe`.
//!
//! [rust-lang:issue57325]: https://github.com/rust-lang/rust/issues/57325
//! [rust-lang:rfc1214]: https://rust-lang.github.io/rfcs/1214-projections-lifetimes-and-wf.html

use std::any::TypeId;
use std::fmt::Debug;
use std::marker::PhantomData;

/// Function pointer with erased first parameter.
pub struct VirtualFn<B, C, R> {
    first: TypeId,
    ptr: *const (),
    _marker: PhantomData<fn(B, C) -> R>,
}

impl<B, C, R> VirtualFn<B, C, R> {
    /// Construct new pointer. Type of `A` will be erased.
    pub fn new<A>(fn_ptr: fn(&A, B, C) -> R) -> Self
    where
        A: ?Sized + 'static,
    {
        let first = TypeId::of::<A>();
        let ptr = fn_ptr as *const ();

        VirtualFn {
            first,
            ptr,
            _marker: PhantomData,
        }
    }

    /// Recover original `fn` pointer.
    pub fn downcast<A>(self) -> Option<fn(&A, B, C) -> R>
    where
        A: ?Sized + 'static,
    {
        if self.first == TypeId::of::<A>() {
            // SAFETY:
            // * Input parameter is the same as the original one as confirmed by TypeId.
            // * Representations of both types are (almost always) identical so the process of transmuting is sound.
            // See https://dev-doc.rust-lang.org/std/primitive.fn.html#casting-to-and-from-integers
            let fn_ptr =
                unsafe { std::mem::transmute::<*const (), for<'a> fn(&'a A, B, C) -> R>(self.ptr) };
            Some(fn_ptr)
        } else {
            None
        }
    }
}

impl<B, C, R> Debug for VirtualFn<B, C, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::any::type_name;

        f.debug_struct("VirtualFn")
            .field(
                "ptr",
                &format!(
                    "fn(&dyn Any, {}, {}) -> {}",
                    type_name::<B>(),
                    type_name::<C>(),
                    type_name::<R>()
                ),
            )
            .finish_non_exhaustive()
    }
}

impl<B, C, R> Clone for VirtualFn<B, C, R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<B, C, R> Copy for VirtualFn<B, C, R> {}
