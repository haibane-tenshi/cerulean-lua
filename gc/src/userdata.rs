//! Traits describing Lua userdata.
//!
//! # Design overview
//!
//! Traits presented in this module are more generic than is technically required.
//! The reason for that is to avoid coupling garbage collector with Lua runtime.
//!
//! Userdata by design should take a reference to runtime, which contains the heap,
//! which through userdata receives reference to runtime.
//! This creates circular dependency between types and the only other way to handle it
//! is to host both types in the same crate.
//!
//! This situation is unsatisfying.
//!
//! On the one hand heap has no use for runtime at all.
//! While it needs to be aware of userdata interface, provided API is entirely for external use.
//! On the other hand either project is complex enough in its own right.
//!
//! In the end I decided that it is worth to provide extra level of "generic-ness" to decouple the two.
//!
//! # Architecture
//!
//! The key problem when designing generic userdata trait is that its method invocation unmistakably takes references as arguments.
//! By design those use higher-ranked lifetimes so that we can actually construct usable trait objects.
//! But for the same reason we cannot simply erase argument types.
//! Higher-ranked lifetimes need to be exposed in the trait.
//!
//! [`Params`] trait assists us in the endeavor.
//! It is effectively a type constructor over aforementioned lifetimes.
//! The tradeoff is that everything touching [`Userdata`] now also requires an explicit `P: Params` generic.
//!
//! # Userdata design
//!
//! Every userdata object consist of three parts: *object* itself, *method dispatcher* function and *metatable*.
//!
//! * *Method dispatcher* traslates Lua invocations into actual Rust function calls.
//! * *Metatable* is required because Lua doesn't know how to interact with trait object directly
//!     (and in fact userdata interface is incompatible with our Lua calling convention,
//!     so metatable works as an intermediate in this case).
//!
//! We get different kind of userdata depending on which combination of parts is available:
//!
//! * *Light* userdata contains object and method dispatcher, but no metatable.
//! * *Full* userdata contains all three.
//!
//! Note that light userdata still requires metatable to be useable,
//! the primary distinction from full userdata is where that metatable is coming from.
//! For full userdata metatable is allocated alongside the object,
//! but for light userdata it is provided by external source,
//! typically a metatable registry inside runtime.
//!
//! In order to construct userdata object all you need is to put respective parts together.
//! This portion of the process is fully automated, [`Heap`](crate::Heap) does everything for you in its getter methods.
//! Instead, you should focus on providing correct components.
//!
//! Out of those method dispatcher is the most critical part.
//! As already discussed dispatcher is responsible for actually calling Rust methods on your type.
//! A dispatcher can be set using [`Heap::set_dispatcher_of`](crate::Heap::set_dispatcher_of) for each individual type.
//!
//! Notably **all values of one type share dispatcher function**.
//! This is by design: due to how Rust works, dispatcher function *must* get fixed the moment trait object is constructed.
//! This causes potential problems such as inconsistent behavior in case objects of the same type are constructed with different dispatchers.
//! It is especially egregious when a downstream crate tries to extend API surface of a type by implementing a new trait.
//! The situation can be solved by providing a single global dispatcher for every type - and that is exactly what is done here.
//!
//! There are two ways to provide metatable for [`FullUserdata`].
//! The simplest option is to set it in allocation method using [`Heap::alloc_full_userdata`](crate::Heap::alloc_full_userdata).
//! Otherwise you can set default metatable for type and it will be used instead
//! when you allocate full userdata through other methods.
//!
//! For this reason it is meaningless to implement [`Userdata`] trait on its own -
//! unless you wish to completely replace provided mechanism.

use crate::Root;

/// Type constructor for arguments and output of userdata dispacher.
pub trait Params: 'static {
    type Id<'id>;
    type Rt<'rt>;
    type Res;
}

/// Trait describing Lua userdata.
///
/// Userdata is a mechanism for Lua to represent and interact with foreign types.
///
/// This specific trait's purpose is to provide type-erased interface to userdata object.
/// **You should not implement this trait for your own types.**
/// See section on [userdata design](crate::userdata#userdata-design) in module-level docs for more details.
pub trait Userdata<P>
where
    P: Params,
{
    fn method(&self, ident: P::Id<'_>) -> Option<P::Res>;
}

/// Signature of method dispatcher function for type `T`.
pub type Dispatcher<T, P> = fn(Root<T>, <P as Params>::Id<'_>) -> Option<<P as Params>::Res>;

/// Trait describing types that contain its own Lua metatable.
pub trait Metatable<M> {
    /// Retrieve metatable.
    fn metatable(&self) -> Option<&M>;

    /// Set new metatable.
    ///
    /// # Return
    ///
    /// This function returns the previously set metatable.
    fn set_metatable(&mut self, mt: Option<M>) -> Option<M>;
}

/// Trait describing Lua full userdata.
///
/// Userdata is a mechanism for Lua to represent and interact with foreign types.
/// Full userdata is just userdata with attached metatable.
///
/// This specific trait's purpose is to provide type-erased interface to userdata object.
/// See section on [userdata design](crate::userdata#userdata-design) in module-level docs for more details.
pub trait FullUserdata<M, P: Params>: Userdata<P> + Metatable<M> {}

impl<T, M, P: Params> FullUserdata<M, P> for T where T: Userdata<P> + Metatable<M> {}

/// A "default" params set for use in doc tests.
///
/// This type is not part of public API and you shouldn't use it.
#[doc(hidden)]
pub struct UnitParams;

#[doc(hidden)]
impl Params for UnitParams {
    type Id<'id> = ();
    type Rt<'rt> = ();
    type Res = ();
}
