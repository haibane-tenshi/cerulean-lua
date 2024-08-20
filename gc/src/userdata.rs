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
//! On the other hand either projects is complex enough in its own right.
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
//! * Userdata interface is not compatible with our Lua ABI because it needs to retain access to underlying object.
//!     This isn't a problem: Lua interacts with all userdata through *metatable* methods.
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
//! Instead, you should focus on providing correct parts.
//!
//! Out of those method dispatcher is most critical.
//! As already discussed dispatcher is responsible for actually calling Rust methods on your value.
//! A dispatcher can be set using [`Heap::set_dispatcher`](crate::Heap::set_dispatcher) for each individual type.
//! By design all values of one type share dispatcher method.
//!
//! For this reason it is meaningless to implement [`Userdata`] trait on its own -
//! unless you wish to completely replace provided mechanism.

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
    fn method(&self, ident: P::Id<'_>, rt: P::Rt<'_>) -> Option<P::Res>;
}

/// Signature of method dispatcher function for type `T`.
pub type Dispatcher<T, P> =
    fn(&T, <P as Params>::Id<'_>, <P as Params>::Rt<'_>) -> Option<<P as Params>::Res>;
