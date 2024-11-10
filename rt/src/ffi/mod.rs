//! Traits defining Lua foreign function interface (FFI) with Rust.
//!
//! While runtime is designed to evaluate Lua code primarily,
//! it also sometimes needs to call into Rust functions.
//! Conceptually, such calls are part of Lua program call stack,
//! but they are also forced to be part of host language's call stack as well.
//! This produces a behavioral rift between Lua code and Rust code invoked through FFI.
//!
//! In order to bridge semantic differences between those two modes of execution,
//! compatible Rust function is required to generate *coroutine object* that serve as its body.
//! Using coroutines means that Rust-backed frames can be suspended,
//! which results in a number of advantages:
//!
//! * Nested calls to Rust functions don't grow host's program stack
//! * Rust frames interact nicely with Lua threads
//! * Error handling is *not* a complete mess
//!
//! The downsides are... well, coroutine things:
//!
//! * `Pin` noise
//! * There is no direct support from compiler (yet)
//! * Doing common actions like calling other functions requires [communicating](delegate#runtime-communication-protocol) with runtime
//!
//! Those can be overcome, but are tricky around places.
//!
//! # Execution model
//!
//! Every Lua execution thread has a *call stack*.
//! Entries in the stack (*frames*) correspond to individual function calls.
//! Each of those can be backed either by Lua or Rust code.
//!
//! Lua-backed frames are managed internally by runtime and never exposed to the user.
//!
//! Rust-backed frames are represented via objects implementing [`Delegate`] trait.
//! Since runtime cannot interfere with execution of Rust functions
//! it function behavior is delegated to such an object.
//!
//! # `LuaFfi` trait
//!
//! Rust functions callable by Lua runtime are represented by [`LuaFfi`] trait.
//! Invoking it through [`LuaFfi::call`] should generate "body" of the function which must implement [`Delegate`] trait.
//! See [module-level documentation](delegate) for more details on delegates and their contract.
//!
//! ## Unpin optimization
//!
//! Using coroutines has a direct downside.
//! In order to get resumed, coroutine body needs to get [`Pin`]ned.
//! Behind all fancy words there is simple implication: value cannot move in memory until it is dropped.
//! For this reason it must be allocated in `Pin<Box<_>>`.
//!
//! This is a rather unfortunate consequence for coroutines that don't require `Pin`.
//! Even a simple callback that never suspends needs to pay the price.
//!
//! As a point for optimization, function can optionally provide [`LuaFfi::call_unpin`] implementation.
//! The output of this function is required to be `Unpin` so it can be freely moved in memory.
//! When calling `LuaFfi` function runtime will attempt to use unpin version first
//! which allows to avoid unnecessary allocations.
//!
//! However, **this is user's responsibility to ensure that pin and unpin versions have identical behavior**.
//! Constructors provided by this module are guaranteed to provide correct implementation,
//! but you should be careful when implementing `LuaFfi` by hand.
//! There is no way to diagnose incoherency caused by mismatched behavior.
//!
//! In case the delegate truly requires to get pinned,
//! you can set [`LuaFfi::UnpinDelegate`] to [`delegate::Never`] which ensures that it cannot be accidentally used.
//!
//! ## `dyn` safety
//!
//! [`LuaFfi`] is `dyn`-safe but can be verbose to use as it requires to specify associated types.
//!
//! [`DLuaFfi`] is provided as refinement of the trait which sets associated types to typed-erased alternatives.
//! Those types can also be used independently using [`DynDelegate`] and [`UnpinDynDelegate`].
//!
//! # Constructing
//!
//! This module provides a few methods to conveniently create [`LuaFfi`] objects out of normal Rust functions.
//!
//! * [`from_fn`] - packages `Fn` callback. Requires delegate to be `Unpin`.
//! * [`from_fn_mut`] - packages `FnMut` callback. Requires delegate to be `Unpin`.
//! * [`from_fn_pin`] - packages `Fn` callback. Allows for `!Unpin` delegates.
//! * [`from_fn_mut_pin`] - packages `FnMut` callback. Allows for `!Unpin` delegates.

pub mod arg_parser;
pub mod coroutine;
pub mod delegate;
pub mod signature;
pub mod tuple;

use std::cell::RefCell;
use std::fmt::Debug;
use std::path::Path;
use std::pin::Pin;

use gc::Trace;

use crate::chunk_cache::ChunkId;
use crate::runtime::{Closure, RuntimeView};
use crate::value::Types;

use delegate::{Delegate, Never};

/// Trait defining Rust functions invokable by Lua runtime.
pub trait LuaFfi<Ty>: Trace
where
    Ty: Types,
{
    type Delegate: Delegate<Ty>;
    type UnpinDelegate: Delegate<Ty> + Unpin;

    fn call(&self) -> Self::Delegate;
    fn call_unpin(&self) -> Option<Self::UnpinDelegate> {
        None
    }
    fn debug_info(&self) -> DebugInfo;
}

impl<Ty, T> LuaFfi<Ty> for Box<T>
where
    Ty: Types,
    T: LuaFfi<Ty> + ?Sized,
{
    type Delegate = <T as LuaFfi<Ty>>::Delegate;
    type UnpinDelegate = <T as LuaFfi<Ty>>::UnpinDelegate;

    fn call(&self) -> Self::Delegate {
        LuaFfi::call(self.as_ref())
    }

    fn debug_info(&self) -> DebugInfo {
        self.as_ref().debug_info()
    }
}

/// Construct `LuaFfi` object out of `Fn() -> impl (Delegate + Unpin)` function.
///
/// Delegate is required to be `Unpin`.
/// It is currently impossible to automatically identify whether type implements `Unpin` or not
/// (to correctly generate [`LuaFfi::call_unpin`]).
/// Use [`from_fn_pin`] instead if the delegate need to be pinned.
///
/// Rust closures cannot implement custom traits.
/// In case closure captures any weak references you should pass them in `trace` argument.
/// It will be used in [`Trace`] implementation in place of closure itself.
pub fn from_fn<F, S, T>(f: F, name: S, trace: T) -> FromFn<F, S, T> {
    FromFn {
        func: f,
        name,
        trace,
    }
}

pub struct FromFn<F, S, T> {
    func: F,
    name: S,
    trace: T,
}

impl<F, S, T> Trace for FromFn<F, S, T>
where
    F: 'static,
    S: 'static,
    T: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        self.trace.trace(collector);
    }
}

impl<Ty, F, R, S, T> LuaFfi<Ty> for FromFn<F, S, T>
where
    Ty: Types,
    F: Fn() -> R + 'static,
    R: Delegate<Ty> + Unpin,
    S: AsRef<str> + 'static,
    T: Trace,
{
    type Delegate = R;
    type UnpinDelegate = R;

    fn call(&self) -> Self::Delegate {
        (self.func)()
    }

    fn call_unpin(&self) -> Option<Self::UnpinDelegate> {
        Some((self.func)())
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: self.name.as_ref().to_string(),
        }
    }
}

/// Construct `LuaFfi` object out of `FnMut() -> impl (Delegate + Unpin)` function.
///
/// Delegate is required to be `Unpin`.
/// It is currently impossible to automatically identify whether type implements `Unpin` or not
/// (to correctly generate [`LuaFfi::call_unpin`]).
/// Use [`from_fn_pin`] instead if the delegate need to be pinned.
///
/// Rust closures cannot implement custom traits.
/// In case closure captures any weak references you should pass them in `trace` argument.
/// It will be used in [`Trace`] implementation in place of closure itself.
pub fn from_fn_mut<F, S, T>(f: F, name: S, trace: T) -> FromFnMut<F, S, T> {
    FromFnMut {
        func: RefCell::new(f),
        name,
        trace,
    }
}

pub struct FromFnMut<F, S, T> {
    func: RefCell<F>,
    name: S,
    trace: T,
}

impl<F, S, T> Trace for FromFnMut<F, S, T>
where
    F: 'static,
    S: 'static,
    T: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        self.trace.trace(collector);
    }
}

impl<Ty, F, R, S, T> LuaFfi<Ty> for FromFnMut<F, S, T>
where
    Ty: Types,
    F: FnMut() -> R + 'static,
    R: Delegate<Ty> + Unpin,
    S: AsRef<str> + 'static,
    T: Trace,
{
    type Delegate = R;
    type UnpinDelegate = R;

    fn call(&self) -> Self::Delegate {
        (self.func.borrow_mut())()
    }

    fn call_unpin(&self) -> Option<Self::UnpinDelegate> {
        Some((self.func.borrow_mut())())
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: self.name.as_ref().to_string(),
        }
    }
}

/// Construct `LuaFfi` object out of `Fn() -> impl Delegate` function.
///
/// It is currently impossible to automatically identify whether type implements `Unpin` or not
/// (to correctly generate [`LuaFfi::call_unpin`]).
/// Use [`from_fn`] instead if the delegate do not require to be pinned.
///
/// Rust closures cannot implement custom traits.
/// In case closure captures any weak references you should pass them in `trace` argument.
/// It will be used in [`Trace`] implementation in place of closure itself.
pub fn from_fn_pin<F, S, T>(f: F, name: S, trace: T) -> FromFnPin<F, S, T> {
    FromFnPin {
        func: f,
        name,
        trace,
    }
}

pub struct FromFnPin<F, S, T> {
    func: F,
    name: S,
    trace: T,
}

impl<F, S, T> Trace for FromFnPin<F, S, T>
where
    F: 'static,
    S: 'static,
    T: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        self.trace.trace(collector);
    }
}

impl<Ty, F, R, S, T> LuaFfi<Ty> for FromFnPin<F, S, T>
where
    Ty: Types,
    F: Fn() -> R + 'static,
    R: Delegate<Ty>,
    S: AsRef<str> + 'static,
    T: Trace,
{
    type Delegate = R;
    type UnpinDelegate = Never;

    fn call(&self) -> Self::Delegate {
        (self.func)()
    }

    fn call_unpin(&self) -> Option<Self::UnpinDelegate> {
        None
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: self.name.as_ref().to_string(),
        }
    }
}

/// Construct `LuaFfi` object out of `FnMut() -> impl Delegate` function.
///
/// It is currently impossible to automatically identify whether type implements `Unpin` or not
/// (to correctly generate [`LuaFfi::call_unpin`]).
/// Use [`from_fn`] instead if the delegate do not require to be pinned.
///
/// Rust closures cannot implement custom traits.
/// In case closure captures any weak references you should pass them in `trace` argument.
/// It will be used in [`Trace`] implementation in place of closure itself.
pub fn from_fn_mut_pin<F, S, T>(f: F, name: S, trace: T) -> FromFnMutPin<F, S, T> {
    FromFnMutPin {
        func: RefCell::new(f),
        name,
        trace,
    }
}

pub struct FromFnMutPin<F, S, T> {
    func: RefCell<F>,
    name: S,
    trace: T,
}

impl<F, S, T> Trace for FromFnMutPin<F, S, T>
where
    F: 'static,
    S: 'static,
    T: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        self.trace.trace(collector);
    }
}

impl<Ty, F, R, S, T> LuaFfi<Ty> for FromFnMutPin<F, S, T>
where
    Ty: Types,
    F: FnMut() -> R + 'static,
    R: Delegate<Ty>,
    S: AsRef<str> + 'static,
    T: Trace,
{
    type Delegate = R;
    type UnpinDelegate = Never;

    fn call(&self) -> Self::Delegate {
        (self.func.borrow_mut())()
    }

    fn call_unpin(&self) -> Option<Self::UnpinDelegate> {
        None
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: self.name.as_ref().to_string(),
        }
    }
}

/// Type-erased pinned `dyn Delegate`.
pub type DynDelegate<Ty> = Pin<Box<dyn Delegate<Ty>>>;

/// Type-erased unpinned `dyn (Delegate + Unpin)`.
pub type UnpinDynDelegate<Ty> = Box<dyn Delegate<Ty> + Unpin>;

/// Refinement of `LuaFfi` to be used as trait object (`dyn DLuaFfi`).
pub trait DLuaFfi<Ty>:
    LuaFfi<Ty, Delegate = DynDelegate<Ty>, UnpinDelegate = UnpinDynDelegate<Ty>>
where
    Ty: Types,
{
}

impl<T, Ty> DLuaFfi<Ty> for T
where
    Ty: Types,
    T: LuaFfi<
        Ty,
        Delegate = Pin<Box<dyn Delegate<Ty>>>,
        UnpinDelegate = Box<dyn Delegate<Ty> + Unpin>,
    >,
{
}

/// Convert arbitrary `LuaFfi` function to `Box<dyn DLuaFfi>`.
pub fn boxed<F, Ty>(f: F) -> Box<dyn DLuaFfi<Ty>>
where
    Ty: Types,
    F: LuaFfi<Ty> + 'static,
    <F as LuaFfi<Ty>>::Delegate: 'static,
    <F as LuaFfi<Ty>>::UnpinDelegate: 'static,
{
    let inner = BoxedLuaFfiFn { value: f };

    Box::new(inner)
}

struct BoxedLuaFfiFn<F> {
    value: F,
}

impl<F> Trace for BoxedLuaFfiFn<F>
where
    F: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        let BoxedLuaFfiFn { value } = self;

        value.trace(collector);
    }
}

impl<F, Ty> LuaFfi<Ty> for BoxedLuaFfiFn<F>
where
    Ty: Types,
    F: LuaFfi<Ty>,
    <F as LuaFfi<Ty>>::Delegate: 'static,
    <F as LuaFfi<Ty>>::UnpinDelegate: 'static,
{
    type Delegate = Pin<Box<dyn Delegate<Ty>>>;
    type UnpinDelegate = Box<dyn Delegate<Ty> + Unpin>;

    fn call(&self) -> Self::Delegate {
        Box::pin(self.value.call())
    }

    fn call_unpin(&self) -> Option<Self::UnpinDelegate> {
        Some(Box::new(self.value.call_unpin()?))
    }

    fn debug_info(&self) -> DebugInfo {
        self.value.debug_info()
    }
}

pub fn dyn_ffi<F, Ty>(f: F) -> impl DLuaFfi<Ty>
where
    Ty: Types,
    F: LuaFfi<Ty> + 'static,
    <F as LuaFfi<Ty>>::Delegate: 'static,
    <F as LuaFfi<Ty>>::UnpinDelegate: 'static,
{
    BoxedLuaFfiFn { value: f }
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct DebugInfo {
    pub name: String,
}

pub fn call_chunk<Ty>(chunk_id: ChunkId) -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
{
    let f = move || {
        use crate::gc::LuaPtr;
        use crate::runtime::FunctionPtr;
        use crate::value::Callable;
        use delegate::Request;
        use repr::index::{FunctionId, StackSlot};

        let ptr = FunctionPtr {
            chunk_id,
            function_id: FunctionId(0),
        };

        delegate::yield_1(
            move |mut rt: RuntimeView<'_, Ty>| {
                let closure = rt.construct_closure(ptr, [rt.core.global_env.downgrade()])?;
                let request = Request::Invoke {
                    callable: Callable::Lua(LuaPtr(closure)),
                    start: StackSlot(0),
                };
                Ok(request)
            },
            |_: RuntimeView<'_, Ty>| Ok(()),
        )
    };

    from_fn(f, "rt::ffi::call_chunk", ())
}

pub fn call_file<Ty>(script: impl AsRef<Path>) -> impl LuaFfi<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>, RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    let mut script = Some(script.as_ref().to_path_buf());
    let f = move || {
        let script = script.take();
        delegate::yield_1(
            move |mut rt: RuntimeView<'_, Ty>| {
                use crate::error::AlreadyDroppedError;
                use crate::gc::LuaPtr;
                use crate::value::Callable;
                use delegate::Request;
                use repr::index::StackSlot;

                let script = script.ok_or(AlreadyDroppedError)?;
                let chunk_id = rt.load_from_file(&script)?;
                let callback = rt.core.gc.alloc_cell(boxed(call_chunk(chunk_id)));
                let request = Request::Invoke {
                    callable: Callable::Rust(LuaPtr(callback)),
                    start: StackSlot(0),
                };

                Ok(request)
            },
            |_: RuntimeView<'_, Ty>| Ok(()),
        )
    };

    from_fn_mut(f, "rt::ffi::call_file", ())
}
