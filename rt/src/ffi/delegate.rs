//! Traits defining coroutines compatible with runtime's execution model.
//!
//! Rust-backed frames are stored by runtime as objects implementing [`Delegate`] trait.
//!
//! # Constructing
//!
//! By itself [`Delegate`] only constrains [`Coroutine`] trait (which represents generic coroutines).
//! `Delegate` provides blanket impl for compatible coroutines,
//! as such you should implement `Coroutine` trait directly.
//!
//! Implementing state machines by hand is not very fun,
//! so this module also provides few convenience methods to handle common cases.
//!
//! ## Simple callbacks
//!
//! Rust callbacks that don't need to communicate with runtime can be easily constructed from a single callback
//!
//! * [`from_mut`] - converts `FnMut`/`Fn` closure into coroutine.
//!
//!     For simplicity it doesn't keep track whether coroutine was completed or not.
//!     Repeated calls to `resume` will use the callback again.
//!
//! * [`from_once`] - converts `FnOnce` closure into coroutine.
//!
//!     Internally closure is wrapped in `Option`, repeated calls will result in error.
//!
//! Choice is merely a convenience: runtime will never resume completed coroutines.
//!
//! ## Hand-written state machines
//!
//! The following functions allow to avoid writing verbose `Coroutine` impl:
//!
//! * [`repeat`] - calls `FnMut(_) -> State<_, _>` closure repeatedly on calls to `resume`.
//!
//! * [`try_repeat`] - calls `FnMut(_) -> Result<State<_, ()> E>` closure repeatedly on calls to `resume`.
//!
//!     *Try* version permits usage of `?` operator and handles errors with *significatly* less pain.
//!     The output is rearranged to `State<_, Result<(), E>>` which `Delegate::resume` expects.
//!
//! * [`yield_1`] - composes delegate out of two `FnOnce` closures, where the first one can perform a request and second can process result.
//!
//!     Suitable for functions that want to invoke some kind of callback once and clean up afterward.
//!     (Un?)surprisingly common scenario among Lua `std` functions.
//!
//! A downside of returning `State` enum that error propagation operator `?` doesn't work anymore.
//! To assist with this issue there is [`try_complete`] function that neatly rewraps
//! `Result<State<_, ()>, E>` into `State<_, Result<(), E>>`.
//!
//! ## Converting `Future`
//!
//! In the future (hehe) we may also provide a way to transform futures into coroutines.
//!
//! # Evaluating
//!
//! ## Lua function ABI
//!
//! <div class="warning">
//!
//! More complete description (relevant for Lua bytecode generation) is covered by [another section](repr::opcode#lua-function-abi).
//! This part contains excerpts relevant specifically to Rust delegates.
//!
//! </div>
//!
//! Every function upon being entered gets its own stack space isolated from other functions.
//! Addressing always starts with `StackSlot(0)`.
//!
//! Delegates can store their state on stack if desired,
//! but the primary application is to communicate with other functions:
//!
//! * Any values that function finds on the stack upon starting execution are to be considered arguments.
//!
//!     You **should not** rely for stack to be in any specific state!
//!     There is no type system to have your back.
//!
//! * Any values left after the execution are to be considered return values.
//!    
//!     Unlike Rust, Lua functions (and by extension delegates) have variadic returns.
//!     It is ok to return multiple values at once or even a different count of values on different code paths.
//!
//! When invoking other functions delegate must specify portion of the stack that is passed to callee as arguments.
//! Values starting from specified `StackSlot` will be lost (passed into other function),
//! and any returns will be placed into the space instead.
//! Values below that portion will be protected by runtime and cannot be accessed by anyone except current delegate.
//!
//! *By convention* Lua functions discover their arguments on the stack in the same order as they are declared in the source
//! (first arg in `StackSlot(0)`, second in `StackSlot(1)`, etc.).
//!
//! You don't need to worry about providing correct argument count to Lua functions.
//! Lua frames get special treatment from runtime and automatically adjust their stack to desired size.
//! However, no such adjustment is done for delegates: Rust functions are expected to manage stack state on their own.
//!
//! ## Runtime communication protocol
//!
//! Coroutine cannot directly pass control to another frame (whether implemented in Lua or Rust)
//! from within `resume`, e.g. invoke other function.
//! Instead it has to communicate intent to runtime driving the coroutine.
//! This is done through exchange of [`Request`] and [`Response`] structs.
//!
//! Requests are passed when coroutine yields (returns [`State::Yielded`] variant),
//! responses are received as argument to [`Coroutine::resume`].
//!
//! There is only 1 request supported:
//!
//! * [`Request::Invoke`] calls a function on current Lua thread.
//!
//! There are two possible responses:
//!
//! * [`Response::Resume`] indicates that *outer environment* prompted coroutine to continue execution.
//! * [`Response::Returned`] provides final status of evaluating another function.
//!    
//!    Note that according to [Lua ABI](#lua-function-abi) the actual return values live on the stack!
//!    Value provided inside the variant indicates success/failure of evaluation.
//!
//!    Receiving an error means that runtime is currently in the process of [unwinding the call stack](#error-handling).
//!
//! It is guaranteed that every response is correlated to previously made request:
//!
//! | `Request::`              | `Response::` |
//! |--------------------------|--------------|
//! | Initial call to `resume` | `Resume`     |
//! | `Invoke`                 | `Returned`   |
//!
//! When hand-writing coroutine state, improper responses can either be marked [`unreachable!`] or converted to runtime error.
//!
//! ## Error handling
//!
//! Evaluating other functions may result in a **runtime error**, also referred to as **Lua panic**.
//!
//! When that happens runtime will unwind the call stack and give opportunity to every frame to handle the error.
//! By default all Lua-backed frames will simply propagate the error.
//! However Rust-backed frames will receive the error object through `Response::Returned(Err(_))` and can decide what to do with it.
//!
//! At this point runtime already removed all call frames above the coroutine
//! and cleaned stack space (starting from the part passed as arguments to the callee),
//! so that delegate can continue execution.
//! You can set message handler (link - TODO) in order to collect more information about the error before unwinding.
//!
//! Returning `State::Completed(Err(_))` will propagate Lua panic with (the newly provided) error object.
//! Any other return indicates to runtime that the error was handled.
//! From this point execution will continue as normal.
//!
//! Note that the only way to propagate the error is to complete coroutine execution.
//! This choice is by design:
//! resuming execution of panicked Lua-backed frames is never semantically sound (it is always considered Lua UB),
//! so the same is not permitted for Rust-backed frames as well.
//! Runtime will never resume completed coroutines so this is your last chance to perform cleanup, etc.
//!
//! ## Rust panic safety
//!
//! Runtime does not protect calls to `Delegate::resume`!
//! It means that any panic escaping from coroutine will interrupt execution and bubble to host program.
//! It is *very* likely to leave internals in inconsistent state.
//! Any attempt to resume evaluation on such runtime is well-behaved from perspective of Rust
//! but is considered Lua UB - runtime cannot uphold semantics of Lua program.
//!
//! In case that presents an issue and some of your delegates may still panic during normal execution,
//! consider wrapping relevant bits of `resume` implementation in [`catch_unwind`](std::panic::catch_unwind).

use std::pin::Pin;

use repr::index::StackSlot;

use crate::error::RtError;
use crate::runtime::RuntimeView;
use crate::value::{Callable, CoreTypes, Strong};

use super::coroutine::{Coroutine, State};

/// Request runtime to perform specific action.
pub enum Request<Ty>
where
    Ty: CoreTypes,
{
    Invoke {
        callable: Callable<Strong, Ty>,
        start: StackSlot,
    },
}

/// Result of performing previously requested action.
pub enum Response<Ty>
where
    Ty: CoreTypes,
{
    Resume,
    Returned(Result<(), RtError<Ty>>),
}

/// Coroutine capable of being executed by Lua runtime.
///
/// This trait has blanket impl for all compatible coroutines.
/// You need to implement [`Coroutine`] trait instead.
///
/// See [module-level](super::delegate) documentation for details.
pub trait Delegate<Ty>:
    for<'a> Coroutine<RuntimeView<'a, Ty>, Yielded = Request<Ty>, Complete = Result<(), RtError<Ty>>>
where
    Ty: CoreTypes,
{
}

impl<Ty, T> Delegate<Ty> for T
where
    Ty: CoreTypes,
    T: for<'a> Coroutine<
        RuntimeView<'a, Ty>,
        Yielded = Request<Ty>,
        Complete = Result<(), RtError<Ty>>,
    >,
{
}

/// Surrogate `!` type, implementing `Delegate`.
///
/// Values of this type cannot be constructed
/// which ensures that this coroutine cannot be entered or resumed even by accident.
pub enum Never {}

impl<'a, Ty> Coroutine<RuntimeView<'a, Ty>> for Never
where
    Ty: CoreTypes,
{
    type Yielded = Request<Ty>;
    type Complete = Result<(), RtError<Ty>>;

    fn resume(
        self: Pin<&mut Self>,
        _args: RuntimeView<'a, Ty>,
    ) -> State<Self::Yielded, Self::Complete> {
        match *Pin::into_inner(self) {}
    }
}

/// No-op function for compile-time testing `Delegate` impls.
fn assert_impl<Ty: CoreTypes, T: Delegate<Ty>>(_: &T) {}

/// Construct non-reenterable delegate out of `FnMut`/`Fn` closure.
///
/// For implementation simplicity resulting coroutine doesn't track whether it was completed or not.
/// Repeated calls to [`Coroutine::resume`] will invoke the callback.
pub fn from_mut<F, Ty>(f: F) -> FromMut<F>
where
    Ty: CoreTypes,
    F: for<'a> FnMut(RuntimeView<'a, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    let r = FromMut(f);
    assert_impl(&r);
    r
}

pub struct FromMut<F>(F);

impl<'a, F, Ty> Coroutine<RuntimeView<'a, Ty>> for FromMut<F>
where
    Ty: CoreTypes,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    type Yielded = Request<Ty>;
    type Complete = Result<(), RtError<Ty>>;

    fn resume(
        self: Pin<&mut Self>,
        args: RuntimeView<'a, Ty>,
    ) -> State<Self::Yielded, Self::Complete> {
        let func = &mut Pin::into_inner(self).0;
        let r = (func)(args);
        State::Complete(r)
    }
}

/// Construct non-reenterable delegate out of `FnOnce` closure.
///
/// Internally callback is wrapped in `Option`.
/// Repeated calls to [`Coroutine::resume`] will result in error.
pub fn from_once<F, Ty>(f: F) -> FromOnce<F>
where
    Ty: CoreTypes,
    F: for<'a> FnOnce(RuntimeView<'a, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    let r = FromOnce(Some(f));
    assert_impl(&r);
    r
}

pub struct FromOnce<F>(Option<F>);

impl<'a, F, Ty> Coroutine<RuntimeView<'a, Ty>> for FromOnce<F>
where
    Ty: CoreTypes,
    F: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    type Yielded = Request<Ty>;
    type Complete = Result<(), RtError<Ty>>;

    fn resume(
        self: Pin<&mut Self>,
        args: RuntimeView<'a, Ty>,
    ) -> State<Self::Yielded, Self::Complete> {
        use crate::error::AlreadyDroppedError;

        let func = Pin::into_inner(self).0.take();
        let r = if let Some(func) = func {
            (func)(args)
        } else {
            Err(RtError::AlreadyDropped(AlreadyDroppedError))
        };
        State::Complete(r)
    }
}

/// Construct reenterable delegate out of `FnMut(_) -> State<_, _>` closure.
pub fn repeat<F, Ty>(f: F) -> Repeat<F>
where
    Ty: CoreTypes,
    F: for<'a> FnMut(RuntimeView<'a, Ty>) -> State<Request<Ty>, Result<(), RtError<Ty>>> + Unpin,
{
    let r = Repeat(f);
    assert_impl(&r);
    r
}

pub struct Repeat<F>(F);

impl<'a, Ty, F> Coroutine<RuntimeView<'a, Ty>> for Repeat<F>
where
    Ty: CoreTypes,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> State<Request<Ty>, Result<(), RtError<Ty>>> + Unpin,
{
    type Yielded = Request<Ty>;
    type Complete = Result<(), RtError<Ty>>;

    fn resume(
        self: Pin<&mut Self>,
        args: RuntimeView<'a, Ty>,
    ) -> State<Self::Yielded, Self::Complete> {
        (Pin::into_inner(self).0)(args)
    }
}

/// Construct reenterable delegate out of fallible `FnMut(_) -> Result<State<_, ()>, Err>` closure.
///
/// Unlike [`repeat`] accepted callback returns `Result`,
/// so that you can use your favorite `?` to short-circuit.
/// The output will be rearranged to be compatible with [`Delegate`].
pub fn try_repeat<F, Ty>(f: F) -> TryRepeat<F>
where
    Ty: CoreTypes,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> Result<State<Request<Ty>, ()>, RtError<Ty>> + Unpin,
{
    let r = TryRepeat(f);
    assert_impl(&r);
    r
}

pub struct TryRepeat<F>(F);

impl<'a, Ty, F> Coroutine<RuntimeView<'a, Ty>> for TryRepeat<F>
where
    Ty: CoreTypes,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> Result<State<Request<Ty>, ()>, RtError<Ty>> + Unpin,
{
    type Yielded = Request<Ty>;
    type Complete = Result<(), RtError<Ty>>;

    fn resume(
        self: Pin<&mut Self>,
        args: RuntimeView<'a, Ty>,
    ) -> State<Self::Yielded, Self::Complete> {
        match (Pin::into_inner(self).0)(args) {
            Ok(State::Yielded(t)) => State::Yielded(t),
            Ok(State::Complete(t)) => State::Complete(Ok(t)),
            Err(t) => State::Complete(Err(t)),
        }
    }
}

/// Construct delegate that yields at most once.
///
/// This coroutine is composed of 2 callbacks: `f0` and `f1`.
/// 1. `f0` will be called initially and can perform a request to runtime.
/// 2. `f1` will be executed when corutine is resumed after successfully completing request.
/// 3. After, coroutine will be marked as completed.
///
/// Any received errors will be propagated, completing the coroutine.
pub fn yield_1<F0, F1, Ty>(f0: F0, f1: F1) -> Yield1<F0, F1>
where
    Ty: CoreTypes,
    F0: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<Request<Ty>, RtError<Ty>> + Unpin,
    F1: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    Yield1(Yield1State::Point0(f0, f1))
}

enum Yield1State<F0, F1> {
    Point0(F0, F1),
    Point1(F1),
    Completed,
}

pub struct Yield1<F0, F1>(Yield1State<F0, F1>);

impl<'a, Ty, F0, F1> Coroutine<RuntimeView<'a, Ty>> for Yield1<F0, F1>
where
    Ty: CoreTypes,
    F0: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<Request<Ty>, RtError<Ty>> + Unpin,
    F1: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    type Yielded = Request<Ty>;
    type Complete = Result<(), RtError<Ty>>;

    fn resume(
        mut self: Pin<&mut Self>,
        args: RuntimeView<'a, Ty>,
    ) -> State<Self::Yielded, Self::Complete> {
        use crate::error::AlreadyDroppedError;

        let state = std::mem::replace(&mut self.0, Yield1State::Completed);

        match state {
            Yield1State::Point0(f0, f1) => match (f0)(args) {
                Ok(request) => {
                    self.0 = Yield1State::Point1(f1);
                    State::Yielded(request)
                }
                Err(err) => State::Complete(Err(err)),
            },
            Yield1State::Point1(f1) => {
                self.0 = Yield1State::Completed;
                State::Complete((f1)(args))
            }
            Yield1State::Completed => State::Complete(Err(AlreadyDroppedError.into())),
        }
    }
}

/// Rearrange output of fallible closure.
pub fn try_complete<Y, T, E>(
    mut f: impl FnMut() -> Result<State<Y, T>, E>,
) -> impl FnMut() -> State<Y, Result<T, E>> {
    move || match f() {
        Ok(State::Yielded(t)) => State::Yielded(t),
        Ok(State::Complete(t)) => State::Complete(Ok(t)),
        Err(t) => State::Complete(Err(t)),
    }
}
