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

use std::path::Path;
use std::pin::Pin;

use gc::Root;
use repr::index::StackSlot;

use crate::backtrace::Location;
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::{Diagnostic, RtError, RuntimeError};
use crate::runtime::thread::StackGuard;
use crate::runtime::{Closure, Core, FunctionPtr, ThreadId};
use crate::value::{Callable, Strong, Types, WeakValue};

use super::coroutine::State;

pub struct RuntimeView<'rt, Ty>
where
    Ty: Types,
{
    pub core: &'rt mut Core<Ty>,
    pub chunk_cache: &'rt mut dyn ChunkCache,
    pub stack: StackGuard<'rt, Ty>,
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: Types,
{
    pub fn reborrow(&mut self) -> RuntimeView<'_, Ty> {
        let RuntimeView {
            core,
            chunk_cache,
            stack,
        } = self;

        RuntimeView {
            core: *core,
            chunk_cache: *chunk_cache,
            stack: stack.reborrow(),
        }
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: Types,
{
    pub fn construct_closure(
        &mut self,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = WeakValue<Ty>>,
    ) -> Result<Root<Closure<Ty>>, RtError<Ty>> {
        Closure::new(self, fn_ptr, upvalues).map_err(Into::into)
    }

    pub fn chunk_cache(&self) -> &dyn ChunkCache {
        self.chunk_cache
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: Types,
{
    pub fn load(
        &mut self,
        source: String,
        location: Option<Location>,
    ) -> Result<ChunkId, LoadError> {
        use codespan_reporting::files::SimpleFile;
        use logos::Logos;
        use parser::lex::Token;

        let lexer = Token::lexer(&source);
        let chunk = match parser::parser::chunk(lexer) {
            Ok(chunk) => chunk,
            Err(err) => {
                let name = location
                    .map(|loc| loc.file.clone())
                    .unwrap_or_else(|| "<unnamed>".to_string());
                let diag = Diagnostic {
                    files: SimpleFile::new(name, source),
                    message: err.into_diagnostic(),
                };

                return Err(diag.into());
            }
        };

        let chunk_id = self.chunk_cache.insert(chunk, Some(source), location)?;

        Ok(chunk_id)
    }

    pub fn load_from_file(&mut self, path: impl AsRef<Path>) -> Result<ChunkId, RtError<Ty>>
    where
        Ty::String: From<String>,
    {
        let path = path.as_ref();

        let source = std::fs::read_to_string(path).map_err(|err| {
            self.core.alloc_error_msg(format!(
                "failed to load file {}: {err}",
                path.to_string_lossy()
            ))
        })?;
        let location = Location {
            file: path.to_string_lossy().to_string(),
            line: 0,
            column: 0,
        };

        self.load(source, Some(location)).map_err(Into::into)
    }
}

#[derive(Debug)]
pub enum LoadError {
    Immutable(crate::chunk_cache::ImmutableCacheError),
    CompilationFailure(Box<Diagnostic>),
}

impl From<crate::chunk_cache::ImmutableCacheError> for LoadError {
    fn from(value: crate::chunk_cache::ImmutableCacheError) -> Self {
        LoadError::Immutable(value)
    }
}

impl From<Diagnostic> for LoadError {
    fn from(value: Diagnostic) -> Self {
        LoadError::CompilationFailure(Box::new(value))
    }
}

impl<Value> From<LoadError> for RuntimeError<Value> {
    fn from(value: LoadError) -> Self {
        match value {
            LoadError::Immutable(err) => RuntimeError::Immutable(err),
            LoadError::CompilationFailure(diag) => RuntimeError::Diagnostic(diag),
        }
    }
}

/// Request runtime to perform specific action.
pub enum Request<Ty>
where
    Ty: Types,
{
    Invoke {
        callable: Callable<Strong, Ty>,
        start: StackSlot,
    },
    Resume {
        thread: ThreadId,
        start: StackSlot,
    },
    Yield {
        start: StackSlot,
    },
}

/// Result of performing previously requested action.
#[derive(Debug)]
pub enum Response<Ty>
where
    Ty: Types,
{
    Resume,
    Evaluated(Result<(), RtError<Ty>>),
}

/// Coroutine capable of being executed by Lua runtime.
///
/// This trait has blanket impl for all compatible coroutines.
/// You need to implement [`Coroutine`] trait instead.
///
/// See [module-level](super::delegate) documentation for details.
pub trait Delegate<Ty>
where
    Ty: Types,
{
    fn resume(
        self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>>;
}

// impl<Ty, T> Delegate<Ty> for T
// where
//     Ty: CoreTypes,
//     T: for<'a> Coroutine<
//         (RuntimeView<'a, Ty>, Response<Ty>),
//         Yielded = Request<Ty>,
//         Complete = Result<(), RtError<Ty>>,
//     >,
// {
//     fn resume(self: Pin<&mut Self>, rt: RuntimeView<'_, Ty>, response: Response<Ty>) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
//         <_ as Coroutine<_>>::resume(self, (rt, response))
//     }
// }

impl<Ty, T> Delegate<Ty> for &mut T
where
    Ty: Types,
    T: Delegate<Ty> + Unpin + ?Sized,
{
    fn resume(
        mut self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        let this: Pin<&mut T> = Pin::new(*self);
        this.resume(rt, response)
    }
}

impl<Ty, T> Delegate<Ty> for Pin<&mut T>
where
    Ty: Types,
    T: Delegate<Ty> + ?Sized,
{
    fn resume(
        mut self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        let this: Pin<&mut T> = (*self).as_mut();
        Delegate::resume(this, rt, response)
    }
}

impl<Ty, T> Delegate<Ty> for Box<T>
where
    Ty: Types,
    T: Delegate<Ty> + Unpin + ?Sized,
{
    fn resume(
        mut self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        let this: Pin<&mut T> = Pin::new((*self).as_mut());
        Delegate::resume(this, rt, response)
    }
}

impl<Ty, T> Delegate<Ty> for Pin<Box<T>>
where
    Ty: Types,
    T: Delegate<Ty> + ?Sized,
{
    fn resume(
        self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        let this: Pin<&mut T> = Pin::into_inner(self).as_mut();
        Delegate::resume(this, rt, response)
    }
}

/// Surrogate `!` type, implementing `Delegate`.
///
/// Values of this type cannot be constructed
/// which ensures that this coroutine cannot be entered or resumed even by accident.
pub enum Never {}

impl<Ty> Delegate<Ty> for Never
where
    Ty: Types,
{
    fn resume(
        self: Pin<&mut Self>,
        _rt: RuntimeView<'_, Ty>,
        _response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        match *Pin::into_inner(self) {}
    }
}

/// No-op function for compile-time testing `Delegate` impls.
fn assert_impl<Ty: Types, T: Delegate<Ty>>(_: &T) {}

/// Construct non-reenterable delegate out of `FnMut`/`Fn` closure.
///
/// For implementation simplicity resulting coroutine doesn't track whether it was completed or not.
/// Repeated calls to [`Coroutine::resume`] will invoke the callback.
pub fn from_mut<F, Ty>(f: F) -> FromMut<F>
where
    Ty: Types,
    F: for<'a> FnMut(RuntimeView<'a, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    let r = FromMut(f);
    assert_impl(&r);
    r
}

pub struct FromMut<F>(F);

impl<F, Ty> Delegate<Ty> for FromMut<F>
where
    Ty: Types,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    fn resume(
        self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        debug_assert!(matches!(response, Response::Resume));

        let func = &mut Pin::into_inner(self).0;
        let r = (func)(rt);
        State::Complete(r)
    }
}

/// Construct non-reenterable delegate out of `FnOnce` closure.
///
/// Internally callback is wrapped in `Option`.
/// Repeated calls to [`Coroutine::resume`] will result in error.
pub fn from_once<F, Ty>(f: F) -> FromOnce<F>
where
    Ty: Types,
    F: for<'a> FnOnce(RuntimeView<'a, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    let r = FromOnce(Some(f));
    assert_impl(&r);
    r
}

pub struct FromOnce<F>(Option<F>);

impl<F, Ty> Delegate<Ty> for FromOnce<F>
where
    Ty: Types,
    F: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    fn resume(
        self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        use crate::error::AlreadyDroppedError;

        debug_assert!(matches!(response, Response::Resume));

        let func = Pin::into_inner(self).0.take();
        let r = if let Some(func) = func {
            (func)(rt)
        } else {
            Err(RtError::AlreadyDropped(AlreadyDroppedError))
        };
        State::Complete(r)
    }
}

/// Construct reenterable delegate out of `FnMut(_) -> State<_, _>` closure.
///
/// Lua panics will be automatically propagated.
pub fn repeat<F, Ty>(f: F) -> Repeat<F>
where
    Ty: Types,
    F: for<'a> FnMut(RuntimeView<'a, Ty>) -> State<Request<Ty>, Result<(), RtError<Ty>>> + Unpin,
{
    let r = Repeat(f);
    assert_impl(&r);
    r
}

pub struct Repeat<F>(F);

impl<Ty, F> Delegate<Ty> for Repeat<F>
where
    Ty: Types,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> State<Request<Ty>, Result<(), RtError<Ty>>> + Unpin,
{
    fn resume(
        self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        match response {
            Response::Resume => (),
            Response::Evaluated(Ok(())) => (),
            Response::Evaluated(Err(err)) => return State::Complete(Err(err)),
        }

        (Pin::into_inner(self).0)(rt)
    }
}

/// Construct reenterable delegate out of fallible `FnMut(_) -> Result<State<_, ()>, Err>` closure.
///
/// Unlike [`repeat`] accepted callback returns `Result`,
/// so that you can use your favorite `?` to short-circuit.
/// The output will be rearranged to be compatible with [`Delegate`].
///
/// Lua panics will be automatically propagated.
pub fn try_repeat<F, Ty>(f: F) -> TryRepeat<F>
where
    Ty: Types,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> Result<State<Request<Ty>, ()>, RtError<Ty>> + Unpin,
{
    let r = TryRepeat(f);
    assert_impl(&r);
    r
}

pub struct TryRepeat<F>(F);

impl<Ty, F> Delegate<Ty> for TryRepeat<F>
where
    Ty: Types,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> Result<State<Request<Ty>, ()>, RtError<Ty>> + Unpin,
{
    fn resume(
        self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        match response {
            Response::Resume => (),
            Response::Evaluated(Ok(())) => (),
            Response::Evaluated(Err(err)) => return State::Complete(Err(err)),
        }

        match (Pin::into_inner(self).0)(rt) {
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
    Ty: Types,
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

impl<Ty, F0, F1> Delegate<Ty> for Yield1<F0, F1>
where
    Ty: Types,
    F0: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<Request<Ty>, RtError<Ty>> + Unpin,
    F1: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<(), RtError<Ty>> + Unpin,
{
    fn resume(
        mut self: Pin<&mut Self>,
        rt: RuntimeView<'_, Ty>,
        response: Response<Ty>,
    ) -> State<Request<Ty>, Result<(), RtError<Ty>>> {
        use crate::error::AlreadyDroppedError;

        let state = std::mem::replace(&mut self.0, Yield1State::Completed);

        match (state, response) {
            (Yield1State::Point0(f0, f1), Response::Resume) => match (f0)(rt) {
                Ok(request) => {
                    self.0 = Yield1State::Point1(f1);
                    State::Yielded(request)
                }
                Err(err) => State::Complete(Err(err)),
            },
            (Yield1State::Point0(..), Response::Evaluated(..)) => unreachable!(),
            (Yield1State::Point1(f1), response) => {
                match response {
                    Response::Resume => (),
                    Response::Evaluated(Ok(())) => (),
                    Response::Evaluated(Err(err)) => return State::Complete(Err(err)),
                }

                self.0 = Yield1State::Completed;
                State::Complete((f1)(rt))
            }
            (Yield1State::Completed, _) => State::Complete(Err(AlreadyDroppedError.into())),
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
