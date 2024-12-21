//! Runtime object.
//!
//! # Coherence
//!
//! One challenging aspect of emulating Lua language is that it is more permissive than Rust.
//! In Lua it is OK to have multiple aliasing mutable references - which is something that we need to carefully model.
//!
//! In order to work around this problem, in relevant places (where references must persist for prolonged time)
//! we separate them into two parts: *index* and *store*.
//! The *store* contains objects that are being borrowed and *index* is simple identifier into it.
//! Importantly, out of two *store* carries the lifetime, which allows us to persist only index and avoid borrowing issues.
//! However this approach also introduces a new problem.
//!
//! Index can represent the value only as long as the lookup actually returns that value.
//! There are two conditions for that to happen:
//!
//! * lookup should happen in the same store index is associated with
//! * the value inside store should not get replaced
//!
//! (note: too strict? last may not be *always* required)
//!
//! We call this property **coherence**.
//! Runtime relies on it in order to provide correct semantics to Lua program
//! even for something as simple as calling another function or maintaining internal caches.
//!
//! Generally speaking, breaking coherence is a logical error.
//! It will not lead to unsafety, however in its presence runtime can no longer guarantee semantics of Lua program,
//! resulting in Lua Undefined Behavior.
//!
//! ## Index-store pairs
//!
//! The following objects and indices into them are under purview of coherence:
//!
//! * `Heap` and references into it (`Gc`/`GcCell`/`Root`/`RootCell`).
//! * Chunk cache and `ChunkId`.
//! * Orchestrator and `ThreadId`.
//!
//! ## Preserving coherence
//!
//! Unfortunately, permitting external code to break coherence is incredibly easy:
//!
//! ```
//! fn process(callback: impl FnOnce(&mut Vec<u8>)) {
//!     let mut cache = vec![1, 2, 3];
//!     assert_eq!(cache[2], 3);
//!
//!     callback(&mut cache);
//!
//!     // Oops, cache might have changed without us noticing.
//!     assert_eq!(cache[2], 3);
//! }
//!
//! process(|cache| {
//!     // Malicious callback replaces the cache or some of its objects.
//!     std::mem::replace(cache, vec![0, 0]);
//! })
//! ```
//!
//! `process` may have good reasons to expose the mutable reference to callback,
//! but it also allows various `std::mem` methods to completely disrupt existing assumptions outside of its control.
//!
//! The runtime makes a reasonable effort to prevent common cases of coherence breakage:
//!
//! * Certain Lua objects are allocated behind immutable references (`Gc`/`Root`).
//!     
//!     This currently includes strings (which are interned and any changes to string content may break interning guarantees)
//!     and Lua closures (which must keep track of upvalues/captures and changing those may affect existing frames).
//!
//! * It is not possible to directly access backing store of thread stack neither from host program nor delegates.
//!     
//!     This is because for a suspended frame stack is part of its preserved state and should be protected from external code.
//!     During execution delegates get access to their allotted stack space via `StackGuard` object.
//!
//! * Chunk cache does not permit removal or modification of managed chunks.
//!
//! * Existing thread objects cannot be directly accessed.
//!
//!     It is possible to observe certain properties of a thread through `ThreadGuard` object.
//!
//! * Orchestrator (thread supervisor) cannot be directly accessed.
//!
//! ## User-driven coherence breakage
//!
//! You should note that the list in previous section is not waterproof.
//! There exist ways for user to break coherence through public APIs.
//! Patching those was considered either impossible or too disruptive.
//! Here's a (not comprehensive) list of known loopholes:
//!
//! * You can replace heap object.
//!
//!     There are two argument to it.
//!     
//!     First, protecting heap is very disruptive.
//!     Any gc-reference lookup requires heap access, modifying objects inside heap requires mutable heap access.
//!     As such, heap references are incredibly pervasive throughout entire runtime.
//!
//!     Second, replacing heap is unlikely to end well regardless.
//!     It is nigh impossible to accidentally (or intentionally) replicate internal heap state with the purpose of forging/replacing certain values.
//!     As a result most weak reference will stop pointing to existing values and error-out on lookup.
//!     Strong references can detect whether the lookup is performed in the heap it were created from and will panic on error.
//!
//!     In other words, replacing heap is almost surefire way to cause runtime to panic and/or error-out.
//!
//! * You can replace chunk cache.
//!
//!     Currently runtime does not guard access to chunk cache from host program.
//!     This may change in the future.
//!
//!     However it is not possible to replace chunk cache from inside delegates:
//!     those observe it as trait object (`&dyn ChunkCache`) and `std::mem` APIs cannot be applied to unsized objects.
//!
//! * You can pass weak reference created in another heap into runtime.
//!
//!     The situation is very similar to heap replacement.
//!     Those external reference will most likely error out on lookup,
//!     or in a rare case will be silently interpreted as pointing to a different object.
//!

mod closure;
mod orchestrator;
pub mod thread;

use std::fmt::Debug;

use enumoid::EnumMap;
use gc::{GcCell, Interned, Root, RootCell};
use repr::literal::Literal;

use crate::builtins::coerce::CustomPolicy;
use crate::chunk_cache::ChunkCache;
use crate::error::{AlreadyDroppedError, RtError, ThreadError};
use crate::ffi::DLuaFfi;
use crate::gc::Heap;
use crate::value::{
    Callable, KeyValue, Meta, SolitaryType, Strong, StrongValue, Types, Value, Weak, WeakValue,
};
use orchestrator::Orchestrator;
use thread::frame::Event;
use thread::ThreadGuard;

pub use closure::{Closure, FunctionPtr};
pub use orchestrator::ThreadId;

pub struct MetatableRegistry<T> {
    values: EnumMap<SolitaryType, Option<RootCell<T>>>,
}

impl<T> MetatableRegistry<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get(&self, type_: SolitaryType) -> Option<&RootCell<T>> {
        self.values.get(type_).as_ref()
    }

    pub fn get_gc(&self, type_: SolitaryType) -> Option<GcCell<T>> {
        self.get(type_).map(|ptr| ptr.downgrade())
    }

    pub fn set(&mut self, type_: SolitaryType, value: RootCell<T>) -> Option<RootCell<T>> {
        self.values.get_mut(type_).replace(value)
    }
}

impl<T> Default for MetatableRegistry<T> {
    fn default() -> Self {
        Self {
            values: Default::default(),
        }
    }
}

impl<T> Debug for MetatableRegistry<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MetatableRegistry")
            .field("values", &self.values)
            .finish()
    }
}

pub struct Core<Ty>
where
    Ty: Types,
{
    pub global_env: StrongValue<Ty>,
    pub metatable_registry: MetatableRegistry<Ty::Table>,
    pub dialect: CustomPolicy,
    pub gc: Heap<Ty>,
}

impl<Ty> Core<Ty>
where
    Ty: Types,
{
    pub fn alloc_string(&mut self, s: Ty::String) -> Root<Interned<Ty::String>> {
        self.gc.intern(s)
    }

    pub fn alloc_literal(&mut self, literal: Literal) -> StrongValue<Ty> {
        match literal {
            Literal::Nil => Value::Nil,
            Literal::Bool(t) => Value::Bool(t),
            Literal::Int(t) => Value::Int(t),
            Literal::Float(t) => Value::Float(t),
            Literal::String(s) => {
                use crate::gc::LuaPtr;

                let s = self.alloc_string(s.into());
                Value::String(LuaPtr(s))
            }
        }
    }

    pub fn alloc_error_msg(&mut self, msg: impl Into<Ty::String>) -> RtError<Ty> {
        use crate::error::ValueError;
        use crate::gc::LuaPtr;

        let msg = self.alloc_string(msg.into());
        ValueError(Value::String(LuaPtr(msg))).into()
    }
}

impl<Ty> Core<Ty>
where
    Ty: Types,
{
    pub fn metatable_of(
        &self,
        value: &WeakValue<Ty>,
    ) -> Result<Option<Meta<Ty>>, AlreadyDroppedError> {
        let Core {
            metatable_registry,
            gc,
            ..
        } = self;

        value.metatable(gc, metatable_registry)
    }
}

impl<Ty> Debug for Core<Ty>
where
    Ty: Types,
    StrongValue<Ty>: Debug,
    Ty::Table: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Core")
            .field("global_env", &self.global_env)
            .field("metatable_registry", &self.metatable_registry)
            .field("dialect", &self.dialect)
            .field("gc", &self.gc)
            .finish()
    }
}

pub(crate) struct Cache<Ty>
where
    Ty: Types,
{
    events: EnumMap<Event, Root<Interned<Ty::String>>>,
}

impl<Ty> Cache<Ty>
where
    Ty: Types,
{
    fn new(heap: &mut Heap<Ty>) -> Self {
        let events =
            EnumMap::new_with(|event: Event| heap.intern(event.to_metamethod().to_str().into()));

        Cache { events }
    }

    fn lookup_event(&self, event: Event) -> KeyValue<Weak, Ty> {
        use crate::gc::LuaPtr;

        let ptr = self.events.get(event).downgrade();
        KeyValue::String(LuaPtr(ptr))
    }
}

pub struct Runtime<Ty, C>
where
    Ty: Types,
{
    pub core: Core<Ty>,
    pub chunk_cache: C,
    orchestrator: Orchestrator<Ty>,
    internal_cache: Cache<Ty>,
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: Types,
    C: Debug,
{
    pub fn new(chunk_cache: C, mut core: Core<Ty>) -> Self {
        tracing::trace!(?chunk_cache, "constructed runtime");

        let orchestrator = Orchestrator::new(&mut core.gc);
        let internal_cache = Cache::new(&mut core.gc);
        Runtime {
            core,
            chunk_cache,
            orchestrator,
            internal_cache,
        }
    }
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: Types,
{
    pub fn new_thread(&mut self, callable: Callable<Strong, Ty>) -> ThreadId {
        self.orchestrator.new_thread(&mut self.core.gc, callable)
    }
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: Types,
    C: ChunkCache,
{
    fn context(&mut self) -> (orchestrator::Context<Ty>, &mut Orchestrator<Ty>) {
        use orchestrator::Context;

        let Runtime {
            core,
            chunk_cache,
            orchestrator,
            internal_cache,
        } = self;

        let ctx = Context {
            core,
            internal_cache,
            chunk_cache,
        };

        (ctx, orchestrator)
    }

    pub fn thread(&self, thread_id: ThreadId) -> Option<ThreadGuard<'_, Ty>> {
        let status = self.orchestrator.status_of(thread_id)?;
        // There may not be actual thread constructed yet if it is still in `ThreadImpetus` state.
        let thread = self.orchestrator.thread(thread_id);

        let guard = ThreadGuard {
            thread_id,
            thread,
            heap: &self.core.gc,
            chunk_cache: &self.chunk_cache,
            status,
        };

        Some(guard)
    }

    /// Produce iterator over a stack of panicked threads.
    ///
    /// Threads which receive runtime error but don't handle it are called *panicked* threads.
    /// Panicked threads cannot be resumed but you can safely inspect them (e.g. by constructing backtrace)
    /// to identify the problem.
    ///
    /// However, Lua threads form a stack so when a thread panics, the error is propagated to its parent.
    /// In case the parent cannot handle it and panics too, it remembers id of the child thread that caused the issue.
    /// This way panicked threads form a singly-linked list which allows you to reconstruct the entire panicked thread stack.
    ///
    /// This method is a convenience function to assist you in doing exactly that.
    /// Resulting iterator will follow all panicked threads from parent to child starting from the one you specified.
    ///
    /// The iterator will be empty if thread with `thread_id` doesn't exist or is not panicked.
    pub fn panic_stack(&self, thread_id: ThreadId) -> impl Iterator<Item = ThreadGuard<'_, Ty>> {
        use orchestrator::ThreadStatus;

        let mut next = self.thread(thread_id).and_then(|thread| {
            if thread.status() == ThreadStatus::Panicked {
                Some(thread_id)
            } else {
                None
            }
        });

        std::iter::from_fn(move || {
            let thread = self.thread(next.take()?)?;
            debug_assert_eq!(thread.status(), ThreadStatus::Panicked);
            next = thread.panic_origin();
            Some(thread)
        })
    }
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
    C: ChunkCache,
{
    pub fn resume(&mut self, thread_id: ThreadId) -> Result<(), ThreadError> {
        let (ctx, orchestrator) = self.context();
        orchestrator.enter(ctx, thread_id)
    }
}
