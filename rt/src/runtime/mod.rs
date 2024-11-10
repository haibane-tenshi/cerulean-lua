mod closure;
mod dialect;
mod interner;
mod orchestrator;
mod thread;

use std::fmt::Debug;
use std::path::Path;

use enumoid::EnumMap;
use gc::{GcCell, Root, RootCell};
use repr::literal::Literal;

use crate::backtrace::Location;
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::diagnostic::Diagnostic;
use crate::error::{AlreadyDroppedError, RtError, RuntimeError, ThreadError};
use crate::ffi::DLuaFfi;
use crate::gc::Heap;
use crate::value::{
    Callable, KeyValue, Meta, SolitaryType, Strong, StrongValue, Types, Value, Weak, WeakValue,
};
use orchestrator::Orchestrator;
use thread::frame::Event;

pub use closure::{Closure, FunctionPtr};
pub use dialect::{CoerceArgs, DialectBuilder};
pub use interner::{Interned, Interner};
pub use orchestrator::ThreadId;
pub use thread::stack::{StackFrame, StackGuard, TransientStackFrame};
pub use thread::ThreadGuard;

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
    pub dialect: DialectBuilder,
    pub gc: Heap<Ty>,
    pub string_interner: Interner<Ty::String>,
}

impl<Ty> Core<Ty>
where
    Ty: Types,
{
    pub fn alloc_string(&mut self, s: Ty::String) -> Root<Interned<Ty::String>> {
        self.string_interner.insert(s, &mut self.gc)
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
    fn lookup_event(&self, event: Event) -> KeyValue<Weak, Ty> {
        use crate::gc::LuaPtr;

        let s = self.string_interner.event(event.to_metamethod());
        KeyValue::String(LuaPtr(s))
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
            .field("string_interner", &self.string_interner)
            .finish()
    }
}

pub struct Runtime<Ty, C>
where
    Ty: Types,
{
    pub core: Core<Ty>,
    pub chunk_cache: C,
    pub orchestrator: Orchestrator<Ty>,
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: Types,
    C: Debug,
{
    pub fn new(chunk_cache: C, mut core: Core<Ty>) -> Self {
        tracing::trace!(?chunk_cache, "constructed runtime");

        let orchestrator = Orchestrator::new(&mut core.gc);
        Runtime {
            core,
            chunk_cache,
            orchestrator,
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
        } = self;

        let ctx = Context { core, chunk_cache };

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

// impl<'rt, Ty> RuntimeView<'rt, Ty>
// where
//     Ty: CoreTypes,
// {
/// Return runtime into consistent state.
///
/// This function is useful in case you caught Lua panic
/// (one of the methods returned `RuntimeError`)
/// and you want to continue executing code inside this runtime.
/// Lua panic may interrupt execution at arbitrary point
/// potentially leaving internal structures in inconsistent state.
/// Resuming execution on such runtime results *Lua undefined behavior*.
///
/// Invoking `reset` will purge stack and bring internals into consistent state
/// making it safe to execute Lua code once again.
/// As such you should collect any useful information about error (e.g. backtrace)
/// before resetting in case you need it.
///
/// Note that the "consistency" part applies only to runtime itself but not to Lua constructs!
/// It is entirely possible for Lua to panic while modifying some state
/// and since most things (tables, closures, etc.) are shared through references,
/// this corrupted state may be observed by outside code.
/// If that code doesn't expect to find malformed data it may lead to weird and/or buggy behavior.
/// There is nothing runtime can do to help you.
/// In case this presents an issue,
/// the best course of action is to discard the runtime and construct a fresh one.
// pub fn reset(&mut self) {
//     self.stack.lua_frame().clear();
//     self.soft_reset();
// }

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

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: Types,
{
    // pub fn backtrace(&self) -> Backtrace {
    //     use rust_backtrace_stack::RustFrame;

    //     let mut start = self.frames.boundary();
    //     let mut frames = Vec::new();

    //     for rust_frame in self.rust_backtrace_stack.iter() {
    //         let RustFrame {
    //             position,
    //             backtrace,
    //         } = rust_frame;

    //         frames.extend(
    //             self.frames
    //                 .range(start..*position)
    //                 .unwrap_or_default()
    //                 .iter()
    //                 .map(|frame| frame.backtrace(&self.core.gc, self.chunk_cache)),
    //         );
    //         frames.push(backtrace.clone());
    //         start = *position
    //     }

    //     frames.extend(
    //         self.frames
    //             .range(start..)
    //             .unwrap_or_default()
    //             .iter()
    //             .map(|frame| frame.backtrace(&self.core.gc, self.chunk_cache)),
    //     );

    //     Backtrace { frames }
    // }
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
