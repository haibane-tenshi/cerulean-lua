mod dialect;
mod frame;
mod frame_stack;
mod rust_backtrace_stack;
mod stack;
mod upvalue_stack;

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::{Bound, ControlFlow};
use std::path::Path;

use enumoid::EnumMap;
use repr::index::StackSlot;

use crate::backtrace::{Backtrace, Location};
use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::error::diagnostic::Diagnostic;
use crate::error::RuntimeError;
use crate::ffi::LuaFfiOnce;
use crate::value::table::{KeyValue, TableRef};
use crate::value::{TypeProvider, TypeWithoutMetatable, Value};
use frame::{ChangeFrame, Event};
use frame_stack::{FrameStack, FrameStackView};
use rust_backtrace_stack::{RustBacktraceStack, RustBacktraceStackView};
use stack::{RawStackSlot, Stack, StackView};
use upvalue_stack::{UpvalueStack, UpvalueStackView};

pub use dialect::{CoerceArgs, DialectBuilder};
pub use frame::{Closure, ClosureRef, FunctionPtr};

pub struct Core<Types: TypeProvider> {
    pub global_env: Value<Types>,
    pub primitive_metatables: EnumMap<TypeWithoutMetatable, Option<Types::Table>>,
    pub dialect: DialectBuilder,
}

impl<Types> Debug for Core<Types>
where
    Types: TypeProvider,
    Types::Table: Debug,
    Value<Types>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Core")
            .field("global_env", &self.global_env)
            .field("primitive_metatables", &self.primitive_metatables)
            .field("dialect", &self.dialect)
            .finish()
    }
}

pub struct Runtime<Types: TypeProvider, C> {
    pub core: Core<Types>,
    pub chunk_cache: C,
    frames: FrameStack<Value<Types>>,
    stack: Stack<Value<Types>>,
    upvalue_stack: UpvalueStack<Value<Types>>,
    rust_backtrace_stack: RustBacktraceStack,
}

impl<Types: TypeProvider, C> Runtime<Types, C>
where
    C: Debug,
{
    pub fn new(chunk_cache: C, core: Core<Types>) -> Self {
        tracing::trace!(?chunk_cache, "constructed runtime");

        Runtime {
            core,
            chunk_cache,
            frames: Default::default(),
            stack: Default::default(),
            upvalue_stack: Default::default(),
            rust_backtrace_stack: Default::default(),
        }
    }

    pub fn view(&mut self) -> RuntimeView<Types, C> {
        let Runtime {
            core,
            chunk_cache,
            frames,
            stack,
            upvalue_stack,
            rust_backtrace_stack,
        } = self;

        let frames = frames.view();
        let stack = stack.view();
        let upvalue_stack = upvalue_stack.view();
        let rust_backtrace_stack = rust_backtrace_stack.view();

        RuntimeView {
            core,
            chunk_cache,
            frames,
            stack,
            upvalue_stack,
            rust_backtrace_stack,
        }
    }
}

pub struct RuntimeView<'rt, Types: TypeProvider, C> {
    pub core: &'rt mut Core<Types>,
    pub chunk_cache: &'rt mut C,
    frames: FrameStackView<'rt, Value<Types>>,
    pub stack: StackView<'rt, Value<Types>>,
    upvalue_stack: UpvalueStackView<'rt, Value<Types>>,
    rust_backtrace_stack: RustBacktraceStackView<'rt>,
}

impl<'rt, Types, C> RuntimeView<'rt, Types, C>
where
    Types: TypeProvider,
    Value<Types>: Display,
{
    pub fn view_full(&mut self) -> RuntimeView<'_, Types, C> {
        let Ok(view) = self.view(StackSlot(0)) else {
            unreachable!()
        };

        view
    }

    pub fn view(
        &mut self,
        start: StackSlot,
    ) -> Result<RuntimeView<'_, Types, C>, RuntimeError<Types>> {
        let start = self.stack.boundary() + start;
        self.view_raw(start)
    }

    fn view_raw(
        &mut self,
        start: RawStackSlot,
    ) -> Result<RuntimeView<'_, Types, C>, RuntimeError<Types>> {
        use crate::error::OutOfBoundsStack;

        let RuntimeView {
            core,
            chunk_cache,
            frames,
            stack,
            upvalue_stack,
            rust_backtrace_stack,
        } = self;

        let frames = frames.view();
        let stack = stack.view(start).ok_or(OutOfBoundsStack)?;
        let upvalue_stack = upvalue_stack.view_over();
        let rust_backtrace_stack = rust_backtrace_stack.view_over();

        let r = RuntimeView {
            core,
            chunk_cache: *chunk_cache,
            frames,
            stack,
            upvalue_stack,
            rust_backtrace_stack,
        };

        Ok(r)
    }

    pub fn invoke(&mut self, f: impl LuaFfiOnce<Types, C>) -> Result<(), RuntimeError<Types>> {
        self.invoke_at(f, StackSlot(0))
    }

    pub fn invoke_at(
        &mut self,
        f: impl LuaFfiOnce<Types, C>,
        start: StackSlot,
    ) -> Result<(), RuntimeError<Types>> {
        let start = self.stack.boundary() + start;
        self.invoke_at_raw(f, start)
    }

    fn invoke_at_raw(
        &mut self,
        f: impl LuaFfiOnce<Types, C>,
        start: RawStackSlot,
    ) -> Result<(), RuntimeError<Types>> {
        use crate::backtrace::{BacktraceFrame, FrameSource};
        use rust_backtrace_stack::RustFrame;

        let rust_frame = RustFrame {
            position: self.frames.next_raw_id(),
            backtrace: BacktraceFrame {
                source: FrameSource::Rust,
                name: Some(f.debug_info().name),
                location: None,
            },
        };

        let mut view = self.view_raw(start)?;
        view.rust_backtrace_stack.push(rust_frame);

        tracing::trace!(
            stack = view.stack.to_pretty_string(),
            "entering Rust function"
        );

        let r = f.call_once(view.view_full());

        // Forcefully clean up.
        // It is possible that Rust function called Lua panic but forgot to reset the runtime.
        // This guarantees that it is always safe to return control to caller when panic is not propagated.
        if r.is_ok() {
            view.soft_reset();
        }

        r
    }
}

impl<'rt, Types, C> RuntimeView<'rt, Types, C>
where
    Types: TypeProvider,
{
    fn soft_reset(&mut self) {
        self.upvalue_stack.clear();
        self.frames.clear();
        self.rust_backtrace_stack.clear();
    }
}

impl<'rt, Types, C> RuntimeView<'rt, Types, C>
where
    Types: TypeProvider,
    Value<Types>: Clone,
{
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
    /// There is nothing that runtime can do to help you.
    /// In case this presents an issue,
    /// the best thing you can do is to discard the runtime and construct a fresh one.
    pub fn reset(&mut self) {
        self.stack.clear();
        self.soft_reset();
    }
}

impl<'rt, Types, C> RuntimeView<'rt, Types, C>
where
    C: ChunkCache,
    Types: TypeProvider<String = String, Table = TableRef<Types>>,
    KeyValue<Types>: Hash + Eq + From<Event>,
    Value<Types>: Clone + Display + PartialEq,
    Types::RustCallable: LuaFfiOnce<Types, C>,
{
    pub fn enter(
        &mut self,
        closure: ClosureRef,
        start: StackSlot,
    ) -> Result<(), RuntimeError<Types>> {
        use crate::value::callable::Callable;

        let start = self.stack.boundary() + start;
        let frame = closure.construct_frame(self, start, None)?;
        let mut active_frame = frame.activate(self)?;

        loop {
            match active_frame.step() {
                Ok(ControlFlow::Break(ChangeFrame::Return(slot))) => {
                    active_frame.exit(slot)?;

                    let Some(frame) = self.frames.pop() else {
                        break;
                    };

                    active_frame = frame.activate(self)?;
                }
                Ok(ControlFlow::Break(ChangeFrame::Invoke(event, callable, start))) => {
                    let frame = active_frame.suspend();
                    self.frames.push(frame);

                    match callable {
                        Callable::Lua(closure) => {
                            let frame = closure.construct_frame(self, start, event)?;
                            active_frame = frame.activate(self)?;
                        }
                        Callable::Rust(closure) => {
                            self.invoke_at_raw(closure, start)?;

                            if let Some(event) = event {
                                let mut stack = self.stack.view(start).unwrap();
                                stack.adjust_event_returns(event);
                            }

                            let frame = self.frames.pop().unwrap();
                            active_frame = frame.activate(self)?;
                        }
                    }
                }
                Ok(ControlFlow::Continue(())) => (),
                Err(err) => {
                    // Make sure to preserve current frame in case opcode panicked.
                    let frame = active_frame.suspend();
                    self.frames.push(frame);

                    return Err(err.into());
                }
            }
        }

        Ok(())
    }

    pub fn construct_closure(
        &mut self,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = Value<Types>>,
    ) -> Result<Closure, RuntimeError<Types>> {
        Closure::new(self, fn_ptr, upvalues)
    }
}

impl<'rt, Types: TypeProvider, C> RuntimeView<'rt, Types, C> {
    pub fn chunk_cache(&self) -> &C {
        self.chunk_cache
    }
}

impl<'rt, Types: TypeProvider, C> RuntimeView<'rt, Types, C>
where
    C: ChunkCache,
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

    pub fn load_with_key<Q>(
        &mut self,
        key: &Q,
        source: String,
        location: Option<Location>,
    ) -> Result<ChunkId, LoadError>
    where
        Q: ?Sized,
        C: KeyedChunkCache<Q>,
    {
        let chunk_id = self.load(source, location)?;
        let _ = self.chunk_cache.bind(key, chunk_id);

        Ok(chunk_id)
    }

    pub fn precompiled_or_load_with<Q>(
        &mut self,
        key: &Q,
        f: impl FnOnce() -> Result<(String, Option<Location>), RuntimeError<Types>>,
    ) -> Result<ChunkId, RuntimeError<Types>>
    where
        Q: ?Sized,
        C: KeyedChunkCache<Q>,
    {
        if let Some(chunk_id) = self.chunk_cache.get(key) {
            return Ok(chunk_id);
        }

        let (source, location) = f()?;
        self.load_with_key(key, source, location)
            .map_err(Into::into)
    }

    pub fn load_from_file(&mut self, path: impl AsRef<Path>) -> Result<ChunkId, RuntimeError<Types>>
    where
        C: KeyedChunkCache<Path>,
        Types::String: From<String>,
    {
        let path = path.as_ref();

        self.precompiled_or_load_with(path, || {
            let source = std::fs::read_to_string(path).map_err(|err| {
                Value::String(
                    format!("failed to load file {}: {err}", path.to_string_lossy()).into(),
                )
            })?;
            let location = Location {
                file: path.to_string_lossy().to_string(),
                line: 0,
                column: 0,
            };

            Ok((source, Some(location)))
        })
    }
}

impl<'rt, Types: TypeProvider, C> RuntimeView<'rt, Types, C>
where
    C: ChunkCache,
{
    pub fn backtrace(&self) -> Backtrace {
        use rust_backtrace_stack::RustFrame;

        let mut start = self.frames.boundary();
        let mut frames = Vec::new();

        for rust_frame in self.rust_backtrace_stack.iter() {
            let RustFrame {
                position,
                backtrace,
            } = rust_frame;

            frames.extend(
                self.frames
                    .range(start..*position)
                    .unwrap_or_default()
                    .iter()
                    .map(|frame| frame.backtrace(self.chunk_cache)),
            );
            frames.push(backtrace.clone());
            start = *position
        }

        frames.extend(
            self.frames
                .range(start..)
                .unwrap_or_default()
                .iter()
                .map(|frame| frame.backtrace(self.chunk_cache)),
        );

        Backtrace { frames }
    }

    pub fn into_diagnostic(&self, err: RuntimeError<Types>) -> Diagnostic
    where
        Types: TypeProvider<String = String>,
        Value<Types>: Display,
    {
        use codespan_reporting::files::SimpleFile;

        let message = match err {
            RuntimeError::Value(err) => err.into_diagnostic(),
            RuntimeError::Immutable(err) => err.into_diagnostic(),
            RuntimeError::Diagnostic(diag) => return diag,
            RuntimeError::MissingChunk(err) => err.into_diagnostic(),
            RuntimeError::MissingFunction(err) => err.into_diagnostic(),
            RuntimeError::OutOfBoundsStack(err) => err.into_diagnostic(),
            RuntimeError::UpvalueCountMismatch(err) => err.into_diagnostic(),
            RuntimeError::OpCode(err) => err.into_diagnostic(()),
        };

        let (name, source) = self
            .frames
            .last()
            .map(|frame| {
                let ptr = frame.fn_ptr();

                let source = self.chunk_cache.source(ptr.chunk_id);
                let name = self
                    .chunk_cache
                    .location(ptr.chunk_id)
                    .map(|location| location.file);

                (name, source)
            })
            .unwrap_or_default();

        let name = name.unwrap_or_else(|| "<unnamed>".to_string());
        let source = source.unwrap_or_default();

        let files = SimpleFile::new(name, source);

        Diagnostic { files, message }
    }
}

trait MapBound<F> {
    type Output;

    fn mapb(self, f: F) -> Self::Output;
}

impl<F, T, U> MapBound<F> for Bound<T>
where
    F: FnOnce(T) -> U,
{
    type Output = Bound<U>;

    fn mapb(self, f: F) -> Self::Output {
        use Bound::*;

        match self {
            Included(t) => Included(f(t)),
            Excluded(t) => Excluded(f(t)),
            Unbounded => Unbounded,
        }
    }
}

#[derive(Debug)]
pub enum LoadError {
    Immutable(crate::chunk_cache::Immutable),
    CompilationFailure(Diagnostic),
}

impl From<crate::chunk_cache::Immutable> for LoadError {
    fn from(value: crate::chunk_cache::Immutable) -> Self {
        LoadError::Immutable(value)
    }
}

impl From<Diagnostic> for LoadError {
    fn from(value: Diagnostic) -> Self {
        LoadError::CompilationFailure(value)
    }
}

impl<Types> From<LoadError> for RuntimeError<Types>
where
    Types: TypeProvider,
{
    fn from(value: LoadError) -> Self {
        match value {
            LoadError::Immutable(err) => RuntimeError::Immutable(err),
            LoadError::CompilationFailure(diag) => RuntimeError::Diagnostic(diag),
        }
    }
}
