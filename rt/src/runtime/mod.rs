mod dialect;
mod frame;
mod frame_stack;
mod rust_backtrace_stack;
mod stack;
mod upvalue_stack;

use std::fmt::{Debug, Display};
use std::ops::{Bound, ControlFlow};
use std::path::Path;

use enumoid::EnumMap;
use repr::index::StackSlot;

use crate::backtrace::{Backtrace, Location};
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::diagnostic::Diagnostic;
use crate::error::{BorrowError, RuntimeError};
use crate::ffi::LuaFfiOnce;
use crate::gc::{Gc as GarbageCollector, Visit};
use crate::value::{TypeProvider, TypeWithoutMetatable, Value};
use frame::{ChangeFrame, Event};
use frame_stack::{FrameStack, FrameStackView};
use rust_backtrace_stack::{RustBacktraceStack, RustBacktraceStackView};
use stack::{RawStackSlot, Stack, StackView};
use upvalue_stack::{UpvalueStack, UpvalueStackView};

pub use dialect::{CoerceArgs, DialectBuilder};
pub use frame::{Closure, ClosureRef, FunctionPtr};

pub struct Core<Gc: TypeProvider> {
    pub global_env: Value<Gc>,
    pub primitive_metatables: EnumMap<TypeWithoutMetatable, Option<Gc::TableRef>>,
    pub dialect: DialectBuilder,
    pub gc: Gc,
}

impl<Gc> Debug for Core<Gc>
where
    Gc: Debug + TypeProvider,
    Gc::TableRef: Debug,
    Value<Gc>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Core")
            .field("global_env", &self.global_env)
            .field("primitive_metatables", &self.primitive_metatables)
            .field("dialect", &self.dialect)
            .field("gc", &self.gc)
            .finish()
    }
}

pub struct Runtime<Gc: TypeProvider, C> {
    pub core: Core<Gc>,
    pub chunk_cache: C,
    frames: FrameStack<Value<Gc>>,
    stack: Stack<Value<Gc>>,
    upvalue_stack: UpvalueStack<Value<Gc>>,
    rust_backtrace_stack: RustBacktraceStack,
}

impl<Gc: TypeProvider, C> Runtime<Gc, C>
where
    C: Debug,
{
    pub fn new(chunk_cache: C, core: Core<Gc>) -> Self {
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

    pub fn view(&mut self) -> RuntimeView<Gc>
    where
        C: ChunkCache,
    {
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

pub struct RuntimeView<'rt, Gc: TypeProvider> {
    pub core: &'rt mut Core<Gc>,
    pub chunk_cache: &'rt mut dyn ChunkCache,
    frames: FrameStackView<'rt, Value<Gc>>,
    pub stack: StackView<'rt, Value<Gc>>,
    upvalue_stack: UpvalueStackView<'rt, Value<Gc>>,
    rust_backtrace_stack: RustBacktraceStackView<'rt>,
}

impl<'rt, Gc> RuntimeView<'rt, Gc>
where
    Gc: TypeProvider,
    Value<Gc>: Display,
{
    pub fn view_full(&mut self) -> RuntimeView<'_, Gc> {
        let Ok(view) = self.view(StackSlot(0)) else {
            unreachable!()
        };

        view
    }

    pub fn view(&mut self, start: StackSlot) -> Result<RuntimeView<'_, Gc>, RuntimeError<Gc>> {
        let start = self.stack.boundary() + start;
        self.view_raw(start)
    }

    fn view_raw(&mut self, start: RawStackSlot) -> Result<RuntimeView<'_, Gc>, RuntimeError<Gc>> {
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

    pub fn invoke(&mut self, f: impl LuaFfiOnce<Gc>) -> Result<(), RuntimeError<Gc>> {
        self.invoke_at(f, StackSlot(0))
    }

    pub fn invoke_at(
        &mut self,
        f: impl LuaFfiOnce<Gc>,
        start: StackSlot,
    ) -> Result<(), RuntimeError<Gc>> {
        let start = self.stack.boundary() + start;
        self.invoke_at_raw(f, start)
    }

    fn invoke_at_raw(
        &mut self,
        f: impl LuaFfiOnce<Gc>,
        start: RawStackSlot,
    ) -> Result<(), RuntimeError<Gc>> {
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

impl<'rt, Gc> RuntimeView<'rt, Gc>
where
    Gc: TypeProvider,
{
    fn soft_reset(&mut self) {
        self.upvalue_stack.clear();
        self.frames.clear();
        self.rust_backtrace_stack.clear();
    }
}

impl<'rt, Gc> RuntimeView<'rt, Gc>
where
    Gc: TypeProvider,
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

impl<'rt, Gc> RuntimeView<'rt, Gc>
where
    Gc: GarbageCollector,
    Gc::RustCallable: LuaFfiOnce<Gc>,
    Gc::Table: for<'a> Visit<Gc::Sweeper<'a>>,
    Value<Gc>: Debug + Display,
{
    pub fn enter(&mut self, closure: ClosureRef, start: StackSlot) -> Result<(), RuntimeError<Gc>> {
        let start = self.stack.boundary() + start;
        let frame = closure.construct_frame(self, start, None)?;
        self.frames.push(frame);

        while let ControlFlow::Continue(()) = self.resume()? {
            self.collect_garbage()?;
        }

        Ok(())
    }

    fn resume(&mut self) -> Result<ControlFlow<()>, RuntimeError<Gc>> {
        use crate::value::callable::Callable;

        let Some(frame) = self.frames.pop() else {
            return Ok(ControlFlow::Break(()));
        };
        let mut active_frame = frame.activate(self)?;

        for _ in 0..10000 {
            match active_frame.step() {
                Ok(ControlFlow::Break(ChangeFrame::Return(slot))) => {
                    active_frame.exit(slot)?;

                    let Some(frame) = self.frames.pop() else {
                        return Ok(ControlFlow::Break(()));
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
                                let mut stack = self.stack.view(start).expect(
                                    "stack space below invocation bound should be untouched",
                                );
                                stack.adjust_event_returns(event);
                            }

                            let frame = self
                                .frames
                                .pop()
                                .expect("suspended frame should still exist");
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

        let frame = active_frame.suspend();
        self.frames.push(frame);

        Ok(ControlFlow::Continue(()))
    }

    pub fn collect_garbage(&mut self) -> Result<(), BorrowError>
    where
        Gc::Table: for<'a> Visit<Gc::Sweeper<'a>>,
    {
        use crate::gc::{visit_borrow, Sweeper};

        let RuntimeView {
            core:
                Core {
                    global_env,
                    primitive_metatables,
                    dialect: _,
                    gc,
                },
            chunk_cache: _,
            frames,
            stack,
            upvalue_stack,
            rust_backtrace_stack: _,
        } = self;

        let mut sweeper = gc.sweeper();
        global_env.visit(&mut sweeper)?;
        for metatable in primitive_metatables
            .iter()
            .flat_map(|(_, table)| table.as_ref())
        {
            sweeper.mark_table(metatable);
            visit_borrow(metatable, &mut sweeper)?;
        }
        sweeper.mark_with_visitor(frames.iter())?;
        sweeper.mark_with_visitor(stack.iter())?;
        sweeper.mark_with_visitor(upvalue_stack.iter())?;

        sweeper.sweep();

        Ok(())
    }
}

impl<'rt, Gc: TypeProvider> RuntimeView<'rt, Gc> {
    pub fn construct_closure(
        &mut self,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = Value<Gc>>,
    ) -> Result<Closure, RuntimeError<Gc>> {
        Closure::new(self, fn_ptr, upvalues)
    }

    pub fn chunk_cache(&self) -> &dyn ChunkCache {
        self.chunk_cache
    }
}

impl<'rt, Gc: TypeProvider> RuntimeView<'rt, Gc> {
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

    pub fn load_from_file(&mut self, path: impl AsRef<Path>) -> Result<ChunkId, RuntimeError<Gc>>
    where
        Gc::String: From<String>,
        Gc: GarbageCollector,
    {
        let path = path.as_ref();

        let source = std::fs::read_to_string(path).map_err(|err| {
            let msg = self.core.gc.alloc_string(
                format!("failed to load file {}: {err}", path.to_string_lossy()).into(),
            );
            Value::String(msg)
        })?;
        let location = Location {
            file: path.to_string_lossy().to_string(),
            line: 0,
            column: 0,
        };

        self.load(source, Some(location)).map_err(Into::into)
    }
}

impl<'rt, Gc: TypeProvider> RuntimeView<'rt, Gc> {
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

    pub fn into_diagnostic(&self, err: RuntimeError<Gc>) -> Diagnostic
    where
        Gc: TypeProvider,
        Gc::String: AsRef<[u8]>,
        Value<Gc>: Display,
    {
        use codespan_reporting::files::SimpleFile;

        let message = match err {
            RuntimeError::Value(err) => err.into_diagnostic(),
            RuntimeError::Borrow(err) => err.into_diagnostic(),
            RuntimeError::AlreadyDropped(err) => err.into_diagnostic(),
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
    Immutable(crate::chunk_cache::ImmutableCacheError),
    CompilationFailure(Diagnostic),
}

impl From<crate::chunk_cache::ImmutableCacheError> for LoadError {
    fn from(value: crate::chunk_cache::ImmutableCacheError) -> Self {
        LoadError::Immutable(value)
    }
}

impl From<Diagnostic> for LoadError {
    fn from(value: Diagnostic) -> Self {
        LoadError::CompilationFailure(value)
    }
}

impl<Gc> From<LoadError> for RuntimeError<Gc>
where
    Gc: TypeProvider,
{
    fn from(value: LoadError) -> Self {
        match value {
            LoadError::Immutable(err) => RuntimeError::Immutable(err),
            LoadError::CompilationFailure(diag) => RuntimeError::Diagnostic(diag),
        }
    }
}
