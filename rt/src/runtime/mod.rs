mod dialect;
mod frame;
mod frame_stack;
mod rust_backtrace_stack;
mod stack;
// mod upvalue_stack;

use std::fmt::{Debug, Display};
use std::ops::{Bound, ControlFlow};
use std::path::Path;

use enumoid::EnumMap;
use gc::{Heap, Root};
use repr::index::StackSlot;

use crate::backtrace::{Backtrace, Location};
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::diagnostic::Diagnostic;
use crate::error::RuntimeError;
use crate::ffi::LuaFfiOnce;
use crate::value::{RootValue, Strong, TypeProvider, TypeWithoutMetatable, Types, Value};
use frame::{ChangeFrame, Event};
use frame_stack::{FrameStack, FrameStackView};
use rust_backtrace_stack::{RustBacktraceStack, RustBacktraceStackView};
use stack::{RawStackSlot, Stack};
// use upvalue_stack::{UpvalueStack, UpvalueStackView};

pub use dialect::{CoerceArgs, DialectBuilder};
pub use frame::{Closure, FunctionPtr};
pub use stack::StackView;

pub struct Core<Ty>
where
    Ty: TypeProvider,
{
    pub global_env: RootValue<Ty>,
    pub primitive_metatables: EnumMap<TypeWithoutMetatable, Option<<Strong<Ty> as Types>::Table>>,
    pub dialect: DialectBuilder,
    pub gc: Heap,
}

impl<Ty> Debug for Core<Ty>
where
    Ty: TypeProvider,
    RootValue<Ty>: Debug,
    <Strong<Ty> as Types>::Table: Debug,
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

pub struct Runtime<Ty, C>
where
    Ty: TypeProvider,
{
    pub core: Core<Ty>,
    pub chunk_cache: C,
    frames: FrameStack<RootValue<Ty>>,
    stack: Stack<RootValue<Ty>>,
    rust_backtrace_stack: RustBacktraceStack,
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: TypeProvider,
    C: Debug,
{
    pub fn new(chunk_cache: C, core: Core<Ty>) -> Self {
        tracing::trace!(?chunk_cache, "constructed runtime");

        Runtime {
            core,
            chunk_cache,
            frames: Default::default(),
            stack: Default::default(),
            rust_backtrace_stack: Default::default(),
        }
    }

    pub fn view(&mut self) -> RuntimeView<Ty>
    where
        C: ChunkCache,
    {
        let Runtime {
            core,
            chunk_cache,
            frames,
            stack,
            // upvalue_stack,
            rust_backtrace_stack,
        } = self;

        let frames = frames.view();
        let stack = stack.view();
        // let upvalue_stack = upvalue_stack.view();
        let rust_backtrace_stack = rust_backtrace_stack.view();

        RuntimeView {
            core,
            chunk_cache,
            frames,
            stack,
            // upvalue_stack,
            rust_backtrace_stack,
        }
    }
}

pub struct RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
{
    pub core: &'rt mut Core<Ty>,
    pub chunk_cache: &'rt mut dyn ChunkCache,
    frames: FrameStackView<'rt, RootValue<Ty>>,
    pub stack: StackView<'rt, RootValue<Ty>>,
    rust_backtrace_stack: RustBacktraceStackView<'rt>,
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
{
    pub fn view_full(&mut self) -> RuntimeView<'_, Ty> {
        let Ok(view) = self.view(StackSlot(0)) else {
            unreachable!()
        };

        view
    }

    pub fn view(&mut self, start: StackSlot) -> Result<RuntimeView<'_, Ty>, RuntimeError<Ty>> {
        let start = self.stack.boundary() + start;
        self.view_raw(start)
    }

    fn view_raw(&mut self, start: RawStackSlot) -> Result<RuntimeView<'_, Ty>, RuntimeError<Ty>> {
        use crate::error::OutOfBoundsStack;

        let RuntimeView {
            core,
            chunk_cache,
            frames,
            stack,
            rust_backtrace_stack,
        } = self;

        let frames = frames.view();
        let stack = stack.view(start).ok_or(OutOfBoundsStack)?;
        let rust_backtrace_stack = rust_backtrace_stack.view_over();

        let r = RuntimeView {
            core,
            chunk_cache: *chunk_cache,
            frames,
            stack,
            rust_backtrace_stack,
        };

        Ok(r)
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
    RootValue<Ty>: Display,
{
    pub fn invoke(&mut self, f: impl LuaFfiOnce<Ty>) -> Result<(), RuntimeError<Ty>> {
        self.invoke_at(f, StackSlot(0))
    }

    pub fn invoke_at(
        &mut self,
        f: impl LuaFfiOnce<Ty>,
        start: StackSlot,
    ) -> Result<(), RuntimeError<Ty>> {
        let start = self.stack.boundary() + start;
        self.invoke_at_raw(f, start)
    }

    fn invoke_at_raw(
        &mut self,
        f: impl LuaFfiOnce<Ty>,
        start: RawStackSlot,
    ) -> Result<(), RuntimeError<Ty>> {
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

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
{
    fn soft_reset(&mut self) {
        // self.upvalue_stack.clear();
        self.frames.clear();
        self.rust_backtrace_stack.clear();
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
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

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
    Ty::RustCallable: LuaFfiOnce<Ty>,
    RootValue<Ty>: Display,
{
    pub fn enter(
        &mut self,
        closure: Root<Closure>,
        start: StackSlot,
    ) -> Result<(), RuntimeError<Ty>> {
        use frame::Frame;

        let start = self.stack.boundary() + start;
        let frame = Frame::new(closure, self, start, None)?;
        self.frames.push(frame);

        while let ControlFlow::Continue(()) = self.resume()? {}

        Ok(())
    }

    fn resume(&mut self) -> Result<ControlFlow<()>, RuntimeError<Ty>> {
        use crate::value::callable::Callable;
        use frame::Frame;

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
                            let frame = Frame::new(closure.into(), self, start, event)?;
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
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
{
    pub fn construct_closure(
        &mut self,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = RootValue<Ty>>,
    ) -> Result<Closure, RuntimeError<Ty>> {
        Closure::new(self, fn_ptr, upvalues)
    }

    pub fn chunk_cache(&self) -> &dyn ChunkCache {
        self.chunk_cache
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
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

    pub fn load_from_file(&mut self, path: impl AsRef<Path>) -> Result<ChunkId, RuntimeError<Ty>>
    where
        Ty::String: From<String>,
    {
        let path = path.as_ref();

        let source = std::fs::read_to_string(path).map_err(|err| {
            let msg = std::rc::Rc::new(
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

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: TypeProvider,
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
                    .map(|frame| frame.backtrace(&self.core.gc, self.chunk_cache)),
            );
            frames.push(backtrace.clone());
            start = *position
        }

        frames.extend(
            self.frames
                .range(start..)
                .unwrap_or_default()
                .iter()
                .map(|frame| frame.backtrace(&self.core.gc, self.chunk_cache)),
        );

        Backtrace { frames }
    }

    pub fn into_diagnostic(&self, err: RuntimeError<Ty>) -> Diagnostic
    where
        Ty::String: AsRef<[u8]>,
        // RootValue<Ty>: Display,
    {
        use codespan_reporting::files::SimpleFile;

        let message = match err {
            RuntimeError::Value(err) => todo!(), //err.into_diagnostic(),
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
                let ptr = frame.fn_ptr(&self.core.gc);

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

impl<Ty> From<LoadError> for RuntimeError<Ty>
where
    Ty: TypeProvider,
{
    fn from(value: LoadError) -> Self {
        match value {
            LoadError::Immutable(err) => RuntimeError::Immutable(err),
            LoadError::CompilationFailure(diag) => RuntimeError::Diagnostic(diag),
        }
    }
}
