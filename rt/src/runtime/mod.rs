mod dialect;
mod frame;
mod frame_stack;
mod interner;
mod rust_backtrace_stack;
mod stack;

use std::fmt::{Debug, Display};
use std::ops::{Bound, ControlFlow};
use std::path::Path;

use enumoid::EnumMap;
use gc::{Heap, Root, RootCell};
use repr::index::StackSlot;

use crate::backtrace::{Backtrace, Location};
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::diagnostic::Diagnostic;
use crate::error::{AlreadyDroppedError, RuntimeError};
use crate::ffi::{LuaFfi, LuaFfiOnce};
use crate::value::{CoreTypes, Strong, StrongValue, TypeWithoutMetatable, Types, Value, WeakValue};
use frame::{ChangeFrame, Event, Frame};
use frame_stack::{FrameStack, FrameStackView};
use interner::Interner;
use rust_backtrace_stack::{RustBacktraceStack, RustBacktraceStackView};
use stack::{RawStackSlot, Stack};

pub use dialect::{CoerceArgs, DialectBuilder};
pub use frame::{Closure, FunctionPtr};
pub use interner::Interned;
pub use stack::{StackFrame, StackGuard, TransientStackFrame};

pub struct Core<Ty>
where
    Ty: CoreTypes,
{
    pub global_env: StrongValue<Ty>,
    pub primitive_metatables: EnumMap<TypeWithoutMetatable, Option<<Strong<Ty> as Types>::Table>>,
    pub dialect: DialectBuilder,
    pub gc: Heap,
}

type GcTable<Ty> = <crate::value::Weak<Ty> as Types>::Table;

impl<Ty> Core<Ty>
where
    Ty: CoreTypes,
{
    pub fn metatable_of(
        &self,
        value: &WeakValue<Ty>,
    ) -> Result<Option<GcTable<Ty>>, AlreadyDroppedError> {
        use crate::value::Metatable;
        use TypeWithoutMetatable::*;

        let Core {
            primitive_metatables,
            gc: heap,
            ..
        } = self;

        let r = match value {
            Value::Nil => primitive_metatables[Nil].as_ref().map(|t| t.downgrade()),
            Value::Bool(_) => primitive_metatables[Bool].as_ref().map(|t| t.downgrade()),
            Value::Int(_) => primitive_metatables[Int].as_ref().map(|t| t.downgrade()),
            Value::Float(_) => primitive_metatables[Float].as_ref().map(|t| t.downgrade()),
            Value::String(_) => primitive_metatables[String].as_ref().map(|t| t.downgrade()),
            Value::Function(_) => primitive_metatables[Function]
                .as_ref()
                .map(|t| t.downgrade()),
            Value::Table(t) => heap.get(*t).ok_or(AlreadyDroppedError)?.metatable(),
            Value::Userdata(t) => heap.get(*t).ok_or(AlreadyDroppedError)?.metatable(),
        };

        Ok(r)
    }
}

impl<Ty> Debug for Core<Ty>
where
    Ty: CoreTypes,
    StrongValue<Ty>: Debug,
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
    Ty: CoreTypes,
{
    pub core: Core<Ty>,
    pub chunk_cache: C,
    frames: FrameStack<Frame<Ty>>,
    stack: Stack<Ty>,
    rust_backtrace_stack: RustBacktraceStack,
    string_interner: Interner<Ty::String>,
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: CoreTypes,
    C: Debug,
{
    pub fn new(chunk_cache: C, mut core: Core<Ty>) -> Self {
        tracing::trace!(?chunk_cache, "constructed runtime");

        let stack = Stack::new(&mut core.gc);

        Runtime {
            core,
            chunk_cache,
            frames: Default::default(),
            stack,
            rust_backtrace_stack: Default::default(),
            string_interner: Default::default(),
        }
    }
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: CoreTypes,
    C: ChunkCache,
{
    pub fn view(&mut self) -> RuntimeView<Ty> {
        let Runtime {
            core,
            chunk_cache,
            frames,
            stack,
            // upvalue_stack,
            rust_backtrace_stack,
            string_interner,
        } = self;

        let frames = frames.view();
        let stack = stack.guard();
        // let upvalue_stack = upvalue_stack.view();
        let rust_backtrace_stack = rust_backtrace_stack.view();

        RuntimeView {
            core,
            chunk_cache,
            frames,
            stack,
            // upvalue_stack,
            rust_backtrace_stack,
            string_interner,
        }
    }
}

pub struct RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
{
    pub core: &'rt mut Core<Ty>,
    pub chunk_cache: &'rt mut dyn ChunkCache,
    frames: FrameStackView<'rt, Frame<Ty>>,
    pub stack: StackGuard<'rt, Ty>,
    rust_backtrace_stack: RustBacktraceStackView<'rt>,
    string_interner: &'rt mut Interner<Ty::String>,
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
{
    pub fn view_full(&mut self) -> RuntimeView<'_, Ty> {
        self.view(StackSlot(0)).unwrap()
    }

    pub fn view(&mut self, start: StackSlot) -> Option<RuntimeView<'_, Ty>> {
        let start = self.stack.boundary() + start;
        self.view_raw(start)
    }

    fn view_raw(&mut self, start: RawStackSlot) -> Option<RuntimeView<'_, Ty>> {
        let RuntimeView {
            core,
            chunk_cache,
            frames,
            stack,
            rust_backtrace_stack,
            string_interner,
        } = self;

        let stack = stack.guard(start)?;
        let frames = frames.view();
        let rust_backtrace_stack = rust_backtrace_stack.view_over();

        let r = RuntimeView {
            core,
            chunk_cache: *chunk_cache,
            frames,
            stack,
            rust_backtrace_stack,
            string_interner,
        };

        Some(r)
    }

    pub fn alloc_string(&mut self, s: Ty::String) -> Root<Interned<Ty::String>> {
        self.string_interner.insert(s, &mut self.core.gc)
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Display,
{
    pub fn invoke_at(
        &mut self,
        f: impl LuaFfiOnce<Ty>,
        start: StackSlot,
    ) -> Result<(), RuntimeError<Ty>> {
        use crate::error::OutOfBoundsStack;

        let rt = self.view(start).ok_or(OutOfBoundsStack)?;
        rt.invoke(f)
    }

    pub fn invoke(mut self, f: impl LuaFfiOnce<Ty>) -> Result<(), RuntimeError<Ty>> {
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

        // Do an extra protective view.
        // We cannot reasonably expect `self` to be in consistent state
        // because caller might have caught `RuntimeError` and immediately passed
        // the runtime to us.
        let mut view = self.view_full();
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
    Ty: CoreTypes,
{
    fn soft_reset(&mut self) {
        self.frames.clear();
        self.rust_backtrace_stack.clear();
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
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
    /// There is nothing runtime can do to help you.
    /// In case this presents an issue,
    /// the best course of action is to discard the runtime and construct a fresh one.
    pub fn reset(&mut self) {
        self.stack.lua_frame().clear();
        self.soft_reset();
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
    Ty::RustClosure: LuaFfi<Ty>,
    WeakValue<Ty>: Display,
{
    pub fn enter(mut self, closure: RootCell<Closure<Ty>>) -> Result<(), RuntimeError<Ty>> {
        use crate::error::OutOfBoundsStack;
        use crate::value::callable::Callable;

        let frame = Frame::new(&mut self, closure, None)?;

        let mut active_frame = frame.activate(&mut self)?;

        loop {
            match active_frame.step() {
                Ok(ControlFlow::Break(ChangeFrame::Return(slot))) => {
                    active_frame.exit(slot)?;

                    let Some(frame) = self.frames.pop() else {
                        break Ok(());
                    };

                    active_frame = frame.activate(&mut self)?;
                }
                Ok(ControlFlow::Break(ChangeFrame::Invoke(event, callable, start))) => {
                    let frame = active_frame.suspend();
                    self.frames.push(frame);

                    let mut rt = self.view_raw(start).ok_or(OutOfBoundsStack)?;
                    // Ensure that stack space passed to another function no longer hosts upvalues.
                    rt.stack.lua_frame().evict_upvalues();

                    match callable {
                        Callable::Lua(closure) => {
                            let frame = Frame::new(&mut rt, closure, event)?;
                            active_frame = frame.activate(&mut self)?;
                        }
                        Callable::Rust(closure) => {
                            rt.stack.lua_frame().sync_transient(&mut rt.core.gc);
                            rt.invoke(closure)?;

                            if let Some(event) = event {
                                let mut stack = self.stack.guard(start).expect(
                                    "stack space below invocation bound should be untouched",
                                );
                                stack.lua_frame().adjust_event_returns(event);
                            }

                            let frame = self
                                .frames
                                .pop()
                                .expect("suspended frame should still exist");

                            active_frame = frame.activate(&mut self)?;
                        }
                    }
                }
                Ok(ControlFlow::Continue(())) => (),
                Err(err) => {
                    // Make sure to preserve current frame in case opcode panicked.
                    let frame = active_frame.suspend();
                    self.frames.push(frame);

                    break Err(err.into());
                }
            }
        }
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
{
    pub fn construct_closure(
        &mut self,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = WeakValue<Ty>>,
    ) -> Result<RootCell<Closure<Ty>>, RuntimeError<Ty>> {
        Closure::new(self, fn_ptr, upvalues)
    }

    pub fn chunk_cache(&self) -> &dyn ChunkCache {
        self.chunk_cache
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
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
        use crate::gc::StringRef;

        let path = path.as_ref();

        let source = std::fs::read_to_string(path).map_err(|err| {
            let msg = StringRef::new(
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
    Ty: CoreTypes,
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
        StrongValue<Ty>: Display,
    {
        use codespan_reporting::files::SimpleFile;

        let message = match err {
            RuntimeError::Value(err) => err.into_diagnostic(),
            RuntimeError::Borrow(err) => err.into_diagnostic(),
            RuntimeError::AlreadyDropped(err) => err.into_diagnostic(),
            RuntimeError::Immutable(err) => err.into_diagnostic(),
            RuntimeError::Diagnostic(diag) => return *diag,
            RuntimeError::MissingChunk(err) => err.into_diagnostic(),
            RuntimeError::MissingFunction(err) => err.into_diagnostic(),
            RuntimeError::OutOfBoundsStack(err) => err.into_diagnostic(),
            RuntimeError::UpvalueCountMismatch(err) => err.into_diagnostic(),
            RuntimeError::OpCode(err) => {
                if let Some(frame) = self.frames.last() {
                    let ip = frame.current_ip();
                    let fn_ptr = self.core.gc[frame.closure()].fn_ptr();
                    let chunk = self
                        .chunk_cache
                        .chunk(fn_ptr.chunk_id)
                        .expect("closure should be constructed out of existing chunk");
                    let function = chunk
                        .get_function(fn_ptr.function_id)
                        .expect("closure should be constructed out of existing function");

                    let opcode = *function
                        .opcodes
                        .get(ip)
                        .expect("error should be constructed out of existing opcode");

                    let debug_info = chunk
                        .debug_info
                        .as_ref()
                        .and_then(|info| info.functions.get(fn_ptr.function_id))
                        .and_then(|info| info.opcodes.get(ip))
                        .cloned();

                    err.into_diagnostic((), opcode, debug_info)
                } else {
                    use crate::error::ExtraDiagnostic;
                    use codespan_reporting::diagnostic::Diagnostic;

                    let mut diag = Diagnostic::error()
                        .with_message("opcode error failed to generate diagnostic");

                    diag.with_help([
                        "opcode-related errors only carry the cause of failure, all additional information is left inside runtime",
                        "if you see this error, most likely runtime was reset before diagnostic was generated"
                    ]);

                    diag
                }
            }
        };

        let (name, source) = self
            .frames
            .last()
            .map(|frame| {
                let ptr = self.core.gc[frame.closure()].fn_ptr();

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

impl<Ty> From<LoadError> for RuntimeError<Ty>
where
    Ty: CoreTypes,
{
    fn from(value: LoadError) -> Self {
        match value {
            LoadError::Immutable(err) => RuntimeError::Immutable(err),
            LoadError::CompilationFailure(diag) => RuntimeError::Diagnostic(diag),
        }
    }
}
