mod frame;
mod frame_stack;
mod rust_backtrace_stack;
mod stack;
mod upvalue_stack;

use std::fmt::Debug;
use std::ops::{Bound, ControlFlow};
use std::path::Path;

use repr::index::StackSlot;

use crate::backtrace::{Backtrace, Location};
use crate::chunk_cache::{ChunkCache, ChunkId, KeyedChunkCache};
use crate::error::diagnostic::Diagnostic;
use crate::error::RuntimeError;
use crate::ffi::LuaFfiOnce;
use crate::value::Value;
use frame::ChangeFrame;
use frame_stack::{FrameStack, FrameStackView};
use rust_backtrace_stack::{RustBacktraceStack, RustBacktraceStackView};
use stack::{RawStackSlot, Stack, StackView};
use upvalue_stack::{UpvalueStack, UpvalueStackView};

pub use frame::{Closure, ClosureRef, FunctionPtr};

pub struct Runtime<C> {
    pub chunk_cache: C,
    pub global_env: Value<C>,
    frames: FrameStack<C>,
    stack: Stack<C>,
    upvalue_stack: UpvalueStack<C>,
    rust_backtrace_stack: RustBacktraceStack,
}

impl<C> Runtime<C>
where
    C: Debug,
{
    pub fn new(chunk_cache: C, global_env: Value<C>) -> Self {
        tracing::trace!(?chunk_cache, "constructed runtime");

        Runtime {
            chunk_cache,
            global_env,
            frames: Default::default(),
            stack: Default::default(),
            upvalue_stack: Default::default(),
            rust_backtrace_stack: Default::default(),
        }
    }

    pub fn view(&mut self) -> RuntimeView<C> {
        let Runtime {
            chunk_cache,
            global_env,
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
            chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
            rust_backtrace_stack,
        }
    }
}

pub struct RuntimeView<'rt, C> {
    chunk_cache: &'rt mut C,
    pub global_env: &'rt Value<C>,
    frames: FrameStackView<'rt, C>,
    pub stack: StackView<'rt, C>,
    upvalue_stack: UpvalueStackView<'rt, C>,
    rust_backtrace_stack: RustBacktraceStackView<'rt>,
}

impl<'rt, C> RuntimeView<'rt, C> {
    pub fn view(&mut self, start: StackSlot) -> Result<RuntimeView<C>, RuntimeError<C>> {
        let start = self.stack.boundary() + start;
        self.view_raw(start)
    }

    fn view_raw(&mut self, start: RawStackSlot) -> Result<RuntimeView<C>, RuntimeError<C>> {
        use crate::error::OutOfBoundsStack;

        let RuntimeView {
            chunk_cache,
            global_env,
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
            chunk_cache: *chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
            rust_backtrace_stack,
        };

        Ok(r)
    }

    pub fn invoke(&mut self, f: impl LuaFfiOnce<C>) -> Result<(), RuntimeError<C>> {
        self.invoke_at(f, StackSlot(0))
    }

    pub fn invoke_at(
        &mut self,
        f: impl LuaFfiOnce<C>,
        start: StackSlot,
    ) -> Result<(), RuntimeError<C>> {
        let start = self.stack.boundary() + start;
        self.invoke_at_raw(f, start)
    }

    fn invoke_at_raw(
        &mut self,
        f: impl LuaFfiOnce<C>,
        start: RawStackSlot,
    ) -> Result<(), RuntimeError<C>> {
        use crate::backtrace::{BacktraceFrame, FrameSource};
        use rust_backtrace_stack::RustFrame;

        let rust_frame = RustFrame {
            position: self.frames.next_raw_id(),
            backtrace: BacktraceFrame {
                source: FrameSource::Rust,
                name: Some(f.name()),
                location: None,
            },
        };

        let mut view = self.view_raw(start)?;
        view.rust_backtrace_stack.push(rust_frame);

        tracing::trace!(
            stack = view.stack.to_pretty_string(),
            "entering Rust function"
        );

        let r = f.call_once(view.view(StackSlot(0)).unwrap());

        // Forcefully clean up.
        // It is possible that Rust function called Lua panic but forgot to reset the runtime.
        // This guarantees that it is always safe to return control to caller when panic is not propagated.
        if r.is_ok() {
            view.soft_reset();
        }

        r
    }

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

    fn soft_reset(&mut self) {
        self.upvalue_stack.clear();
        self.frames.clear();
        self.rust_backtrace_stack.clear();
    }
}

impl<'rt, C> RuntimeView<'rt, C>
where
    C: ChunkCache,
{
    pub fn enter(&mut self, closure: ClosureRef, start: StackSlot) -> Result<(), RuntimeError<C>> {
        use crate::value::callable::Callable;

        let start = self.stack.boundary() + start;
        let frame = closure.construct_frame(self, start)?;
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
                Ok(ControlFlow::Break(ChangeFrame::Invoke(callable, start))) => {
                    let frame = active_frame.suspend();
                    // Make sure to convert slot here!
                    // Stack slot is in relation to already suspended frame which most likely
                    // have different boundary from view held by `enter` function itself.
                    let start = frame.stack_boundary() + start;
                    self.frames.push(frame);

                    match callable {
                        Callable::LuaClosure(closure) => {
                            let frame = closure.construct_frame(self, start)?;
                            active_frame = frame.activate(self)?;
                        }
                        Callable::RustClosure(closure) => {
                            self.invoke_at_raw(closure, start)?;

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
        upvalues: impl IntoIterator<Item = Value<C>>,
    ) -> Result<Closure, RuntimeError<C>> {
        use crate::error::{MissingChunk, MissingFunction};

        let signature = &self
            .chunk_cache
            .chunk(fn_ptr.chunk_id)
            .ok_or(MissingChunk(fn_ptr.chunk_id))?
            .get_function(fn_ptr.function_id)
            .ok_or(MissingFunction(fn_ptr))?
            .signature;

        let upvalues = upvalues
            .into_iter()
            .chain(std::iter::repeat(Value::Nil))
            .take(signature.upvalue_count)
            .map(|value| self.stack.fresh_upvalue(value))
            .collect();

        let closure = Closure { fn_ptr, upvalues };

        Ok(closure)
    }
}

impl<'rt, C> RuntimeView<'rt, C> {
    pub fn chunk_cache(&self) -> &C {
        self.chunk_cache
    }
}

impl<'rt, C> RuntimeView<'rt, C>
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
        f: impl FnOnce() -> Result<(String, Option<Location>), RuntimeError<C>>,
    ) -> Result<ChunkId, RuntimeError<C>>
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

    pub fn load_from_file(&mut self, path: impl AsRef<Path>) -> Result<ChunkId, RuntimeError<C>>
    where
        C: KeyedChunkCache<Path>,
    {
        let path = path.as_ref();

        self.precompiled_or_load_with(path, || {
            let source = std::fs::read_to_string(path).map_err(|err| {
                Value::String(format!(
                    "failed to load file {}: {err}",
                    path.to_string_lossy()
                ))
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

impl<'rt, C> RuntimeView<'rt, C>
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

    pub fn into_diagnostic(&self, err: RuntimeError<C>) -> Diagnostic {
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

fn map_bound<T, U>(bound: Bound<T>, f: impl FnOnce(T) -> U) -> Bound<U> {
    use std::ops::Bound::*;

    match bound {
        Included(t) => Included(f(t)),
        Excluded(t) => Excluded(f(t)),
        Unbounded => Unbounded,
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

impl<C> From<LoadError> for RuntimeError<C> {
    fn from(value: LoadError) -> Self {
        match value {
            LoadError::Immutable(err) => RuntimeError::Immutable(err),
            LoadError::CompilationFailure(diag) => RuntimeError::Diagnostic(diag),
        }
    }
}
