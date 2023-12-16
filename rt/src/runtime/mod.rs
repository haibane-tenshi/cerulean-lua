mod frame;
mod frame_stack;
mod stack;
mod upvalue_stack;

use std::fmt::Debug;
use std::ops::ControlFlow;

use repr::index::StackSlot;

use crate::backtrace::Backtrace;
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::RuntimeError;
use crate::ffi::LuaFfiOnce;
use crate::value::Value;
use frame::ChangeFrame;
use frame_stack::{FrameStack, FrameStackView};
use stack::{Stack, StackView};
use upvalue_stack::UpvalueView;

pub use frame::{Closure, ClosureRef, FunctionPtr};

pub struct Runtime<C> {
    pub chunk_cache: C,
    pub global_env: Value<C>,
    frames: FrameStack<C>,
    stack: Stack<C>,
    upvalue_stack: Vec<Value<C>>,
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
        }
    }

    pub fn view(&mut self) -> RuntimeView<C> {
        let Runtime {
            chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
        } = self;

        let frames = frames.view();
        let stack = StackView::new(stack);
        let upvalue_stack = UpvalueView::new(upvalue_stack);

        RuntimeView {
            chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
        }
    }
}

pub struct RuntimeView<'rt, C> {
    pub chunk_cache: &'rt mut C,
    pub global_env: &'rt Value<C>,
    frames: FrameStackView<'rt, C>,
    pub stack: StackView<'rt, C>,
    upvalue_stack: UpvalueView<'rt, C>,
}

impl<'rt, C> RuntimeView<'rt, C> {
    pub fn view(&mut self, start: StackSlot) -> Result<RuntimeView<C>, RuntimeError<C>> {
        use crate::error::OutOfBoundsStack;

        let RuntimeView {
            chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
        } = self;

        let frames = frames.view();
        let start = stack.boundary() + start;
        let stack = stack.view(start).ok_or(OutOfBoundsStack)?;
        let upvalue_stack = upvalue_stack.view_over();

        let r = RuntimeView {
            chunk_cache: *chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
        };

        Ok(r)
    }

    pub fn invoke(&mut self, f: impl LuaFfiOnce<C>) -> Result<(), RuntimeError<C>> {
        f.call_once(self.view(StackSlot(0)).unwrap())
    }

    pub fn invoke_at(
        &mut self,
        f: impl LuaFfiOnce<C>,
        start: StackSlot,
    ) -> Result<(), RuntimeError<C>> {
        f.call_once(self.view(start)?)
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
        self.upvalue_stack.clear();
        self.frames.clear();
    }
}

impl<'rt, C> RuntimeView<'rt, C>
where
    C: ChunkCache<ChunkId>,
{
    pub fn enter(&mut self, closure: ClosureRef, start: StackSlot) -> Result<(), RuntimeError<C>> {
        use crate::value::callable::Callable;

        let frame = closure.construct_frame(self, start)?;
        let mut active_frame = frame.activate(self)?;

        loop {
            match active_frame.step() {
                Ok(ControlFlow::Break(ChangeFrame::Return(slot))) => {
                    active_frame.exit(slot)?;
                    tracing::trace!(stack = ?self.stack, "adjusted stack upon function return");

                    let Some(frame) = self.frames.pop() else {
                        break;
                    };

                    active_frame = frame.activate(self)?;
                }
                Ok(ControlFlow::Break(ChangeFrame::Invoke(callable, start))) => {
                    let frame = active_frame.suspend();
                    self.frames.push(frame);

                    match callable {
                        Callable::LuaClosure(closure) => {
                            let frame = closure.construct_frame(self, start)?;
                            active_frame = frame.activate(self)?;
                        }
                        Callable::RustClosure(closure) => {
                            self.invoke_at(closure, start)?;

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

    pub fn backtrace(&self) -> Backtrace {
        let frames = self
            .frames
            .iter()
            .map(|frame| frame.backtrace(self.chunk_cache))
            .collect();

        Backtrace { frames }
    }
}
