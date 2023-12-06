mod frame;
mod stack;
mod upvalue_stack;

use std::fmt::Debug;
use std::ops::ControlFlow;

use repr::index::StackSlot;

use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::RuntimeError;
use crate::ffi::LuaFfiOnce;
use crate::value::Value;
use frame::{ChangeFrame, Frame};
use stack::{Stack, StackView};
use upvalue_stack::UpvalueView;

pub use frame::{Closure, ClosureRef, FunctionPtr};

pub struct Runtime<C> {
    pub chunk_cache: C,
    pub global_env: Value<C>,
    frames: Vec<Frame<C>>,
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

        let frames = FrameStackView::new(frames);
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
    pub fn view(&mut self, start: StackSlot) -> Result<RuntimeView<C>, RuntimeError> {
        let RuntimeView {
            chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
        } = self;

        let frames = frames.view();
        let start = stack.protected_size() + start;
        let stack = stack.view(start).ok_or(RuntimeError::CatchAll)?;
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

    pub fn invoke(&mut self, f: impl LuaFfiOnce<C>) -> Result<(), RuntimeError> {
        f.call_once(self.view(StackSlot(0)).unwrap())
    }

    pub fn invoke_at(
        &mut self,
        f: impl LuaFfiOnce<C>,
        start: StackSlot,
    ) -> Result<(), RuntimeError> {
        f.call_once(self.view(start)?)
    }
}

impl<'rt, C> RuntimeView<'rt, C>
where
    C: ChunkCache<ChunkId>,
{
    pub fn enter(&mut self, closure: ClosureRef, start: StackSlot) -> Result<(), RuntimeError> {
        use crate::value::callable::Callable;

        let frame = closure.construct_frame(self, start)?;
        let mut active_frame = frame.activate(self)?;

        loop {
            match active_frame.step()? {
                ControlFlow::Break(ChangeFrame::Return(slot)) => {
                    active_frame.exit(slot)?;
                    tracing::trace!(stack = ?self.stack, "adjusted stack upon function return");

                    let Some(frame) = self.frames.pop() else {
                        break;
                    };

                    active_frame = frame.activate(self)?;
                }
                ControlFlow::Break(ChangeFrame::Invoke(start)) => {
                    let frame = active_frame.suspend();
                    self.frames.push(frame);

                    // It is extremely annoying to keep callable on the stack (either caller or callee),
                    // however this causes stack adjustments on every single fn call.
                    // I would like to implement it differently,
                    // but somewhat frustratingly this seems to be the simplest workable option.
                    // The only other approach I can think of is constructing dedicated callable *stack*
                    // (single-value register doesn't work due to nested calls)
                    // and make fn invocation into two instructions: StoreCallable + Invoke.
                    // Not sure if it will work better.
                    let Some(Value::Function(callable)) = self.stack.remove(start) else {
                        return Err(RuntimeError::CatchAll);
                    };

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
                ControlFlow::Continue(()) => (),
            }
        }

        Ok(())
    }

    pub fn construct_closure(
        &mut self,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = Value<C>>,
    ) -> Result<Closure, RuntimeError> {
        let signature = &self
            .chunk_cache
            .chunk(fn_ptr.chunk_id)
            .ok_or(RuntimeError::CatchAll)?
            .get_function(fn_ptr.function_id)
            .ok_or(RuntimeError::CatchAll)?
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

struct FrameStackView<'a, C> {
    frames: &'a mut Vec<Frame<C>>,
    protected_size: usize,
}

impl<'a, C> FrameStackView<'a, C> {
    fn new(frames: &'a mut Vec<Frame<C>>) -> Self {
        FrameStackView {
            frames,
            protected_size: 0,
        }
    }

    fn view(&mut self) -> FrameStackView<C> {
        let protected_size = self.frames.len();

        FrameStackView {
            frames: self.frames,
            protected_size,
        }
    }

    fn pop(&mut self) -> Option<Frame<C>> {
        if self.frames.len() <= self.protected_size {
            return None;
        }

        self.frames.pop()
    }

    fn push(&mut self, frame: Frame<C>) {
        self.frames.push(frame)
    }
}
