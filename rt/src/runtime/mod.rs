mod frame;
mod stack;
mod upvalue_stack;

use std::fmt::Debug;
use std::ops::ControlFlow;

use repr::index::StackSlot;
use repr::value::Value as ReprValue;

use crate::chunk_cache::ChunkCache;
use crate::ffi::LuaFfiOnce;
use crate::RuntimeError;
use frame::{ChangeFrame, Frame};
use stack::{Stack, StackView};
use upvalue_stack::UpvalueView;

pub use frame::{Closure, ClosureRef, FunctionPtr};

pub type Value = ReprValue<ClosureRef>;

impl From<Closure> for Value {
    fn from(value: Closure) -> Self {
        ClosureRef::new(value).into()
    }
}

impl From<ClosureRef> for Value {
    fn from(value: ClosureRef) -> Self {
        Value::Function(value)
    }
}

pub struct Runtime<C> {
    chunk_cache: C,
    global_env: Value,
    frames: Vec<Frame>,
    stack: Stack,
    upvalue_stack: Vec<Value>,
}

impl<C> Runtime<C>
where
    C: Debug,
{
    pub fn new(chunk_cache: C, global_env: Value) -> Self {
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
    pub global_env: &'rt Value,
    frames: FrameStackView<'rt>,
    pub stack: StackView<'rt>,
    upvalue_stack: UpvalueView<'rt>,
}

impl<'rt, C> RuntimeView<'rt, C>
where
    C: ChunkCache,
{
    fn view(&mut self) -> RuntimeView<C> {
        let RuntimeView {
            chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
        } = self;

        let frames = frames.view();
        let stack = stack.view_over();
        let upvalue_stack = upvalue_stack.view_over();

        RuntimeView {
            chunk_cache,
            global_env,
            frames,
            stack,
            upvalue_stack,
        }
    }

    pub fn enter(&mut self, closure: ClosureRef, start: StackSlot) -> Result<(), RuntimeError> {
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
                ControlFlow::Break(ChangeFrame::Invoke(closure, start)) => {
                    let frame = active_frame.suspend();
                    self.frames.push(frame);

                    let frame = closure.construct_frame(self, start)?;
                    active_frame = frame.activate(self)?;
                }
                ControlFlow::Continue(()) => (),
            }
        }

        Ok(())
    }

    pub fn construct_closure(
        &mut self,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = Value>,
    ) -> Result<Closure, RuntimeError> {
        let signature = &self
            .chunk_cache
            .chunk(fn_ptr.chunk_id)
            .ok_or(RuntimeError)?
            .get_function(fn_ptr.function_id)
            .ok_or(RuntimeError)?
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

    pub fn invoke(&mut self, f: impl LuaFfiOnce<C>) -> Result<(), RuntimeError> {
        f.call_once(self.view())
    }
}

struct FrameStackView<'a> {
    frames: &'a mut Vec<Frame>,
    protected_size: usize,
}

impl<'a> FrameStackView<'a> {
    fn new(frames: &'a mut Vec<Frame>) -> Self {
        FrameStackView {
            frames,
            protected_size: 0,
        }
    }

    fn view(&mut self) -> FrameStackView {
        let protected_size = self.frames.len();

        FrameStackView {
            frames: self.frames,
            protected_size,
        }
    }

    fn pop(&mut self) -> Option<Frame> {
        if self.frames.len() <= self.protected_size {
            return None;
        }

        self.frames.pop()
    }

    fn push(&mut self, frame: Frame) {
        self.frames.push(frame)
    }
}
