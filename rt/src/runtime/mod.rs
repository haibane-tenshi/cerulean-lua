mod frame;
mod stack;

use std::fmt::Debug;
use std::ops::ControlFlow;

use repr::index::StackSlot;
use repr::value::Value;

use crate::chunk_cache::{ChunkCache, FunctionPtr};
use crate::ffi::Ffi;
use crate::RuntimeError;
use frame::{ActiveFrame, ChangeFrame, Frame};
use stack::StackView;

pub struct Runtime<C> {
    chunk_cache: C,
    frames: Vec<Frame>,
    stack: Vec<Value>,
}

impl<C> Runtime<C>
where
    C: Debug,
{
    pub fn new(chunk_cache: C) -> Self {
        tracing::trace!(?chunk_cache, "constructed runtime");

        Runtime {
            chunk_cache,
            frames: Default::default(),
            stack: Default::default(),
        }
    }

    pub fn view(&mut self) -> RuntimeView<C> {
        let Runtime {
            chunk_cache,
            frames,
            stack,
        } = self;

        let frames = FrameStackView::new(frames);
        let stack = StackView::new(stack);

        RuntimeView {
            chunk_cache,
            frames,
            stack,
        }
    }
}

pub struct RuntimeView<'rt, C> {
    pub chunk_cache: &'rt mut C,
    frames: FrameStackView<'rt>,
    pub stack: StackView<'rt>,
}

impl<'rt, C> RuntimeView<'rt, C>
where
    C: ChunkCache,
{
    fn view(&mut self) -> RuntimeView<C> {
        let RuntimeView {
            chunk_cache,
            frames,
            stack,
        } = self;

        let frames = frames.view();
        let stack = stack.view_over();

        RuntimeView {
            chunk_cache,
            frames,
            stack,
        }
    }

    pub fn make_frame(&self, ptr: FunctionPtr, offset: StackSlot) -> Frame {
        let stack_start = self.stack.protected_size() + offset;

        Frame {
            function_ptr: ptr,
            ip: Default::default(),
            stack_start,
        }
    }

    pub fn activate(&mut self, frame: Frame) -> Result<ActiveFrame, RuntimeError>
    where
        C: ChunkCache,
    {
        let RuntimeView {
            chunk_cache, stack, ..
        } = self;

        let Frame {
            function_ptr,
            ip,
            stack_start,
        } = frame;

        let chunk = chunk_cache
            .chunk(function_ptr.chunk_id)
            .ok_or(RuntimeError)?;
        let function = chunk
            .get_function(function_ptr.function_id)
            .ok_or(RuntimeError)?;

        let constants = &chunk.constants;
        let opcodes = &function.codes;
        let stack = stack.view(stack_start).unwrap();

        let r = ActiveFrame {
            function_ptr,
            constants,
            opcodes,
            ip,
            stack,
        };

        Ok(r)
    }

    pub fn enter(&mut self, frame: Frame) -> Result<(), RuntimeError> {
        let mut active_frame = self.activate(frame)?;

        loop {
            match active_frame.step()? {
                ControlFlow::Break(ChangeFrame::Return(slot)) => {
                    active_frame.exit(slot)?;
                    tracing::trace!(stack = ?self.stack, "adjusted stack upon function return");

                    let Some(frame) = self.frames.pop() else {
                        break
                    };

                    active_frame = self.activate(frame)?;
                }
                ControlFlow::Break(ChangeFrame::Invoke(function_id, slot)) => {
                    let frame = active_frame.suspend();
                    let chunk_id = frame.function_ptr.chunk_id;
                    let stack_start = frame.stack_start + slot;

                    self.frames.push(frame);

                    let function_ptr = FunctionPtr {
                        chunk_id,
                        function_id,
                    };

                    let frame = Frame {
                        function_ptr,
                        ip: Default::default(),
                        stack_start,
                    };

                    active_frame = self.activate(frame)?;
                }
                ControlFlow::Continue(()) => (),
            }
        }

        Ok(())
    }

    pub fn invoke(&mut self, f: impl Ffi<C>) -> Result<(), RuntimeError> {
        f.call(self.view())
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
