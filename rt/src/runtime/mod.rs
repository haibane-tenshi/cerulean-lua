mod frame;
mod stack;

use std::fmt::Debug;
use std::ops::ControlFlow;

use repr::value::Value;

use crate::chunk_cache::{ChunkCache, FunctionPtr};
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
    chunk_cache: &'rt mut C,
    frames: FrameStackView<'rt>,
    stack: StackView<'rt>,
}

impl<'rt, C> RuntimeView<'rt, C>
where
    C: ChunkCache,
{
    fn activate_frame(&mut self) -> Option<ActiveFrame> {
        let frame = self.frames.pop()?;
        frame.activate(self).ok()
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        use crate::chunk_cache::ChunkId;
        use repr::index::FunctionId;
        use stack::ProtectedSize;

        // For now just enter the first chunk.
        // Need to be fixed later when we get Lua-from-Rust FFI working.
        let frame = Frame {
            function_ptr: FunctionPtr {
                chunk_id: ChunkId(0),
                function_id: FunctionId(0),
            },
            ip: Default::default(),
            stack_start: ProtectedSize(0),
        };

        let Ok(mut active_frame) = frame.activate(self) else {
            return Ok(())
        };

        loop {
            match active_frame.step()? {
                ControlFlow::Break(ChangeFrame::Return(slot)) => {
                    active_frame.exit(slot)?;
                    tracing::trace!(stack = ?self.stack, "adjusted stack upon function return");

                    active_frame = match self.activate_frame() {
                        Some(t) => t,
                        None => break,
                    };
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

                    active_frame = frame.activate(self)?;
                }
                ControlFlow::Continue(()) => (),
            }
        }

        Ok(())
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
