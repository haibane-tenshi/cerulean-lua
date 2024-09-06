use std::pin::Pin;

use repr::index::StackSlot;

use crate::chunk_cache::ChunkCache;
use crate::error::RtError;
use crate::ffi::delegate::Delegate as RustDelegate;
use crate::ffi::{DLuaFfi, LuaFfi};
use crate::value::{Callable, CoreTypes, Strong, Value, Weak};

use super::frame::{Context as FrameContext, Frame as LuaFrame};
use super::frame_stack::FrameStack;
use super::stack::{RawStackSlot, Stack, StackGuard};
use super::{Closure, Core, Event, RuntimeView};

pub(crate) struct Context<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(crate) core: &'a mut Core<Ty>,
    pub(crate) chunk_cache: &'a mut dyn ChunkCache,
}

impl<'a, Ty> Context<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn reborrow(&mut self) -> Context<'_, Ty> {
        let Context { core, chunk_cache } = self;

        Context {
            core: *core,
            chunk_cache: *chunk_cache,
        }
    }

    pub(crate) fn frame_context<'b>(
        &'b mut self,
        stack: StackGuard<'b, Ty>,
    ) -> FrameContext<'b, Ty> {
        let Context { core, chunk_cache } = self;

        FrameContext {
            core,
            chunk_cache: *chunk_cache,
            stack,
        }
    }

    pub(crate) fn runtime_view<'b>(&'b mut self, stack: StackGuard<'b, Ty>) -> RuntimeView<'b, Ty> {
        let Context { core, chunk_cache } = self;

        RuntimeView {
            core,
            chunk_cache: *chunk_cache,
            stack,
        }
    }
}

struct RustFrame<Ty>
where
    Ty: CoreTypes,
{
    delegate: Pin<Box<dyn RustDelegate<Ty>>>,
}

impl<Ty> RustFrame<Ty>
where
    Ty: CoreTypes,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    fn new(closure: &Ty::RustClosure) -> Self {
        let sequence = closure.call();

        RustFrame { delegate: sequence }
    }
}

impl<Ty> RustFrame<Ty>
where
    Ty: CoreTypes,
{
    fn enter(mut self, ctx: RuntimeView<Ty>) -> FrameRequest<Ty> {
        use crate::ffi::coroutine::State;
        use crate::ffi::delegate::Request;

        match self.delegate.as_mut().resume(ctx) {
            State::Complete(r) => match r {
                Ok(()) => FrameRequest::Return,
                Err(err) => FrameRequest::Unwind {
                    this_frame: Implementor::Rust(self),
                    err,
                },
            },
            State::Yielded(request) => match request {
                Request::Invoke { callable, start } => FrameRequest::Invoke {
                    this_frame: Implementor::Rust(self),
                    event: None,
                    callable,
                    start,
                },
            },
        }
    }
}

impl<Ty> LuaFrame<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
{
    fn enter(self, ctx: FrameContext<Ty>) -> FrameRequest<Ty> {
        use super::frame::ChangeFrame;

        let mut active_frame = match self.activate(ctx) {
            Ok(t) => t,
            Err((frame, err)) => {
                return FrameRequest::Unwind {
                    this_frame: Implementor::Lua(frame),
                    err,
                }
            }
        };

        match active_frame.enter() {
            Ok(ChangeFrame::Return(slot)) => {
                active_frame.exit(slot);

                FrameRequest::Return
            }
            Ok(ChangeFrame::Invoke(event, callable, start)) => FrameRequest::Invoke {
                this_frame: Implementor::Lua(active_frame.suspend()),
                event,
                callable,
                start,
            },
            Err(err) => FrameRequest::Unwind {
                this_frame: Implementor::Lua(active_frame.suspend()),
                err: err.into(),
            },
        }
    }
}

enum Implementor<Ty>
where
    Ty: CoreTypes,
{
    Lua(LuaFrame<Ty>),
    Rust(RustFrame<Ty>),
}

impl<Ty> Implementor<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    fn new(closure: Callable<Strong, Ty>, ctx: FrameContext<Ty>) -> Result<Self, RtError<Ty>> {
        use crate::gc::LuaPtr;

        let r = match closure {
            Callable::Lua(LuaPtr(closure)) => {
                let frame = LuaFrame::new(closure, ctx)?;
                Implementor::Lua(frame)
            }
            Callable::Rust(LuaPtr(closure)) => {
                let frame = RustFrame::new(&ctx.core.gc[&closure]);
                Implementor::Rust(frame)
            }
        };

        Ok(r)
    }
}

struct Frame<Ty>
where
    Ty: CoreTypes,
{
    implementor: Implementor<Ty>,
    stack_start: RawStackSlot,

    /// Whether frame was created as result of evaluating metamethod.
    ///
    /// Metamethods mimic builtin behavior of opcodes,
    /// therefore need cleanup to ensure correct stack state after frame is exited.
    event: Option<Event>,
}

pub(crate) struct Thread<Ty>
where
    Ty: CoreTypes,
{
    frames: FrameStack<Frame<Ty>>,
    stack: Stack<Ty>,
}

impl<Ty> Thread<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(crate) fn from_callable_with(
        callable: Callable<Strong, Ty>,
        args: impl IntoIterator<Item = Value<Weak, Ty>>,
        mut ctx: Context<Ty>,
    ) -> Result<Self, RtError<Ty>> {
        let mut stack = ctx.core.gc.pause(|heap| {
            // This call allocates, we need to protect weak references in arguments until they are rooted.
            let mut stack = Stack::new(heap);
            let mut guard = stack.full_guard();
            let mut guard = guard.lua_frame();
            guard.extend(args, false);
            guard.sync_transient(heap);

            stack
        });

        let ctx = FrameContext::new(ctx.reborrow(), stack.full_guard());
        let kind = Implementor::new(callable, ctx)?;
        let frame = Frame {
            implementor: kind,
            stack_start: RawStackSlot::default(),
            event: None,
        };

        let mut frames = FrameStack::default();
        frames.push(frame);

        let r = Thread { frames, stack };

        Ok(r)
    }

    pub(crate) fn enter(&mut self, mut ctx: Context<Ty>) -> Result<(), RtError<Ty>> {
        use crate::error::OutOfBoundsStack;

        loop {
            let Some(frame) = self.frames.pop() else {
                // TODO: I believe Lua expects this to be an error instead.
                break Ok(());
            };

            let Frame {
                implementor,
                stack_start,
                event,
            } = frame;

            let mut stack = self
                .stack
                .guard(stack_start)
                .expect("thread should preserve stack space of suspended frames");

            // Poll the frame.
            let request = match implementor {
                Implementor::Lua(frame) => {
                    let context = ctx.frame_context(stack.reborrow());
                    frame.enter(context)
                }
                Implementor::Rust(frame) => {
                    let rt = ctx.runtime_view(stack.reborrow());
                    frame.enter(rt)
                }
            };

            // Process requests.
            let result = (|| {
                match request {
                    FrameRequest::Return => {
                        if let Some(event) = event {
                            stack.lua_frame().adjust_event_returns(event);
                        }

                        Ok(())
                    }
                    FrameRequest::Invoke {
                        this_frame,
                        event,
                        callable,
                        start,
                    } => {
                        let frame = Frame {
                            implementor: this_frame,
                            stack_start,
                            event,
                        };
                        self.frames.push(frame);

                        let mut stack = stack.guard_at(start).ok_or(OutOfBoundsStack)?;
                        // Ensure that stack space passed to another function no longer hosts upvalues.
                        stack.lua_frame().evict_upvalues();

                        let stack_start = stack.boundary();
                        let ctx = ctx.frame_context(stack);
                        let implementor = Implementor::new(callable, ctx)?;
                        let frame = Frame {
                            implementor,
                            stack_start,
                            event,
                        };

                        self.frames.push(frame);

                        Ok(())
                    }
                    FrameRequest::Unwind { this_frame, err } => {
                        let frame = Frame {
                            implementor: this_frame,
                            stack_start,
                            event,
                        };
                        self.frames.push(frame);

                        Err(err)
                    }
                }
            })();

            // Process error and unwind if necessary.
            match result {
                Ok(()) => (),
                Err(_err) => {
                    todo!("pass errors through Rust frames.");
                }
            }
        }
    }
}

enum FrameRequest<Ty>
where
    Ty: CoreTypes,
{
    Return,
    Invoke {
        this_frame: Implementor<Ty>,
        event: Option<Event>,
        callable: Callable<Strong, Ty>,
        start: StackSlot,
    },
    Unwind {
        this_frame: Implementor<Ty>,
        err: RtError<Ty>,
    },
}
