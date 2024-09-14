use std::pin::Pin;

use crate::chunk_cache::ChunkCache;
use crate::error::RtError;
use crate::ffi::delegate::{Delegate as RustDelegate, Response};
use crate::ffi::{DLuaFfi, LuaFfi};
use crate::value::{Callable, CoreTypes, Strong, Value, Weak};

use super::frame::{Context as FrameContext, Frame as LuaFrame};
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
    fn enter(
        &mut self,
        ctx: RuntimeView<Ty>,
        response: Response<Ty>,
    ) -> Result<FrameControl<Ty>, RtError<Ty>> {
        use crate::ffi::coroutine::State;
        use crate::ffi::delegate::Request;

        let boundary = ctx.stack.boundary();

        match self.delegate.as_mut().resume((ctx, response)) {
            State::Complete(Ok(())) => Ok(FrameControl::Pop),
            State::Complete(Err(err)) => Err(err),
            State::Yielded(request) => match request {
                Request::Invoke { callable, start } => {
                    let start = boundary + start;
                    let r = FrameControl::Push {
                        event: None,
                        callable,
                        start,
                    };

                    Ok(r)
                }
            },
        }
    }
}

impl<Ty> LuaFrame<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
{
    fn enter(&mut self, ctx: FrameContext<Ty>) -> Result<FrameControl<Ty>, RtError<Ty>> {
        use super::frame::ChangeFrame;

        let boundary = ctx.stack.boundary();
        let mut active_frame = self.activate(ctx)?;

        let r = match active_frame.enter()? {
            ChangeFrame::Return(slot) => {
                active_frame.exit(slot);

                FrameControl::Pop
            }
            ChangeFrame::Invoke(event, callable, start) => {
                // Ensure that stack space passed to another function no longer hosts upvalues.
                // active_frame.stack.lua_frame().evict_upvalues();

                let start = boundary + start;
                FrameControl::Push {
                    event,
                    callable,
                    start,
                }
            }
        };

        Ok(r)
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

impl<Ty> Frame<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
{
    fn enter(
        &mut self,
        mut ctx: Context<Ty>,
        stack: &mut Stack<Ty>,
        resume: ThreadState,
    ) -> Result<FrameControl<Ty>, RtError<Ty>> {
        let Frame {
            implementor,
            stack_start,
            event,
        } = self;

        let stack_start = *stack_start;

        let mut stack = stack
            .guard(stack_start)
            .expect("thread should preserve stack space of suspended frames");

        let result = match implementor {
            Implementor::Lua(frame) => {
                let context = ctx.frame_context(stack.reborrow());
                frame.enter(context)
            }
            Implementor::Rust(frame) => {
                let rt = ctx.runtime_view(stack.reborrow());
                frame.enter(rt, resume.into())
            }
        }?;

        if matches!(result, FrameControl::Pop) {
            if let Some(event) = *event {
                stack.lua_frame().adjust_event_returns(event);
            }
        }

        Ok(result)
    }
}

pub(crate) struct Thread<Ty>
where
    Ty: CoreTypes,
{
    frames: Vec<Frame<Ty>>,
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

        let frames = vec![frame];

        let r = Thread { frames, stack };

        Ok(r)
    }

    pub(crate) fn activate<'a>(&'a mut self, ctx: Context<'a, Ty>) -> ActiveThread<'a, Ty> {
        let Thread { frames, stack } = self;

        ActiveThread { ctx, frames, stack }
    }
}

enum FrameControl<Ty>
where
    Ty: CoreTypes,
{
    Pop,
    Push {
        callable: Callable<Strong, Ty>,
        start: RawStackSlot,
        event: Option<Event>,
    },
}

#[derive(Debug, Clone, Copy)]
enum ThreadState {
    Resume,
    Evaluated,
}

impl<Ty> From<ThreadState> for Response<Ty>
where
    Ty: CoreTypes,
{
    fn from(value: ThreadState) -> Self {
        match value {
            ThreadState::Resume => Response::Resume,
            ThreadState::Evaluated => Response::Evaluated(Ok(())),
        }
    }
}

pub(crate) struct ActiveThread<'a, Ty>
where
    Ty: CoreTypes,
{
    ctx: Context<'a, Ty>,
    frames: &'a mut Vec<Frame<Ty>>,
    stack: &'a mut Stack<Ty>,
}

impl<'a, Ty> ActiveThread<'a, Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(crate) fn enter(&mut self) -> Result<(), RtError<Ty>> {
        let mut state = ThreadState::Resume;

        loop {
            let Some(frame) = self.frames.last_mut() else {
                // TODO: I believe Lua expects this to be an error instead.
                break Ok(());
            };

            let request = frame.enter(self.ctx.reborrow(), self.stack, state);

            let result = match request {
                Ok(ctrl) => self.modify_call_stack(ctrl),
                Err(err) => Err(err),
            };

            state = match result {
                Ok(state) => state,
                Err(err) => self.unwind(err)?,
            }
        }
    }

    fn modify_call_stack(&mut self, ctrl: FrameControl<Ty>) -> Result<ThreadState, RtError<Ty>> {
        use crate::error::OutOfBoundsStack;

        match ctrl {
            FrameControl::Pop => {
                self.frames.pop();

                Ok(ThreadState::Evaluated)
            }
            FrameControl::Push {
                event,
                callable,
                start,
            } => {
                let stack = self.stack.guard(start).ok_or(OutOfBoundsStack)?;

                let stack_start = stack.boundary();
                let ctx = self.ctx.frame_context(stack);
                let implementor = Implementor::new(callable, ctx)?;
                let frame = Frame {
                    implementor,
                    stack_start,
                    event,
                };

                self.frames.push(frame);

                Ok(ThreadState::Resume)
            }
        }
    }

    #[inline(never)]
    fn unwind(&mut self, mut error: RtError<Ty>) -> Result<ThreadState, RtError<Ty>> {
        // Unwinding loop.
        // One problem here is that we actually invoke Rust frames in the process,
        // which may leave us with more requests to process,
        // which may fail and cause us to unwind again.
        // This situation is unlikely and I'm not sure if this is the best way to approach it.
        loop {
            let ctrl = self.propagate_error(error)?;
            match self.modify_call_stack(ctrl) {
                Ok(state) => break Ok(state),
                Err(err) => {
                    error = err;
                }
            };
        }
    }
}

impl<'a, Ty> ActiveThread<'a, Ty>
where
    Ty: CoreTypes,
{
    /// Propagate runtime error up the call stack.
    ///
    /// This function will give an opportunity to every frame to handle the error.
    ///
    /// Lua frames inherently don't have an ability to interact with panic mechanism,
    /// therefore the error is simply propagated intact.
    ///
    /// Rust frames are resumed and their response is determined by the output.
    /// Receiving `State::Completed(Err(_))` indicates that frame wants to propagate panic (using the new error value).
    /// Receiving any other result indicates that error was handled and execution should continue from this point as normal.
    ///
    /// Upon successfully handling the error this function will clean up the call stack before returning.
    /// Otherwise call stack will be left intact.
    ///
    /// Temporaries' stack is always cleaned up since it is required to be brought into correct state for delegate to execution.
    ///
    /// # Returns
    ///
    /// On success function returns request made by the frame that handled the error.
    ///
    /// On failure function the error object (possibly transformed by Rust frames during propagation).
    fn propagate_error(&mut self, mut error: RtError<Ty>) -> Result<FrameControl<Ty>, RtError<Ty>> {
        if self.frames.is_empty() {
            return Err(error);
        }

        let mut iter = self.frames.iter_mut().enumerate().rev();
        let upper_bound = iter.next().unwrap().1.stack_start;
        let mut iter = iter.scan(upper_bound, |upper_bound, (i, frame)| {
            let r = *upper_bound;
            *upper_bound = frame.stack_start;
            Some((i, frame, r))
        });

        loop {
            let Some((i, frame, upper_bound)) = iter.next() else {
                break Err(error);
            };

            let Implementor::Rust(rust_frame) = &mut frame.implementor else {
                continue;
            };

            let mut stack = self.stack.guard(frame.stack_start).unwrap();
            // Clear portion of the stack belonging to other functions.
            stack.truncate(upper_bound - frame.stack_start);
            let ctx = self.ctx.runtime_view(stack);

            match rust_frame.enter(ctx, Response::Evaluated(Err(error))) {
                Ok(request) => {
                    self.frames.truncate(i + 1);
                    break Ok(request);
                }
                Err(err) => {
                    error = err;
                }
            }
        }
    }
}
