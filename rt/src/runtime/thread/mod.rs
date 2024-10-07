pub(crate) mod frame;
pub(crate) mod stack;

use crate::chunk_cache::ChunkCache;
use crate::error::RtError;
use crate::ffi::delegate::Response;
use crate::ffi::DLuaFfi;
use crate::gc::Heap;
use crate::value::{Callable, CoreTypes, Strong};

use super::{Closure, Core};
use frame::{Context as FrameContext, Frame, FrameControl};
use stack::{RawStackSlot, Stack};

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
        stack: &'b mut Stack<Ty>,
    ) -> FrameContext<'b, Ty> {
        let Context { core, chunk_cache } = self;

        FrameContext {
            core,
            chunk_cache: *chunk_cache,
            stack,
        }
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
    Ty: CoreTypes,
{
    pub(crate) fn activate<'a>(&'a mut self, ctx: Context<'a, Ty>) -> ActiveThread<'a, Ty> {
        let Thread { frames, stack } = self;

        ActiveThread { ctx, frames, stack }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ThreadState {
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

            let ctx = self.ctx.frame_context(self.stack);
            let request = frame.enter(ctx, state);

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
                let ctx = self.ctx.frame_context(self.stack);
                let frame = Frame::new(callable, start, event, ctx)?;

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
        // The last frame is the one that invoked the panic.
        // We need to skip it.
        let upper_bound = iter.next().unwrap().1.stack_start();
        let mut iter = iter.scan(upper_bound, |upper_bound, (i, frame)| {
            let r = *upper_bound;
            *upper_bound = frame.stack_start();
            Some((i, frame, r))
        });

        loop {
            let Some((i, frame, upper_bound)) = iter.next() else {
                break Err(error);
            };

            // Clear portion of the stack belonging to other functions.
            self.stack.truncate(upper_bound);

            let ctx = self.ctx.frame_context(self.stack);
            match frame.process_error(ctx, error) {
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

pub(super) struct ThreadImpetus<Ty>
where
    Ty: CoreTypes,
{
    first_callable: Callable<Strong, Ty>,
    stack: Stack<Ty>,
}

impl<Ty> ThreadImpetus<Ty>
where
    Ty: CoreTypes,
{
    pub(super) fn new(callable: Callable<Strong, Ty>, heap: &mut Heap<Ty>) -> Self {
        ThreadImpetus {
            first_callable: callable,
            stack: Stack::new(heap),
        }
    }
}

impl<Ty> ThreadImpetus<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(super) fn init(self, mut ctx: Context<Ty>) -> Result<Thread<Ty>, RtError<Ty>> {
        let ThreadImpetus {
            first_callable,
            mut stack,
        } = self;

        let ctx = ctx.frame_context(&mut stack);
        let frame = Frame::new(first_callable, RawStackSlot::default(), None, ctx)?;

        let r = Thread {
            frames: vec![frame],
            stack,
        };

        Ok(r)
    }
}
