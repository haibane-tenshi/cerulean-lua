pub(crate) mod frame;
pub(crate) mod stack;
pub(crate) mod upvalue_register;

use crate::chunk_cache::ChunkCache;
use crate::error::RtError;
use crate::ffi::delegate::Response;
use crate::ffi::DLuaFfi;
use crate::gc::Heap;
use crate::value::{Callable, CoreTypes, Strong};

use super::orchestrator::{Context as OrchestratorContext, ThreadId, ThreadStore};
use super::{Closure, Core};
use frame::{Context as FrameContext, DelegateThreadControl, Frame, FrameControl};
use stack::{RawStackSlot, Stack, StackGuard};
use upvalue_register::UpvalueRegister;

pub(crate) struct Context<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(crate) core: &'a mut Core<Ty>,
    pub(crate) chunk_cache: &'a mut dyn ChunkCache,
    pub(crate) current_thread_id: ThreadId,
    pub(crate) thread_store: &'a mut ThreadStore<Ty>,
    pub(crate) upvalue_cache: &'a mut UpvalueRegister<Ty>,
}

impl<'a, Ty> Context<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn reborrow(&mut self) -> Context<'_, Ty> {
        let Context {
            core,
            chunk_cache,
            current_thread_id,
            thread_store,
            upvalue_cache,
        } = self;

        Context {
            core: *core,
            chunk_cache: *chunk_cache,
            current_thread_id: *current_thread_id,
            thread_store,
            upvalue_cache,
        }
    }

    pub(crate) fn frame_context<'b>(
        &'b mut self,
        stack: &'b mut Stack<Ty>,
    ) -> FrameContext<'b, Ty> {
        let Context {
            core,
            chunk_cache,
            current_thread_id,
            thread_store,
            upvalue_cache,
        } = self;

        FrameContext {
            core,
            chunk_cache: *chunk_cache,
            current_thread_id: *current_thread_id,
            thread_store,
            upvalue_cache,
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

    /// Denotes range `0..protected` where stack is protected and cannot be accessed externally.
    ///
    /// # Invariants
    ///
    /// ```rust,ignore
    /// assert!(protected <= stack.len());
    /// ```
    protected: RawStackSlot,
}

impl<Ty> Thread<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn activate<'a>(&'a mut self, ctx: Context<'a, Ty>) -> ActiveThread<'a, Ty> {
        let Thread {
            frames,
            stack,
            protected,
        } = self;

        ActiveThread {
            ctx,
            frames,
            stack,
            protected,
        }
    }

    pub(crate) fn stack(&mut self) -> StackGuard<'_, Ty> {
        self.stack.guard(self.protected).unwrap()
    }
}

#[derive(Debug, Clone, Copy)]
enum Prompt {
    Resume,
    Evaluated,
}

impl<Ty> From<Prompt> for Response<Ty>
where
    Ty: CoreTypes,
{
    fn from(value: Prompt) -> Self {
        match value {
            Prompt::Resume => Response::Resume,
            Prompt::Evaluated => Response::Evaluated(Ok(())),
        }
    }
}

impl<Ty> TryFrom<Response<Ty>> for Prompt
where
    Ty: CoreTypes,
{
    type Error = RtError<Ty>;

    fn try_from(value: Response<Ty>) -> Result<Self, Self::Error> {
        match value {
            Response::Resume => Ok(Prompt::Resume),
            Response::Evaluated(Ok(())) => Ok(Prompt::Evaluated),
            Response::Evaluated(Err(err)) => Err(err),
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
    protected: &'a mut RawStackSlot,
}

impl<'a, Ty> ActiveThread<'a, Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(crate) fn enter(&mut self, response: Response<Ty>) -> Result<ThreadControl, RtError<Ty>> {
        let mut prompt = match response.try_into() {
            Ok(prompt) => prompt,
            Err(err) => match self.unwind(err)? {
                Control::Frame(prompt) => prompt,
                Control::Thread(ctrl) => return Ok(ctrl),
            },
        };

        let r = loop {
            let Some(frame) = self.frames.last_mut() else {
                *self.protected = Default::default();
                break Ok(ThreadControl::Return);
            };

            let ctx = self.ctx.frame_context(self.stack);
            let request = frame.enter(ctx, prompt);

            let result = match request {
                Ok(ctrl) => match self.process_control(ctrl) {
                    Ok(Control::Frame(prompt)) => Ok(prompt),
                    Ok(Control::Thread(ctrl)) => break Ok(ctrl),
                    Err(err) => Err(err),
                },
                Err(err) => Err(err),
            };

            prompt = match result {
                Ok(state) => state,
                Err(err) => match self.unwind(err)? {
                    Control::Frame(prompt) => prompt,
                    Control::Thread(ctrl) => break Ok(ctrl),
                },
            }
        };

        // Ensure that stack is properly synced before exiting.
        // Suspended frames are expected to not cause trouble.
        self.stack.sync(&mut self.ctx.core.gc);

        r
    }

    fn process_control(
        &mut self,
        ctrl: Control<FrameControl<Ty>, DelegateThreadControl>,
    ) -> Result<Control<Prompt, ThreadControl>, RtError<Ty>> {
        use crate::error::OutOfBoundsStack;

        match ctrl {
            Control::Frame(ctrl) => self.modify_call_stack(ctrl).map(Control::Frame),
            Control::Thread(ctrl) => {
                let (ctrl, start) = ctrl.into_thread_control();

                if start > self.stack.len() {
                    return Err(OutOfBoundsStack.into());
                }

                *self.protected = start;
                Ok(Control::Thread(ctrl))
            }
        }
    }

    fn modify_call_stack(&mut self, ctrl: FrameControl<Ty>) -> Result<Prompt, RtError<Ty>> {
        match ctrl {
            FrameControl::Return => {
                self.frames.pop();

                Ok(Prompt::Evaluated)
            }
            FrameControl::InitAndEnter {
                event,
                callable,
                start,
            } => {
                use crate::error::OutOfBoundsStack;

                let stack = self.stack.guard(start).ok_or(OutOfBoundsStack)?;
                let frame = Frame::new(
                    callable,
                    event,
                    &self.ctx.core.gc,
                    self.ctx.chunk_cache,
                    stack,
                )?;

                self.frames.push(frame);

                Ok(Prompt::Resume)
            }
        }
    }

    #[inline(never)]
    fn unwind(
        &mut self,
        mut error: RtError<Ty>,
    ) -> Result<Control<Prompt, ThreadControl>, RtError<Ty>> {
        // Unwinding loop.
        // One problem here is that we actually invoke Rust frames in the process,
        // which may leave us with more requests to process,
        // which may fail and cause us to unwind again.
        // This situation is unlikely and I'm not sure if this is the best way to approach it.
        loop {
            let ctrl = self.propagate_error(error)?;
            match self.process_control(ctrl) {
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
    fn propagate_error(
        &mut self,
        mut error: RtError<Ty>,
    ) -> Result<Control<FrameControl<Ty>, DelegateThreadControl>, RtError<Ty>> {
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

    pub(super) fn stack(&mut self) -> StackGuard<'_, Ty> {
        self.stack.full_guard()
    }
}

impl<Ty> ThreadImpetus<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(super) fn init(self, ctx: OrchestratorContext<Ty>) -> Result<Thread<Ty>, RtError<Ty>> {
        let ThreadImpetus {
            first_callable,
            mut stack,
        } = self;

        let frame = Frame::new(
            first_callable,
            None,
            &ctx.core.gc,
            ctx.chunk_cache,
            stack.full_guard(),
        )?;
        let protected = stack.len();

        let r = Thread {
            frames: vec![frame],
            stack,
            protected,
        };

        Ok(r)
    }
}

pub(super) enum Control<F, T> {
    Frame(F),
    Thread(T),
}

pub(crate) enum ThreadControl {
    Resume { thread: ThreadId },
    Yield,
    Return,
}
