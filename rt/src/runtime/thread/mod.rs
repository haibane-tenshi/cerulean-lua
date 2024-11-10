pub(crate) mod frame;
pub(crate) mod stack;

use crate::backtrace::Backtrace;
use crate::chunk_cache::ChunkCache;
use crate::error::{RtError, ThreadError, ThreadPanicked};
use crate::ffi::delegate::Response;
use crate::ffi::DLuaFfi;
use crate::gc::Heap;
use crate::value::{Callable, Strong, Types};

use super::orchestrator::{ThreadId, ThreadStatus, ThreadStore};
use super::{Closure, Core};
use frame::{Context as FrameContext, DelegateThreadControl, Frame, FrameControl, UpvalueRegister};
use stack::{RawStackSlot, Stack, StackGuard};

pub(crate) enum Status {
    Normal,
    Finished,
    Panicked,
}

pub(crate) struct Context<'a, Ty>
where
    Ty: Types,
{
    pub(crate) core: &'a mut Core<Ty>,
    pub(crate) chunk_cache: &'a mut dyn ChunkCache,
    pub(crate) current_thread_id: ThreadId,
    pub(crate) thread_store: &'a mut ThreadStore<Ty>,
    pub(crate) upvalue_cache: &'a mut UpvalueRegister<Ty>,
}

impl<'a, Ty> Context<'a, Ty>
where
    Ty: Types,
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

impl<'a, Ty> Context<'a, Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(crate) fn eval(
        &mut self,
        thread: &mut Thread<Ty>,
        response: Response<Ty>,
    ) -> Result<ThreadControl, ThreadError> {
        let mut active = ActiveThread {
            ctx: self.reborrow(),
            thread,
        };

        active.enter(response)
    }
}

struct Errors<Ty>
where
    Ty: Types,
{
    /// Error object that triggered unwinding.
    ///
    /// We want to keep this one around, because Rust frames (and error handler) can arbitrarily modify error while processing it.
    /// It is important to preserve original in case error is propagated from another thread.
    ///
    /// In our unwinding scheme errors never cross thread boundaries,
    /// instead parent thread receives `ThreadPanicked` which contains `ThreadId` of the other thread.
    /// This way panic threads form a (singly) linked list and allows us to reconstruct entire thread stack that panicked.
    /// Unfortunately it is not possible to preserve this information in another way.
    /// Orchestrator *have* to clear out its own thread stack because after propagating error to another thread
    /// it can never be sure whether a panic was properly handled or not and
    /// whether an error from the other thread (if there is any) is part of the same panic instance.
    original: RtError<Ty>,

    /// Final error object.
    processed: RtError<Ty>,
}

pub(crate) struct Thread<Ty>
where
    Ty: Types,
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

    /// Error values the thread panicked with.
    ///
    /// This field is set to `None` for non-panicked threads.
    panicked_with: Option<Errors<Ty>>,
}

impl<Ty> Thread<Ty>
where
    Ty: Types,
{
    pub(crate) fn stack(&mut self) -> StackGuard<'_, Ty> {
        self.stack.guard(self.protected).unwrap()
    }

    pub(crate) fn status(&self) -> Status {
        if self.panicked_with.is_some() {
            Status::Panicked
        } else if self.frames.is_empty() {
            Status::Finished
        } else {
            Status::Normal
        }
    }

    /// Id of the other thread that caused this thread to panic.
    pub(crate) fn panic_origin(&self) -> Option<ThreadId> {
        use crate::error::RuntimeError;

        self.original_error().and_then(|err| {
            let RuntimeError::Thread(err) = err else {
                return None;
            };

            err.panic_origin()
        })
    }

    pub(crate) fn error(&self) -> Option<&RtError<Ty>> {
        self.panicked_with.as_ref().map(|errors| &errors.processed)
    }

    pub(crate) fn original_error(&self) -> Option<&RtError<Ty>> {
        self.panicked_with.as_ref().map(|errors| &errors.original)
    }
}

#[derive(Debug, Clone, Copy)]
enum Prompt {
    Resume,
    Evaluated,
}

impl<Ty> From<Prompt> for Response<Ty>
where
    Ty: Types,
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
    Ty: Types,
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

struct ActiveThread<'a, Ty>
where
    Ty: Types,
{
    ctx: Context<'a, Ty>,
    thread: &'a mut Thread<Ty>,
}

impl<'a, Ty> ActiveThread<'a, Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    fn enter(&mut self, response: Response<Ty>) -> Result<ThreadControl, ThreadError> {
        use crate::error::thread::ThreadStatus;

        // Check it thread is resumable.
        let dead_status = match self.thread.status() {
            Status::Normal => None,
            Status::Finished => Some(ThreadStatus::Finished),
            Status::Panicked => Some(ThreadStatus::Panicked),
        };

        if let Some(status) = dead_status {
            use crate::error::thread::ResumeDeadThread;

            let err = ResumeDeadThread {
                thread_id: self.ctx.current_thread_id,
                status,
            };

            return Err(err.into());
        }

        let mut prompt = match response.try_into() {
            Ok(prompt) => prompt,
            Err(err) => match self.unwind(err)? {
                Control::Frame(prompt) => prompt,
                Control::Thread(ctrl) => return Ok(ctrl),
            },
        };

        let r = loop {
            let Some(frame) = self.thread.frames.last_mut() else {
                self.thread.protected = Default::default();
                break Ok(ThreadControl::Return);
            };

            let ctx = self.ctx.frame_context(&mut self.thread.stack);
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
        // Suspended threads are expected to not cause trouble.
        self.thread.stack.sync(&mut self.ctx.core.gc);

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

                if start > self.thread.stack.len() {
                    return Err(OutOfBoundsStack.into());
                }

                self.thread.protected = start;
                Ok(Control::Thread(ctrl))
            }
        }
    }

    fn modify_call_stack(&mut self, ctrl: FrameControl<Ty>) -> Result<Prompt, RtError<Ty>> {
        match ctrl {
            FrameControl::Return => {
                self.thread.frames.pop();

                Ok(Prompt::Evaluated)
            }
            FrameControl::InitAndEnter {
                event,
                callable,
                start,
            } => {
                use crate::error::OutOfBoundsStack;

                let stack = self.thread.stack.guard(start).ok_or(OutOfBoundsStack)?;
                let frame = Frame::new(
                    callable,
                    event,
                    &mut self.ctx.core.gc,
                    self.ctx.chunk_cache,
                    stack,
                )?;

                self.thread.frames.push(frame);

                Ok(Prompt::Resume)
            }
        }
    }

    #[inline(never)]
    fn unwind(
        &mut self,
        mut error: RtError<Ty>,
    ) -> Result<Control<Prompt, ThreadControl>, ThreadPanicked> {
        // Unwinding loop.
        // One problem here is that we actually invoke Rust frames in the process,
        // which may leave us with more requests to process,
        // which may fail and cause us to unwind again.
        loop {
            let ctrl = match self.propagate_error(error.clone()) {
                Ok(ctrl) => ctrl,
                Err(err) => {
                    use crate::error::thread::ThreadPanicked;

                    debug_assert!(self.thread.panicked_with.is_none());

                    // Record error value inside thread and replace it with general `ThreadPanicked`.
                    let errors = Errors {
                        original: error,
                        processed: err,
                    };
                    self.thread.panicked_with = Some(errors);

                    let err = ThreadPanicked(self.ctx.current_thread_id);
                    break Err(err);
                }
            };
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
    Ty: Types,
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
    /// Temporaries' stack is always cleaned up since it is required to be brought into correct state for delegate to execute.
    ///
    /// # Returns
    ///
    /// On success returns request made by the frame that handled the error.
    ///
    /// On failure returns the error object (possibly transformed by Rust frames during propagation).
    fn propagate_error(
        &mut self,
        mut error: RtError<Ty>,
    ) -> Result<Control<FrameControl<Ty>, DelegateThreadControl>, RtError<Ty>> {
        if self.thread.frames.is_empty() {
            return Err(error);
        }

        let mut iter = self.thread.frames.iter_mut().enumerate().rev();
        // The last frame is the one that invoked panic.
        // Skip it.
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
            self.thread.stack.truncate(upper_bound);

            let ctx = self.ctx.frame_context(&mut self.thread.stack);
            match frame.process_error(ctx, error) {
                Ok(request) => {
                    self.thread.frames.truncate(i + 1);
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
    Ty: Types,
{
    first_callable: Callable<Strong, Ty>,
    stack: Stack<Ty>,
}

impl<Ty> ThreadImpetus<Ty>
where
    Ty: Types,
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
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(super) fn init(self, heap: &mut Heap<Ty>) -> Thread<Ty> {
        use crate::gc::LuaPtr;
        use crate::runtime::RuntimeView;
        use crate::value::Value;

        let ThreadImpetus {
            first_callable,
            mut stack,
        } = self;

        let frame = match first_callable {
            Callable::Rust(LuaPtr(callable)) => {
                let closure = &heap[&callable];
                Frame::from_rust(closure, None, stack.full_guard())
            }
            callable @ Callable::Lua(_) => {
                // When callable is Lua closure we don't invoke it directly,
                // instead we do so through a trampoline Rust function.
                // This is because constructing Lua closures is a fallible operation and
                // failing here is rather inconvenient going as far as affecting our public APIs.
                // If construction of Lua frame is bound fail it is still going happen
                // but at a later point where it will be evaluated when executing an already existing thread.
                // At that time runtime is prepared to handle such occurrence.

                let mut stack = stack.full_guard();
                {
                    let mut stack = stack.transient_frame();
                    stack.push(callable.downgrade().into());
                    stack.sync(heap);
                }

                // TODO: replace this with tail call when it becomes available.
                let trampoline = crate::ffi::from_fn(
                    || {
                        crate::ffi::delegate::yield_1(
                            |mut rt: RuntimeView<'_, Ty>| {
                                use crate::ffi::delegate::Request;
                                use repr::index::StackSlot;

                                let Some(Value::Function(callable)) = rt.stack.pop() else {
                                    unreachable!(
                                        "trampoline should receive target function through stack"
                                    );
                                };
                                let callable = callable.upgrade(&rt.core.gc).unwrap();

                                let request = Request::Invoke {
                                    callable,
                                    start: StackSlot(0),
                                };

                                Ok(request)
                            },
                            |_| Ok(()),
                        )
                    },
                    "{trampoline}",
                    (),
                );
                let trampoline = crate::ffi::dyn_ffi(trampoline);

                Frame::from_rust(&trampoline, None, stack)
            }
        };

        let protected = stack.len();

        Thread {
            frames: vec![frame],
            stack,
            protected,
            panicked_with: None,
        }
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

pub struct ThreadGuard<'a, Ty>
where
    Ty: Types,
{
    pub(super) thread_id: ThreadId,

    /// Body of the thread.
    ///
    /// Note that it may not exist yet:
    /// newly constructed threads exist as `ThreadImpetus` objects inside orchestrator.
    pub(super) thread: Option<&'a Thread<Ty>>,
    pub(super) heap: &'a Heap<Ty>,
    pub(super) chunk_cache: &'a dyn ChunkCache,
    pub(super) status: ThreadStatus,
}

impl<'a, Ty> ThreadGuard<'a, Ty>
where
    Ty: Types,
{
    /// Thread backtrace.
    pub fn backtrace(&self) -> Backtrace {
        let frames = self
            .thread
            .map(|thread| thread.frames.iter())
            .unwrap_or_default()
            .map(|frame| frame.backtrace(self.heap, self.chunk_cache))
            .collect();

        Backtrace {
            thread_id: self.thread_id,
            frames,
        }
    }

    /// Thread status.
    ///
    /// See [`ThreadStatus`] enum variants for more information.
    pub fn status(&self) -> ThreadStatus {
        self.status
    }

    /// Id of another thread that caused this one to panic.
    ///
    /// In case another thread was resumed, panicked and that panic was not handled there,
    /// it will propagate to current thread, causing it to unwind.
    /// If the current thread ended up panicking as the result,
    /// it will also remember `ThreadId` of the thread it received panic from.
    /// This way it is possible to reconstruct the entire thread stack.
    pub fn panic_origin(&self) -> Option<ThreadId> {
        self.thread?.panic_origin()
    }

    /// The final error that the thread panicked with.
    ///
    /// This value will be `None` for non-panicked threads.
    pub fn error(&self) -> Option<&RtError<Ty>> {
        self.thread?.error()
    }

    /// The original error that caused unwinding.
    ///
    /// Note that this error can be different from one provided by [.error()](Self::error) function:
    /// it is possible that Rust frames and/or error handler modified the value while processing it.
    pub fn original_error(&self) -> Option<&RtError<Ty>> {
        self.thread?.original_error()
    }
}
