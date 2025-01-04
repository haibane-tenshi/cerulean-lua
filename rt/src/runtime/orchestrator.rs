use std::collections::HashMap;
use std::fmt::Display;

use crate::chunk_cache::ChunkCache;
use crate::error::ThreadError;
use crate::ffi::DLuaFfi;
use crate::value::{Callable, Strong, Types};

use super::thread::frame::UpvalueRegister;
use super::thread::stack::{Stack, StackGuard};
use super::thread::{Context as ThreadContext, Thread, ThreadControl, ThreadImpetus};
use super::{Cache, Closure, Core, Heap};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ThreadId(usize);

impl ThreadId {
    pub(crate) fn dummy() -> Self {
        ThreadId(0)
    }
}

impl Display for ThreadId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[expect(clippy::large_enum_variant)]
enum ThreadState<Ty>
where
    Ty: Types,
{
    /// Thread is not yet started and is located in nursery.
    Startup,

    /// Thread is suspended in the middle of evaluation.
    ///
    /// It is possible for it to be finished.
    Running(Thread<Ty>),

    /// Thread is currently evaluating.
    ///
    /// At most one thread should be marked this way.
    ///
    /// Due to borrow conflicts
    /// (Lua frames may need mutable access to *any* thread stack which is caused by upvalue behavior)
    /// we need to move the thread out of structure for evaluation.
    Evaluating,
}

impl<Ty> ThreadState<Ty>
where
    Ty: Types,
{
    // fn as_thread_mut(&mut self) -> Option<&mut Thread<Ty>> {
    //     match self {
    //         ThreadState::Running(thread) => Some(thread),
    //         ThreadState::Startup | ThreadState::Evaluating => None,
    //     }
    // }

    fn take(&mut self) -> Option<Thread<Ty>> {
        use std::mem::replace;

        match replace(self, ThreadState::Evaluating) {
            ThreadState::Running(thread) => Some(thread),
            state => {
                let _ = replace(self, state);
                None
            }
        }
    }

    #[allow(clippy::result_large_err)]
    fn place(&mut self, thread: Thread<Ty>) -> Result<(), Thread<Ty>> {
        if self.is_evaluating() {
            let _ = std::mem::replace(self, ThreadState::Running(thread));
            Ok(())
        } else {
            Err(thread)
        }
    }

    fn is_starting(&self) -> bool {
        matches!(self, ThreadState::Startup)
    }

    fn is_evaluating(&self) -> bool {
        matches!(self, ThreadState::Evaluating)
    }
}

pub(crate) struct ThreadStore<Ty>
where
    Ty: Types,
{
    threads: Vec<ThreadState<Ty>>,

    /// Newly created threads.
    ///
    /// These threads are suspended on entry point into the first actual frame.
    /// Runtime promises that stack space of Lua frames is adjusted on entry
    /// which didn't happen yet for those threads - call arguments are not yet provided.
    ///
    /// This isn't a problem for any other suspension point:
    /// Rust frames are responsible for controlling their allotted stack space.
    nursery: HashMap<ThreadId, ThreadImpetus<Ty>>,
}

impl<Ty> ThreadStore<Ty>
where
    Ty: Types,
{
    fn new_thread(&mut self, callable: Callable<Strong, Ty>) -> ThreadId {
        let id = ThreadId(self.threads.len());

        self.threads.push(ThreadState::Startup);
        self.nursery.insert(id, ThreadImpetus::new(callable));

        id
    }

    pub(super) fn stack_of(&mut self, id: ThreadId) -> Option<StackGuard<'_, Ty>> {
        match self.threads.get_mut(id.0)? {
            ThreadState::Startup => None,
            ThreadState::Running(thread) => Some(thread.stack()),
            ThreadState::Evaluating => None,
        }
    }

    fn get(&self, thread_id: ThreadId) -> Option<&ThreadState<Ty>> {
        self.threads.get(thread_id.0)
    }

    fn thread(&self, thread_id: ThreadId) -> Option<&Thread<Ty>> {
        self.threads.get(thread_id.0).and_then(|state| {
            if let ThreadState::Running(thread) = state {
                Some(thread)
            } else {
                None
            }
        })
    }

    fn contains(&self, thread_id: ThreadId) -> bool {
        (0..self.threads.len()).contains(&thread_id.0)
    }

    fn with_current<R>(
        &mut self,
        thread_id: ThreadId,
        f: impl FnOnce(&mut Self, &mut Thread<Ty>) -> R,
    ) -> R {
        let mut thread = self
            .threads
            .get_mut(thread_id.0)
            // TODO: this is technically observable, convert it to error.
            .expect("thread should exist")
            .take()
            .expect("thread should be initialized to enter");

        let res = f(self, &mut thread);

        self.threads
            .get_mut(thread_id.0)
            .expect("thread should exist")
            .place(thread)
            .ok()
            .expect("current thread should not get overwritten");

        res
    }
}

impl<Ty> ThreadStore<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    fn init(&mut self, startup: ThreadId, stack: Stack<Ty>, heap: &mut Heap<Ty>) {
        let place = self
            .threads
            .get_mut(startup.0)
            .expect("thread should exist");

        assert!(place.is_starting(), "can only initialize a thread once");

        let impetus = self
            .nursery
            .remove(&startup)
            .expect("starting threads should be placed in nursery");
        let thread = impetus.init(stack, heap);

        *place = ThreadState::Running(thread);
    }
}

impl<Ty> Default for ThreadStore<Ty>
where
    Ty: Types,
{
    fn default() -> Self {
        Self {
            threads: Default::default(),
            nursery: Default::default(),
        }
    }
}

pub struct Orchestrator<Ty>
where
    Ty: Types,
{
    /// Call stack between threads.
    ///
    /// Lua forbids resuming threads that are suspended on other threads.
    stack: Vec<ThreadId>,

    store: ThreadStore<Ty>,

    /// Stack of temporaries used to communicate with host program.
    temp_stack: Stack<Ty>,

    /// Backing storage for upvalue register used by Lua frames.
    ///
    /// Exists here mostly to allow reusing existing allocation.
    ///
    /// TODO: Consider using per-thread upvalue stack instead.
    upvalue_cache: UpvalueRegister<Ty>,
}

impl<Ty> Orchestrator<Ty>
where
    Ty: Types,
{
    pub(crate) fn new(heap: &mut Heap<Ty>) -> Self {
        Orchestrator {
            stack: Default::default(),
            store: Default::default(),
            temp_stack: Stack::new(heap),
            upvalue_cache: UpvalueRegister::new(heap),
        }
    }

    pub(crate) fn new_thread(&mut self, callable: Callable<Strong, Ty>) -> ThreadId {
        self.store.new_thread(callable)
    }

    pub(crate) fn stack(&mut self) -> StackGuard<'_, Ty> {
        self.temp_stack.full_guard()
    }

    pub(crate) fn status_of(&self, thread_id: ThreadId) -> Option<ThreadStatus> {
        use super::thread::Status;

        let r = match self.store.threads.get(thread_id.0)? {
            ThreadState::Evaluating => ThreadStatus::Current,
            ThreadState::Startup => ThreadStatus::Suspended,
            ThreadState::Running(thread) => match thread.status() {
                Status::Finished => ThreadStatus::Finished,
                Status::Panicked => ThreadStatus::Panicked,
                Status::Normal => {
                    if self.stack.contains(&thread_id) {
                        ThreadStatus::Active
                    } else {
                        ThreadStatus::Suspended
                    }
                }
            },
        };

        Some(r)
    }

    pub(crate) fn contains(&self, thread_id: ThreadId) -> bool {
        self.store.contains(thread_id)
    }

    pub(crate) fn thread(&self, thread_id: ThreadId) -> Option<&Thread<Ty>> {
        self.store.thread(thread_id)
    }

    pub(crate) fn push(&mut self, thread_id: ThreadId) {
        self.stack.push(thread_id)
    }
}

pub(crate) struct Context<'a, Ty>
where
    Ty: Types,
{
    pub(crate) core: &'a mut Core<Ty>,
    pub(crate) internal_cache: &'a Cache<Ty>,
    pub(crate) chunk_cache: &'a mut dyn ChunkCache,
}

impl<Ty> Context<'_, Ty>
where
    Ty: Types,
{
    fn thread_context<'s>(
        &'s mut self,
        current_thread_id: ThreadId,
        thread_store: &'s mut ThreadStore<Ty>,
        upvalue_cache: &'s mut UpvalueRegister<Ty>,
    ) -> ThreadContext<'s, Ty> {
        let Context {
            core,
            internal_cache,
            chunk_cache,
        } = self;

        ThreadContext {
            core,
            internal_cache,
            chunk_cache: *chunk_cache,
            current_thread_id,
            thread_store,
            upvalue_cache,
        }
    }
}

impl<Ty> Context<'_, Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(crate) fn eval(&mut self, orch: &mut Orchestrator<Ty>) -> Result<(), ThreadError> {
        use crate::ffi::delegate::Response;
        use std::ops::ControlFlow;

        assert!(
            orch.stack.len() <= 1,
            "suspected attempt at resuming panicked orchestrator; currently unsupported"
        );

        let Some(mut current) = orch.stack.pop() else {
            return Ok(());
        };

        let mut response = Response::Resume;

        loop {
            match orch.store.get(current) {
                None => todo!(),
                Some(ThreadState::Evaluating) => unreachable!(),
                Some(ThreadState::Running(_)) => (),
                Some(ThreadState::Startup) => {
                    let new_stack = Stack::new(&mut self.core.gc);
                    let stack = std::mem::replace(&mut orch.temp_stack, new_stack);

                    orch.store.init(current, stack, &mut self.core.gc);
                }
            }

            let r = orch.store.with_current(current, |store, thread| {
                use super::thread::stack::copy;

                let mut temp_stack = orch.temp_stack.full_guard();

                // We use orchestrator's stack as an intermediate destination.
                copy(temp_stack.reborrow(), thread.stack(), &mut self.core.gc);

                let mut ctx = self.thread_context(current, store, &mut orch.upvalue_cache);
                match ctx.eval(thread, response) {
                    Ok(ThreadControl::Resume { thread: thread_id }) => {
                        copy(thread.stack(), temp_stack, &mut self.core.gc);

                        orch.stack.push(current);
                        ControlFlow::Continue((thread_id, Response::Resume))
                    }
                    Ok(ThreadControl::Yield | ThreadControl::Return) => {
                        copy(thread.stack(), temp_stack, &mut self.core.gc);

                        match orch.stack.pop() {
                            Some(thread) => {
                                ControlFlow::Continue((thread, Response::Evaluated(Ok(()))))
                            }
                            None => ControlFlow::Break(Ok(())),
                        }
                    }
                    Err(err) => match orch.stack.pop() {
                        Some(thread) => {
                            ControlFlow::Continue((thread, Response::Evaluated(Err(err.into()))))
                        }
                        None => ControlFlow::Break(Err(err)),
                    },
                }
            });

            (current, response) = match r {
                ControlFlow::Break(res) => break res,
                ControlFlow::Continue(t) => t,
            };
        }
    }
}

/// Status of a Lua thread.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ThreadStatus {
    /// The thread is currently executing.
    Current,

    /// The thread is suspended as part of thread stack.
    ///
    /// Note that Lua considers it ill-formed to resume active threads.
    Active,

    /// The thread is suspended and can be resumed.
    Suspended,

    /// The thread's entry function is driven to completion.
    ///
    /// Note that Lua considers it ill-formed to resume finished threads.
    Finished,

    /// The thread received a runtime error and it wasn't handled.
    Panicked,
}
