use std::collections::HashMap;
use std::fmt::Display;

use crate::chunk_cache::ChunkCache;
use crate::error::thread::ReentryFailure;
use crate::error::ThreadError;
use crate::ffi::DLuaFfi;
use crate::value::{StrongCallable, Types};

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

enum State<T> {
    /// Thread is not yet started and is located in nursery.
    Startup,

    /// Thread is suspended in the middle of evaluation.
    ///
    /// It is possible for it to be finished.
    Running(T),
}

impl<T> State<T> {
    fn as_ref(&self) -> State<&T> {
        match self {
            State::Startup => State::Startup,
            State::Running(t) => State::Running(t),
        }
    }

    fn as_mut(&mut self) -> State<&mut T> {
        match self {
            State::Startup => State::Startup,
            State::Running(t) => State::Running(t),
        }
    }

    fn as_option_mut(&mut self) -> Option<&mut T> {
        self.as_mut().into_option()
    }

    fn into_option(self) -> Option<T> {
        match self {
            State::Startup => None,
            State::Running(t) => Some(t),
        }
    }

    fn is_starting(&self) -> bool {
        matches!(self, State::Startup)
    }
}

enum EvalState<T> {
    Current,
    Startup,
    Running(T),
}

impl<T> EvalState<T> {
    fn into_option(self) -> Option<T> {
        match self {
            EvalState::Current | EvalState::Startup => None,
            EvalState::Running(t) => Some(t),
        }
    }
}

impl<T> From<State<T>> for EvalState<T> {
    fn from(value: State<T>) -> Self {
        match value {
            State::Startup => EvalState::Startup,
            State::Running(t) => EvalState::Running(t),
        }
    }
}

pub(crate) struct ThreadStore<Ty>
where
    Ty: Types,
{
    threads: Vec<State<Thread<Ty>>>,

    /// Newly created threads.
    ///
    /// These threads are suspended on entry point into the first actual frame.
    /// Runtime promises that stack space of Lua frames is adjusted on entry
    /// which didn't happen yet for those threads - call arguments are not yet provided.
    ///
    /// This isn't a problem for any other suspension point:
    /// Rust frames are responsible for controlling their allotted stack space.
    nursery: HashMap<ThreadId, ThreadImpetus<Ty>>,

    next_id: ThreadId,
}

impl<Ty> ThreadStore<Ty>
where
    Ty: Types,
{
    fn new_thread(&mut self, callable: StrongCallable<Ty>) -> ThreadId {
        let id = self.next_id;
        self.next_id = ThreadId(id.0 + 1);

        self.nursery.insert(id, ThreadImpetus::new(callable));

        id
    }

    fn get(&self, id: ThreadId) -> Option<State<&Thread<Ty>>> {
        self.threads
            .get(id.0)
            .map(State::as_ref)
            .or_else(|| self.contains(id).then_some(State::Startup))
    }

    fn contains(&self, id: ThreadId) -> bool {
        (0..self.next_id.0).contains(&id.0)
    }

    fn with_current(&mut self, id: ThreadId) -> (ThreadStoreGuard<'_, Ty>, &mut Thread<Ty>) {
        assert!(self.contains(id), "thread must exist");

        let ThreadStore {
            threads,
            nursery,
            next_id,
        } = self;

        let (slice, thread) = {
            let (front, back) = threads.split_at_mut(id.0);
            let (thread, back) = back.split_first_mut().unwrap();
            (PerforatedSlice(front, back), thread)
        };

        let guard = ThreadStoreGuard {
            threads: slice,
            nursery,
            next_id,
        };

        let thread = thread
            .as_option_mut()
            .expect("thread should be already initialized");

        (guard, thread)
    }
}

impl<Ty> ThreadStore<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    fn init(&mut self, startup: ThreadId, stack: Stack<Ty>, heap: &mut Heap<Ty>) {
        let place = if let Some(place) = self.threads.get_mut(startup.0) {
            place
        } else {
            self.threads.resize_with(startup.0 + 1, || State::Startup);
            self.threads.get_mut(startup.0).unwrap()
        };

        assert!(place.is_starting(), "can only initialize a thread once");

        let impetus = self
            .nursery
            .remove(&startup)
            .expect("starting threads should be placed in nursery");

        let thread = impetus.init(stack, heap);
        *place = State::Running(thread);
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
            next_id: ThreadId(0),
        }
    }
}

/// Mutable slice `&mut [T]` with single element missing in the middle.
///
/// This type allows us to have disjoint mutable reference to omitted element and all others.
///
/// It is currently implemented as two slices, although it can be done more efficiently using unsafe.
struct PerforatedSlice<'a, T>(&'a mut [T], &'a mut [T]);

impl<T> PerforatedSlice<'_, T> {
    fn reborrow(&mut self) -> PerforatedSlice<'_, T> {
        let PerforatedSlice(front, back) = self;
        PerforatedSlice(front, back)
    }

    fn hole(&self) -> usize {
        self.0.len()
    }

    fn get(&self, i: usize) -> Option<Option<&T>> {
        if let Some(t) = self.0.get(i) {
            Some(Some(t))
        } else {
            let i = i - self.0.len();
            if let Some(i) = i.checked_sub(1) {
                self.1.get(i).map(Some)
            } else {
                Some(None)
            }
        }
    }

    fn get_mut(&mut self, i: usize) -> Option<Option<&mut T>> {
        let front_len = self.0.len();

        if let Some(t) = self.0.get_mut(i) {
            Some(Some(t))
        } else {
            let i = i - front_len;
            if let Some(i) = i.checked_sub(1) {
                self.1.get_mut(i).map(Some)
            } else {
                Some(None)
            }
        }
    }
}

struct ThreadStoreGuard<'a, Ty>
where
    Ty: Types,
{
    threads: PerforatedSlice<'a, State<Thread<Ty>>>,
    nursery: &'a mut HashMap<ThreadId, ThreadImpetus<Ty>>,
    next_id: &'a mut ThreadId,
}

impl<Ty> ThreadStoreGuard<'_, Ty>
where
    Ty: Types,
{
    fn reborrow(&mut self) -> ThreadStoreGuard<'_, Ty> {
        let ThreadStoreGuard {
            threads,
            nursery,
            next_id,
        } = self;

        ThreadStoreGuard {
            threads: threads.reborrow(),
            nursery,
            next_id,
        }
    }

    fn current(&self) -> ThreadId {
        ThreadId(self.threads.hole())
    }

    fn contains(&self, id: ThreadId) -> bool {
        (0..self.next_id.0).contains(&id.0)
    }

    fn get(&self, id: ThreadId) -> Option<EvalState<&Thread<Ty>>> {
        self.threads
            .get(id.0)
            .map(|state| {
                state
                    .map(State::as_ref)
                    .map(Into::into)
                    .unwrap_or(EvalState::Current)
            })
            .or_else(|| self.contains(id).then_some(EvalState::Startup))
    }

    fn get_mut(&mut self, id: ThreadId) -> Option<EvalState<&mut Thread<Ty>>> {
        let fallback = self.contains(id).then_some(EvalState::Startup);
        self.threads
            .get_mut(id.0)
            .map(|state| {
                state
                    .map(State::as_mut)
                    .map(Into::into)
                    .unwrap_or(EvalState::Current)
            })
            .or(fallback)
    }
}

struct ThreadManager<Ty>
where
    Ty: Types,
{
    /// Call stack between threads.
    ///
    /// Lua forbids resuming threads that are suspended on other threads.
    stack: Vec<ThreadId>,

    store: ThreadStore<Ty>,
}

impl<Ty> ThreadManager<Ty>
where
    Ty: Types,
{
    fn new_thread(&mut self, callable: StrongCallable<Ty>) -> ThreadId {
        self.store.new_thread(callable)
    }

    fn push(&mut self, id: ThreadId) -> Result<(), ReentryFailure> {
        if let Some(err) = make_reentry_error(id, self.status_of(id)) {
            Err(err)
        } else {
            self.stack.push(id);
            Ok(())
        }
    }

    fn pop(&mut self) -> Option<ThreadId> {
        self.stack.pop()
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn get(&self, id: ThreadId) -> Option<State<&Thread<Ty>>> {
        self.store.get(id)
    }

    fn contains(&self, id: ThreadId) -> bool {
        self.store.contains(id)
    }

    fn status_of(&self, id: ThreadId) -> Option<ThreadStatus> {
        use super::thread::Status;

        let r = match self.store.get(id)? {
            State::Startup => ThreadStatus::Suspended,
            State::Running(thread) => match thread.status() {
                Status::Finished => ThreadStatus::Finished,
                Status::Panicked => ThreadStatus::Panicked,
                Status::Normal => {
                    if self.stack.contains(&id) {
                        ThreadStatus::Active
                    } else {
                        ThreadStatus::Suspended
                    }
                }
            },
        };

        Some(r)
    }

    fn with_current(&mut self, id: ThreadId) -> (ThreadManagerGuard<'_, Ty>, &mut Thread<Ty>) {
        let ThreadManager { stack, store } = self;
        let (store, thread) = store.with_current(id);

        let guard = ThreadManagerGuard { stack, store };

        (guard, thread)
    }
}

impl<Ty> ThreadManager<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    fn init(&mut self, startup: ThreadId, stack: Stack<Ty>, heap: &mut Heap<Ty>) {
        self.store.init(startup, stack, heap)
    }
}

impl<Ty> Default for ThreadManager<Ty>
where
    Ty: Types,
{
    fn default() -> Self {
        Self {
            stack: Default::default(),
            store: Default::default(),
        }
    }
}

pub(crate) struct ThreadManagerGuard<'a, Ty>
where
    Ty: Types,
{
    stack: &'a [ThreadId],
    store: ThreadStoreGuard<'a, Ty>,
}

impl<Ty> ThreadManagerGuard<'_, Ty>
where
    Ty: Types,
{
    pub(crate) fn reborrow(&mut self) -> ThreadManagerGuard<'_, Ty> {
        let ThreadManagerGuard { stack, store } = self;

        ThreadManagerGuard {
            stack,
            store: store.reborrow(),
        }
    }

    pub(crate) fn current(&self) -> ThreadId {
        self.store.current()
    }

    pub(crate) fn status_of(&self, id: ThreadId) -> Option<ThreadStatus> {
        use super::thread::Status;

        let r = match self.store.get(id)? {
            EvalState::Current => ThreadStatus::Current,
            EvalState::Startup => ThreadStatus::Suspended,
            EvalState::Running(thread) => match thread.status() {
                Status::Finished => ThreadStatus::Finished,
                Status::Panicked => ThreadStatus::Panicked,
                Status::Normal => {
                    if self.stack.contains(&id) {
                        ThreadStatus::Active
                    } else {
                        ThreadStatus::Suspended
                    }
                }
            },
        };

        Some(r)
    }

    pub(crate) fn stack_of(&mut self, id: ThreadId) -> Option<StackGuard<'_, Ty>> {
        self.store
            .get_mut(id)
            .and_then(EvalState::into_option)
            .map(Thread::stack)
    }
}

pub struct Orchestrator<Ty>
where
    Ty: Types,
{
    manager: ThreadManager<Ty>,

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
            manager: Default::default(),
            temp_stack: Stack::new(heap),
            upvalue_cache: UpvalueRegister::new(heap),
        }
    }

    pub(crate) fn new_thread(&mut self, callable: StrongCallable<Ty>) -> ThreadId {
        self.manager.new_thread(callable)
    }

    pub(crate) fn stack(&mut self) -> StackGuard<'_, Ty> {
        self.temp_stack.full_guard()
    }

    pub(crate) fn contains(&self, thread_id: ThreadId) -> bool {
        self.manager.contains(thread_id)
    }

    pub(crate) fn push(&mut self, thread_id: ThreadId) -> Result<(), ReentryFailure> {
        self.manager.push(thread_id)
    }

    pub(crate) fn status_of(&self, id: ThreadId) -> Option<ThreadStatus> {
        self.manager.status_of(id)
    }

    pub(crate) fn thread(&self, id: ThreadId) -> Option<&Thread<Ty>> {
        self.manager.get(id).and_then(State::into_option)
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
        threads: ThreadManagerGuard<'s, Ty>,
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
            threads,
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
        use super::thread::stack::copy;
        use crate::ffi::delegate::Response;

        assert!(
            orch.manager.len() <= 1,
            "suspected attempt at resuming panicked orchestrator; currently unsupported"
        );

        let Some(mut current) = orch.manager.pop() else {
            return Ok(());
        };

        let mut response = Response::Resume;

        loop {
            match orch.manager.get(current) {
                None => unreachable!("thread stack should not contains non-existent threads"),
                Some(State::Running(_)) => (),
                Some(State::Startup) => {
                    let new_stack = Stack::new(&mut self.core.gc);
                    let stack = std::mem::replace(&mut orch.temp_stack, new_stack);

                    orch.manager.init(current, stack, &mut self.core.gc);
                }
            }

            let (threads, thread) = orch.manager.with_current(current);
            let mut temp_stack = orch.temp_stack.full_guard();

            // We use orchestrator's stack as an intermediate destination.
            copy(temp_stack.reborrow(), thread.stack(), &mut self.core.gc);

            let mut ctx = self.thread_context(threads, &mut orch.upvalue_cache);
            (current, response) = match ctx.eval(thread, response) {
                Ok(ThreadControl::Resume { thread: thread_id }) => {
                    if let Some(err) =
                        make_reentry_error(thread_id, ctx.threads.status_of(thread_id))
                    {
                        break Err(err.into());
                    }

                    copy(thread.stack(), temp_stack, &mut self.core.gc);
                    orch.manager.push(current)?;

                    (thread_id, Response::Resume)
                }
                Ok(ThreadControl::Yield | ThreadControl::Return) => {
                    copy(thread.stack(), temp_stack, &mut self.core.gc);

                    match orch.manager.pop() {
                        Some(thread) => (thread, Response::Evaluated(Ok(()))),
                        None => break Ok(()),
                    }
                }
                Err(err) => match orch.manager.pop() {
                    Some(thread) => (thread, Response::Evaluated(Err(err.into()))),
                    None => break Err(err),
                },
            };
        }
    }
}

fn make_reentry_error(id: ThreadId, status: Option<ThreadStatus>) -> Option<ReentryFailure> {
    use crate::error::thread::ThreadStatus as Status;

    let status = match status {
        None => Some(Status::NotExist),
        Some(ThreadStatus::Active | ThreadStatus::Current) => Some(Status::Active),
        Some(ThreadStatus::Finished) => Some(Status::Finished),
        Some(ThreadStatus::Panicked) => Some(Status::Panicked),
        Some(ThreadStatus::Suspended) => None,
    }?;

    let err = ReentryFailure {
        thread_id: id,
        status,
    };

    Some(err)
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
