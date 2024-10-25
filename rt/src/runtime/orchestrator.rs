use std::collections::HashMap;

use crate::chunk_cache::ChunkCache;
use crate::error::RtError;
use crate::ffi::DLuaFfi;
use crate::value::{Callable, CoreTypes, Strong};

use super::thread::stack::StackGuard;
use super::thread::upvalue_register::UpvalueRegister;
use super::thread::{Context as ThreadContext, Thread, ThreadControl, ThreadImpetus};
use super::{Closure, Core, Heap};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ThreadId(usize);

impl ThreadId {
    pub(crate) fn dummy() -> Self {
        ThreadId(0)
    }
}

enum ThreadState<Ty>
where
    Ty: CoreTypes,
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
    Ty: CoreTypes,
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
    Ty: CoreTypes,
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
    Ty: CoreTypes,
{
    fn new_thread(&mut self, heap: &mut Heap<Ty>, callable: Callable<Strong, Ty>) -> ThreadId {
        let id = ThreadId(self.threads.len());

        self.threads.push(ThreadState::Startup);
        self.nursery.insert(id, ThreadImpetus::new(callable, heap));

        id
    }

    pub(super) fn stack_of(&mut self, id: ThreadId) -> Option<StackGuard<'_, Ty>> {
        match self.threads.get_mut(id.0)? {
            ThreadState::Startup => {
                let stack = self
                    .nursery
                    .get_mut(&id)
                    .expect("startup threads should be placed in nursery")
                    .stack();

                Some(stack)
            }
            ThreadState::Running(thread) => Some(thread.stack()),
            ThreadState::Evaluating => None,
        }
    }
}

impl<Ty> ThreadStore<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    fn take_thread(
        &mut self,
        id: ThreadId,
        ctx: Context<Ty>,
    ) -> Option<Result<Thread<Ty>, RtError<Ty>>> {
        let place = self.threads.get_mut(id.0)?;

        if place.is_starting() {
            let impetus = self
                .nursery
                .remove(&id)
                .expect("starting threads should be placed in nursery");
            let thread = match impetus.init(ctx) {
                Ok(thread) => thread,
                Err(err) => return Some(Err(err)),
            };

            *place = ThreadState::Running(thread);
        }

        let thread = place
            .take()
            .expect("there should be only one thread under evaluation");

        Some(Ok(thread))
    }
}

impl<Ty> Default for ThreadStore<Ty>
where
    Ty: CoreTypes,
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
    Ty: CoreTypes,
{
    /// Call stack between threads.
    ///
    /// Lua forbids resuming threads that are suspended on other threads.
    stack: Vec<ThreadId>,
    store: ThreadStore<Ty>,

    /// Backing storage for upvalue register used by Lua frames.
    ///
    /// Exists here mostly to allow reusing existing allocation.
    ///
    /// TODO: Consider using per-thread upvalue stack instead.
    upvalue_cache: UpvalueRegister<Ty>,
}

impl<Ty> Orchestrator<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn new(heap: &mut Heap<Ty>) -> Self {
        Orchestrator {
            stack: Default::default(),
            store: Default::default(),
            upvalue_cache: UpvalueRegister::new(heap),
        }
    }

    pub fn new_thread(&mut self, heap: &mut Heap<Ty>, callable: Callable<Strong, Ty>) -> ThreadId {
        self.store.new_thread(heap, callable)
    }
}

impl<Ty> Orchestrator<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(crate) fn enter(
        &mut self,
        mut ctx: Context<Ty>,
        thread_id: ThreadId,
    ) -> Result<(), RtError<Ty>> {
        use crate::ffi::delegate::Response;

        if !self.stack.is_empty() {
            todo!()
        }

        let mut current = thread_id;
        let mut response = Response::Resume;

        loop {
            let mut thread = self
                .store
                .take_thread(current, ctx.reborrow())
                .expect("active thread should exist")?;

            let ctx = ctx.thread_context(current, &mut self.store, &mut self.upvalue_cache);
            match thread.activate(ctx).enter(response) {
                Ok(ThreadControl::Resume { thread }) => {
                    self.stack.push(current);
                    current = thread;
                    response = Response::Resume;
                }
                Ok(ThreadControl::Yield) => {
                    current = match self.stack.pop() {
                        Some(thread) => thread,
                        None => break Ok(()),
                    };
                    response = Response::Evaluated(Ok(()));
                }
                Ok(ThreadControl::Return) => {
                    current = match self.stack.pop() {
                        Some(thread) => thread,
                        None => break Ok(()),
                    };
                    response = Response::Evaluated(Ok(()))
                }
                Err(err) => {
                    current = match self.stack.pop() {
                        Some(thread) => thread,
                        None => break Err(err),
                    };
                    response = Response::Evaluated(Err(err))
                }
            }

            self.store
                .threads
                .get_mut(current.0)
                .expect("active thread should exist")
                .place(thread)
                .ok() // Thread doesn't implement Debug for a moment.
                .expect("evaluating thread should not get overwritten");
        }
    }
}

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
            core,
            chunk_cache: *chunk_cache,
        }
    }

    fn thread_context<'s>(
        &'s mut self,
        current_thread_id: ThreadId,
        thread_store: &'s mut ThreadStore<Ty>,
        upvalue_cache: &'s mut UpvalueRegister<Ty>,
    ) -> ThreadContext<'s, Ty> {
        let Context { core, chunk_cache } = self;

        ThreadContext {
            core,
            chunk_cache: *chunk_cache,
            current_thread_id,
            thread_store,
            upvalue_cache,
        }
    }
}

pub enum ThreadStatus {
    Current,
    Active,
    Suspended,
    Finished,
    Panicked,
}
