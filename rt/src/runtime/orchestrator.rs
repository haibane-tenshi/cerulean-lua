use std::collections::HashMap;

use crate::error::RtError;
use crate::ffi::DLuaFfi;
use crate::value::{Callable, CoreTypes, Strong};

use super::thread::{Context, Thread, ThreadControl, ThreadImpetus};
use super::Closure;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ThreadId(usize);

pub enum ThreadStatus {
    Current,
    Active,
    Suspended,
    Finished,
    Panicked,
}

pub(crate) struct Orchestrator<Ty>
where
    Ty: CoreTypes,
{
    /// Call stack between threads.
    ///
    /// Lua forbids resuming threads that are suspended on other threads.
    stack: Vec<ThreadId>,

    threads: Vec<Option<Thread<Ty>>>,

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

impl<Ty> Orchestrator<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub fn new_thread(&mut self, ctx: Context<Ty>, callable: Callable<Strong, Ty>) -> ThreadId {
        let id = ThreadId(self.threads.len());

        self.threads.push(None);
        self.nursery
            .insert(id, ThreadImpetus::new(callable, &mut ctx.core.gc));

        id
    }

    fn thread_mut(
        &mut self,
        ctx: Context<Ty>,
        id: ThreadId,
    ) -> Option<Result<&mut Thread<Ty>, RtError<Ty>>> {
        let place = self.threads.get_mut(id.0)?;

        if place.is_none() {
            let impetus = self.nursery.remove(&id)?;
            let thread = match impetus.init(ctx) {
                Ok(thread) => thread,
                Err(err) => return Some(Err(err)),
            };

            *place = Some(thread);
        }

        place.as_mut().map(Ok)
    }

    pub(crate) fn push(&mut self, id: ThreadId) {
        self.stack.push(id);
    }

    pub(crate) fn enter(&mut self, mut ctx: Context<Ty>) -> Result<(), RtError<Ty>> {
        use crate::ffi::delegate::Response;

        let Some(mut current) = self.stack.pop() else {
            return Ok(());
        };
        let mut response = Response::Resume;

        loop {
            let thread = self
                .thread_mut(ctx.reborrow(), current)
                .expect("active thread should exist")?;

            let request = thread.activate(ctx.reborrow()).enter(response);
            match request {
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
        }
    }
}

impl<Ty> Default for Orchestrator<Ty>
where
    Ty: CoreTypes,
{
    fn default() -> Self {
        Self {
            stack: Default::default(),
            threads: Default::default(),
            nursery: Default::default(),
        }
    }
}
