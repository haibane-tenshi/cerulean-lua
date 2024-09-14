use crate::error::RtError;
use crate::ffi::DLuaFfi;
use crate::value::CoreTypes;

use super::thread::{Context, Thread};
use super::Closure;

pub(crate) struct Orchestrator<Ty>
where
    Ty: CoreTypes,
{
    threads: Vec<Thread<Ty>>,
}

impl<Ty> Orchestrator<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn push(&mut self, thread: Thread<Ty>) {
        self.threads.push(thread);
    }
}

impl<Ty> Orchestrator<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(crate) fn enter(&mut self, mut ctx: Context<Ty>) -> Result<(), RtError<Ty>> {
        while let Some(mut thread) = self.threads.pop() {
            thread.activate(ctx.reborrow()).enter()?;
        }

        Ok(())
    }
}

impl<Ty> Default for Orchestrator<Ty>
where
    Ty: CoreTypes,
{
    fn default() -> Self {
        Self {
            threads: Default::default(),
        }
    }
}
