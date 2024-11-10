use std::pin::Pin;

use crate::backtrace::BacktraceFrame;
use crate::error::RtError;
use crate::ffi::delegate::{Delegate as RustDelegate, Response};
use crate::ffi::{DLuaFfi, DebugInfo, LuaFfi};
use crate::runtime::RuntimeView;
use crate::value::Types;

use super::{Control, DelegateThreadControl, FrameControl};

pub(super) struct RustBundle<Ty>
where
    Ty: Types,
{
    delegate: Pin<Box<dyn RustDelegate<Ty>>>,
    debug_info: DebugInfo,
}

impl<Ty> RustBundle<Ty>
where
    Ty: Types,
{
    pub(super) fn new(closure: &dyn DLuaFfi<Ty>) -> Self {
        let delegate = closure.call();
        let debug_info = closure.debug_info();

        RustBundle {
            delegate,
            debug_info,
        }
    }
}

impl<Ty> RustBundle<Ty>
where
    Ty: Types,
{
    pub(super) fn enter(
        &mut self,
        mut ctx: RuntimeView<Ty>,
        response: Response<Ty>,
    ) -> Result<Control<FrameControl<Ty>, DelegateThreadControl>, RtError<Ty>> {
        use super::{DelegateThreadControl, FrameControl};
        use crate::ffi::coroutine::State;
        use crate::ffi::delegate::Request;

        let boundary = ctx.stack.boundary();

        // Ensure that stack is properly synced before entering Rust frame.
        // It doesn't matter much if we sync it after, however:
        // if the next frame is Rust frame we will sync it on entry,
        // if the next frame is Lua frame it will sync it when necessary.
        ctx.stack.lua_frame().sync(&mut ctx.core.gc);

        match self.delegate.as_mut().resume(ctx, response) {
            State::Complete(Ok(())) => Ok(Control::Frame(FrameControl::Return)),
            State::Complete(Err(err)) => Err(err),
            State::Yielded(request) => match request {
                Request::Invoke { callable, start } => {
                    let start = boundary + start;
                    let r = FrameControl::InitAndEnter {
                        event: None,
                        callable,
                        start,
                    };

                    Ok(Control::Frame(r))
                }
                Request::Resume { thread, start } => {
                    let start = boundary + start;
                    let request = DelegateThreadControl::Resume { thread, start };
                    Ok(Control::Thread(request))
                }
                Request::Yield { start } => {
                    let start = boundary + start;
                    let request = DelegateThreadControl::Yield { start };
                    Ok(Control::Thread(request))
                }
            },
        }
    }

    pub(super) fn backtrace(&self) -> BacktraceFrame {
        use crate::backtrace::FrameSource;

        let name = Some(self.debug_info.name.clone());

        BacktraceFrame {
            source: FrameSource::Rust,
            name,
            location: None,
        }
    }
}
