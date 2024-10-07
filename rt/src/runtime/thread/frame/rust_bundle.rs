use std::pin::Pin;

use crate::error::RtError;
use crate::ffi::delegate::{Delegate as RustDelegate, Response};
use crate::ffi::{DLuaFfi, LuaFfi};
use crate::runtime::RuntimeView;
use crate::value::CoreTypes;

use super::FrameControl;

pub(super) struct RustBundle<Ty>
where
    Ty: CoreTypes,
{
    delegate: Pin<Box<dyn RustDelegate<Ty>>>,
}

impl<Ty> RustBundle<Ty>
where
    Ty: CoreTypes,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(super) fn new(closure: &Ty::RustClosure) -> Self {
        let sequence = closure.call();

        RustBundle { delegate: sequence }
    }
}

impl<Ty> RustBundle<Ty>
where
    Ty: CoreTypes,
{
    pub(super) fn enter(
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
