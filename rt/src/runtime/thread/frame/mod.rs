mod lua_bundle;
mod rust_bundle;

use enumoid::Enumoid;

use repr::opcode::{AriBinOp, BinOp, BitBinOp, EqBinOp, RelBinOp, StrBinOp};

use crate::chunk_cache::ChunkCache;
use crate::error::RtError;
use crate::ffi::DLuaFfi;
use crate::runtime::{Closure, Core, RuntimeView};
use crate::value::{Callable, CoreTypes, Strong};

use super::super::orchestrator::ThreadId;
use super::stack::{RawStackSlot, Stack};
use super::{Control, Prompt, ThreadControl};

use lua_bundle::{Context as FrameContext, Frame as LuaBundle};
use rust_bundle::RustBundle;

impl<Ty> LuaBundle<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
{
    fn enter(&mut self, ctx: FrameContext<Ty>) -> Result<FrameControl<Ty>, RtError<Ty>> {
        use lua_bundle::ChangeFrame;

        let boundary = ctx.stack.boundary();
        let mut active_frame = self.activate(ctx)?;

        let r = match active_frame.enter()? {
            ChangeFrame::Return(slot) => {
                active_frame.exit(slot);

                FrameControl::Return
            }
            ChangeFrame::Invoke(event, callable, start) => {
                // Ensure that stack space passed to another function no longer hosts upvalues.
                // active_frame.stack.lua_frame().evict_upvalues();

                let start = boundary + start;
                FrameControl::InitAndEnter {
                    event,
                    callable,
                    start,
                }
            }
        };

        Ok(r)
    }
}

enum Bundle<Ty>
where
    Ty: CoreTypes,
{
    Lua(LuaBundle<Ty>),
    Rust(RustBundle<Ty>),
}

pub(super) struct Frame<Ty>
where
    Ty: CoreTypes,
{
    bundle: Bundle<Ty>,
    stack_start: RawStackSlot,

    /// Whether frame was created as result of evaluating metamethod.
    ///
    /// Metamethods mimic builtin behavior of opcodes,
    /// therefore need cleanup to ensure correct stack state after frame is exited.
    event: Option<Event>,
}

impl<Ty> Frame<Ty>
where
    Ty: CoreTypes,
{
    pub(super) fn stack_start(&self) -> RawStackSlot {
        self.stack_start
    }

    pub(super) fn process_error(
        &mut self,
        mut ctx: Context<Ty>,
        err: RtError<Ty>,
    ) -> Result<Control<FrameControl<Ty>, DelegateThreadControl>, RtError<Ty>> {
        use crate::ffi::delegate::Response;

        let Bundle::Rust(rust_bundle) = &mut self.bundle else {
            return Err(err);
        };

        let ctx = ctx.runtime_view(self.stack_start).unwrap();
        rust_bundle.enter(ctx, Response::Evaluated(Err(err)))
    }
}

impl<Ty> Frame<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(super) fn new(
        callable: Callable<Strong, Ty>,
        stack_start: RawStackSlot,
        event: Option<Event>,
        mut ctx: Context<Ty>,
    ) -> Result<Self, RtError<Ty>> {
        use crate::error::OutOfBoundsStack;
        use crate::gc::LuaPtr;

        let ctx = ctx.lua_context(stack_start).ok_or(OutOfBoundsStack)?;
        let bundle = match callable {
            Callable::Lua(LuaPtr(closure)) => {
                let frame = LuaBundle::new(closure, ctx)?;
                Bundle::Lua(frame)
            }
            Callable::Rust(LuaPtr(closure)) => {
                let frame = RustBundle::new(&ctx.core.gc[&closure]);
                Bundle::Rust(frame)
            }
        };

        let r = Frame {
            bundle,
            stack_start,
            event,
        };

        Ok(r)
    }
}

impl<Ty> Frame<Ty>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
{
    pub(super) fn enter(
        &mut self,
        mut ctx: Context<Ty>,
        resume: Prompt,
    ) -> Result<Control<FrameControl<Ty>, DelegateThreadControl>, RtError<Ty>> {
        let Frame {
            bundle,
            stack_start,
            event,
        } = self;

        let stack_start = *stack_start;

        let result = match bundle {
            Bundle::Lua(frame) => {
                let ctx = ctx
                    .lua_context(stack_start)
                    .expect("thread should preserve stack space of suspended frames");
                frame.enter(ctx).map(Control::Frame)
            }
            Bundle::Rust(frame) => {
                let rt = ctx
                    .runtime_view(stack_start)
                    .expect("thread should preserve stack space of suspended frames");
                frame.enter(rt, resume.into())
            }
        }?;

        if matches!(result, Control::Frame(FrameControl::Return)) {
            if let Some(event) = *event {
                ctx.stack
                    .guard(stack_start)
                    .unwrap()
                    .lua_frame()
                    .adjust_event_returns(event);
            }
        }

        Ok(result)
    }
}

pub(super) struct Context<'a, Ty>
where
    Ty: CoreTypes,
{
    pub(crate) core: &'a mut Core<Ty>,
    pub(crate) chunk_cache: &'a mut dyn ChunkCache,
    pub(crate) stack: &'a mut Stack<Ty>,
}

impl<'a, Ty> Context<'a, Ty>
where
    Ty: CoreTypes,
{
    fn lua_context(&mut self, stack_start: RawStackSlot) -> Option<FrameContext<'_, Ty>> {
        let Context {
            core,
            chunk_cache,
            stack,
        } = self;
        let stack = stack.guard(stack_start)?;

        let r = FrameContext {
            core,
            chunk_cache: *chunk_cache,
            stack,
        };

        Some(r)
    }

    fn runtime_view(&mut self, stack_start: RawStackSlot) -> Option<RuntimeView<'_, Ty>> {
        let Context {
            core,
            chunk_cache,
            stack,
        } = self;
        let stack = stack.guard(stack_start)?;

        let r = RuntimeView {
            core,
            chunk_cache: *chunk_cache,
            stack,
        };

        Some(r)
    }
}

pub(super) enum DelegateThreadControl {
    Resume {
        thread: ThreadId,
        start: RawStackSlot,
    },
    Yield {
        start: RawStackSlot,
    },
}

impl DelegateThreadControl {
    pub(super) fn into_thread_control(self) -> (ThreadControl, RawStackSlot) {
        match self {
            DelegateThreadControl::Resume { thread, start } => {
                (ThreadControl::Resume { thread }, start)
            }
            DelegateThreadControl::Yield { start } => (ThreadControl::Yield, start),
        }
    }
}

pub(super) enum FrameControl<Ty>
where
    Ty: CoreTypes,
{
    InitAndEnter {
        callable: Callable<Strong, Ty>,
        start: RawStackSlot,
        event: Option<Event>,
    },
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Enumoid)]
pub(crate) enum Event {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Rem,
    Pow,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    ShL,
    ShR,
    Concat,
    Len,
    Eq,
    Neq,
    Lt,
    LtEq,
    Index,
    NewIndex,
    Call,
}

impl Event {
    pub(crate) fn to_metamethod(self) -> BuiltinMetamethod {
        use BuiltinMetamethod as M;
        use Event::*;

        match self {
            Neg => M::Neg,
            Add => M::Add,
            Sub => M::Sub,
            Mul => M::Mul,
            Div => M::Div,
            FloorDiv => M::FloorDiv,
            Rem => M::Rem,
            Pow => M::Pow,
            BitNot => M::BitNot,
            BitAnd => M::BitAnd,
            BitOr => M::BitOr,
            BitXor => M::BitXor,
            ShL => M::ShL,
            ShR => M::ShR,
            Concat => M::Concat,
            Len => M::Len,
            Eq => M::Eq,
            Neq => M::Eq,
            Lt => M::Lt,
            LtEq => M::LtEq,
            Index => M::Index,
            NewIndex => M::NewIndex,
            Call => M::Call,
        }
    }
}

impl From<BinOp> for Event {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Ari(t) => t.into(),
            BinOp::Bit(t) => t.into(),
            BinOp::Str(t) => t.into(),
            BinOp::Eq(t) => t.into(),
            BinOp::Rel(t) => t.into(),
        }
    }
}

impl From<AriBinOp> for Event {
    fn from(value: AriBinOp) -> Self {
        match value {
            AriBinOp::Add => Event::Add,
            AriBinOp::Sub => Event::Sub,
            AriBinOp::Mul => Event::Mul,
            AriBinOp::Div => Event::Div,
            AriBinOp::FloorDiv => Event::FloorDiv,
            AriBinOp::Rem => Event::Rem,
            AriBinOp::Pow => Event::Pow,
        }
    }
}

impl From<BitBinOp> for Event {
    fn from(value: BitBinOp) -> Self {
        match value {
            BitBinOp::And => Event::BitAnd,
            BitBinOp::Or => Event::BitOr,
            BitBinOp::Xor => Event::BitXor,
            BitBinOp::ShL => Event::ShL,
            BitBinOp::ShR => Event::ShR,
        }
    }
}

impl From<StrBinOp> for Event {
    fn from(value: StrBinOp) -> Self {
        match value {
            StrBinOp::Concat => Event::Concat,
        }
    }
}

impl From<EqBinOp> for Event {
    fn from(value: EqBinOp) -> Self {
        match value {
            EqBinOp::Eq => Event::Eq,
            EqBinOp::Neq => Event::Neq,
        }
    }
}

impl From<RelBinOp> for Event {
    fn from(value: RelBinOp) -> Self {
        match value {
            RelBinOp::Gt | RelBinOp::Lt => Event::Lt,
            RelBinOp::GtEq | RelBinOp::LtEq => Event::LtEq,
        }
    }
}

#[derive(Debug, Clone, Copy, Enumoid)]
pub(crate) enum BuiltinMetamethod {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Rem,
    Pow,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    ShL,
    ShR,
    Concat,
    Len,
    Eq,
    Lt,
    LtEq,
    Index,
    NewIndex,
    Call,
}

impl BuiltinMetamethod {
    pub(crate) fn to_str(self) -> &'static str {
        use BuiltinMetamethod::*;

        match self {
            Neg => "__unm",
            Add => "__add",
            Sub => "__sub",
            Mul => "__mul",
            Div => "__div",
            FloorDiv => "__idiv",
            Rem => "__mod",
            Pow => "__pow",
            BitNot => "__bnot",
            BitAnd => "__band",
            BitOr => "__bor",
            BitXor => "__bxor",
            ShL => "__shl",
            ShR => "__shr",
            Concat => "__concat",
            Len => "__len",
            Eq => "__eq",
            Lt => "__lt",
            LtEq => "__le",
            Index => "__index",
            NewIndex => "__newindex",
            Call => "__call",
        }
    }
}
