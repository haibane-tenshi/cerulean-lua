mod lua_bundle;
mod rust_bundle;

use enumoid::Enumoid;

use repr::opcode::{AriBinOp, BinOp, BitBinOp, EqBinOp, RelBinOp, StrBinOp};

use crate::backtrace::BacktraceFrame;
use crate::chunk_cache::ChunkCache;
use crate::error::{MalformedClosureError, RtError};
use crate::ffi::delegate::RuntimeView;
use crate::ffi::DLuaFfi;
use crate::runtime::closure::UpvaluePlace;
use crate::runtime::{Cache, Closure, Core, Heap};
use crate::value::{Callable, Strong, Types};

use super::super::orchestrator::{ThreadId, ThreadStore};
use super::stack::{RawStackSlot, Stack, StackGuard};
use super::{Control, Prompt, ThreadControl};

use lua_bundle::{Context as FrameContext, Frame as LuaBundle};
use rust_bundle::RustBundle;

pub(crate) use lua_bundle::UpvalueRegister;

impl<Ty> LuaBundle<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
{
    fn enter(&mut self, mut ctx: FrameContext<Ty>) -> Result<FrameControl<Ty>, RtError<Ty>> {
        use lua_bundle::ChangeFrame;
        use repr::index::StackSlot;

        let boundary = ctx.stack.boundary();
        let mut active_frame = self.activate(ctx.reborrow())?;

        let r = active_frame
            .enter()
            .map(|request| match request {
                ChangeFrame::Return(slot) => {
                    let mut stack = ctx.stack.lua_frame();
                    stack.evict_upvalues();
                    let _ = stack.drain(StackSlot(0)..slot);

                    FrameControl::Return
                }
                ChangeFrame::Invoke(event, callable, start) => {
                    // Ensure that stack space passed to another function no longer hosts upvalues.
                    ctx.stack
                        .guard_at(start)
                        .unwrap()
                        .lua_frame()
                        .evict_upvalues();

                    // Make sure that detached upvalues are properly allocated before entering a new frame.
                    ctx.stack.lua_frame().sync_upvalues(&mut ctx.core.gc);

                    let start = boundary + start;
                    FrameControl::InitAndEnter {
                        event,
                        callable,
                        start,
                    }
                }
            })
            .map_err(Into::into);

        // Propagate upvalue changes
        {
            let closure = ctx.core.gc.get_root(self.closure());
            let origin = closure.origin_thread();
            let mut stack = if origin == ctx.current_thread_id {
                ctx.stack
            } else {
                ctx.thread_store
                    .stack_of(origin)
                    .expect("threads should never get deallocated")
            };
            let mut stack = stack.lua_frame();

            for (slot, value) in ctx.upvalues.drain_written() {
                let upvalue = *ctx
                    .core
                    .gc
                    .get_root(self.closure())
                    .upvalues()
                    .get(slot)
                    .unwrap();
                match upvalue {
                    UpvaluePlace::Place(ptr) => {
                        let place = ctx.core.gc.get_mut(ptr).unwrap();
                        *place = value;
                    }
                    UpvaluePlace::Stack(slot) => {
                        stack.set_raw(slot, value);
                    }
                };
            }

            stack.sync(&mut ctx.core.gc);
        }

        r
    }
}

enum Bundle<Ty>
where
    Ty: Types,
{
    Lua(LuaBundle<Ty>),
    Rust(RustBundle<Ty>),
}

pub(super) struct Frame<Ty>
where
    Ty: Types,
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
    Ty: Types,
{
    pub(super) fn stack_start(&self) -> RawStackSlot {
        self.stack_start
    }

    pub(super) fn backtrace(
        &self,
        heap: &Heap<Ty>,
        chunk_cache: &dyn ChunkCache,
    ) -> BacktraceFrame {
        match &self.bundle {
            Bundle::Lua(frame) => frame.backtrace(heap, chunk_cache),
            Bundle::Rust(frame) => frame.backtrace(),
        }
    }
}

impl<Ty> Frame<Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
{
    pub(super) fn new(
        callable: Callable<Strong, Ty>,
        event: Option<Event>,
        heap: &mut Heap<Ty>,
        chunk_cache: &dyn ChunkCache,
        stack: StackGuard<Ty>,
    ) -> Result<Self, MalformedClosureError> {
        use crate::gc::LuaPtr;

        let stack_start = stack.boundary();
        let bundle = match callable {
            Callable::Lua(LuaPtr(closure)) => {
                let frame = LuaBundle::new(closure, heap, chunk_cache, stack)?;
                Bundle::Lua(frame)
            }
            Callable::Rust(LuaPtr(closure)) => {
                let frame = RustBundle::new(&heap[&closure]);
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

    pub(super) fn from_rust(
        callable: &dyn DLuaFfi<Ty>,
        event: Option<Event>,
        stack: StackGuard<Ty>,
    ) -> Self {
        let stack_start = stack.boundary();
        let bundle = Bundle::Rust(RustBundle::new(callable));

        Frame {
            bundle,
            stack_start,
            event,
        }
    }
}

pub(crate) struct Context<'a, Ty>
where
    Ty: Types,
{
    pub(crate) core: &'a mut Core<Ty>,
    pub(crate) internal_cache: &'a Cache<Ty>,
    pub(crate) chunk_cache: &'a mut dyn ChunkCache,
    pub(crate) current_thread_id: ThreadId,
    pub(crate) thread_store: &'a mut ThreadStore<Ty>,
    pub(crate) upvalue_cache: &'a mut UpvalueRegister<Ty>,
    pub(crate) stack: &'a mut Stack<Ty>,
}

impl<Ty> Context<'_, Ty>
where
    Ty: Types,
{
    fn lua_context(&mut self, stack_start: RawStackSlot) -> Option<FrameContext<'_, Ty>> {
        let Context {
            core,
            internal_cache,
            chunk_cache,
            current_thread_id,
            thread_store,
            upvalue_cache,
            stack,
        } = self;

        let stack = stack.guard(stack_start)?;

        let r = FrameContext {
            core,
            internal_cache,
            chunk_cache: *chunk_cache,
            current_thread_id: *current_thread_id,
            thread_store,
            upvalues: upvalue_cache,
            stack,
        };

        Some(r)
    }

    fn runtime_view(&mut self, stack_start: RawStackSlot) -> Option<RuntimeView<'_, Ty>> {
        let Context {
            core,
            chunk_cache,
            stack,
            ..
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

impl<Ty> Context<'_, Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
{
    pub(super) fn eval(
        &mut self,
        frame: &mut Frame<Ty>,
        resume: Prompt,
    ) -> Result<Control<FrameControl<Ty>, DelegateThreadControl>, RtError<Ty>> {
        let Frame {
            bundle,
            stack_start,
            event,
        } = frame;

        let stack_start = *stack_start;

        let result = match bundle {
            Bundle::Lua(frame) => {
                let ctx = self
                    .lua_context(stack_start)
                    .expect("thread should preserve stack space of suspended frames");
                frame.enter(ctx).map(Control::Frame)
            }
            Bundle::Rust(frame) => {
                let mut ctx = self
                    .runtime_view(stack_start)
                    .expect("thread should preserve stack space of suspended frames");
                ctx.eval(frame, resume.into())
            }
        }?;

        if matches!(result, Control::Frame(FrameControl::Return)) {
            if let Some(event) = *event {
                self.stack
                    .guard(stack_start)
                    .unwrap()
                    .lua_frame()
                    .adjust_event_returns(event);
            }
        }

        Ok(result)
    }
}

impl<Ty> Context<'_, Ty>
where
    Ty: Types,
{
    pub(super) fn eval_error(
        &mut self,
        frame: &mut Frame<Ty>,
        err: RtError<Ty>,
    ) -> Result<Control<FrameControl<Ty>, DelegateThreadControl>, RtError<Ty>> {
        use crate::ffi::delegate::Response;

        let Bundle::Rust(rust_bundle) = &mut frame.bundle else {
            return Err(err);
        };

        let mut ctx = self.runtime_view(frame.stack_start).unwrap();
        ctx.eval(rust_bundle, Response::Evaluated(Err(err)))
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
    Ty: Types,
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
