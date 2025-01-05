mod upvalue_register;
mod variadic_register;

use std::fmt::Debug;
use std::ops::ControlFlow;

use gc::{Root, RootCell};
use repr::chunk::ClosureRecipe;
use repr::index::{ConstId, InstrId, InstrOffset, RecipeId, StackSlot};
use repr::literal::Literal;
use repr::opcode::{BinOp, EqBinOp, OpCode, RelBinOp, UnaOp};
use repr::tivec::TiSlice;

use super::super::stack::StackGuard;
use crate::backtrace::BacktraceFrame;
use crate::builtins::NotCallableError;
use crate::chunk_cache::ChunkCache;
use crate::error::opcode::{
    self as opcode_err, IpOutOfBounds, MissingConstId, MissingStackSlot, MissingUpvalue,
};
use crate::error::{
    AlreadyDroppedError, AlreadyDroppedOr, BorrowError, MalformedClosureError, RefAccessError,
    RuntimeError,
};
use crate::gc::{DisplayWith, Heap, LuaPtr};
use crate::runtime::closure::UpvaluePlace;
use crate::runtime::orchestrator::ThreadStore;
use crate::runtime::{Cache, Closure, Core, Interned, ThreadId};
use crate::value::callable::Callable;
use crate::value::{Strong, StrongValue, TableIndex, Types, Value, WeakValue};

use super::Event;
pub(crate) use upvalue_register::UpvalueRegister;
use variadic_register::VariadicRegister;

pub(crate) struct Context<'a, Ty>
where
    Ty: Types,
{
    pub(crate) core: &'a mut Core<Ty>,
    pub(crate) internal_cache: &'a Cache<Ty>,
    pub(crate) chunk_cache: &'a dyn ChunkCache,
    pub(crate) current_thread_id: ThreadId,
    pub(crate) thread_store: &'a mut ThreadStore<Ty>,
    pub(crate) stack: StackGuard<'a, Ty>,
    pub(crate) upvalues: &'a mut UpvalueRegister<Ty>,
}

impl<Ty> Context<'_, Ty>
where
    Ty: Types,
{
    pub(crate) fn reborrow(&mut self) -> Context<'_, Ty> {
        let Context {
            core,
            internal_cache,
            chunk_cache,
            current_thread_id,
            thread_store,
            stack,
            upvalues,
        } = self;

        Context {
            core,
            internal_cache,
            chunk_cache: *chunk_cache,
            current_thread_id: *current_thread_id,
            thread_store,
            stack: stack.reborrow(),
            upvalues,
        }
    }
}

struct Invoke<Ty>(Event, Callable<Strong, Ty>, StackSlot)
where
    Ty: Types;

pub(crate) enum ChangeFrame<Ty>
where
    Ty: Types,
{
    Return(StackSlot),
    Invoke(Option<Event>, Callable<Strong, Ty>, StackSlot),
}

impl<Ty> From<Invoke<Ty>> for ChangeFrame<Ty>
where
    Ty: Types,
{
    fn from(value: Invoke<Ty>) -> Self {
        let Invoke(event, callable, start) = value;
        ChangeFrame::Invoke(Some(event), callable, start)
    }
}

trait MapControlFlow<F> {
    type Output;

    fn map_br(self, f: F) -> Self::Output;
}

impl<B, T, F> MapControlFlow<F> for ControlFlow<B>
where
    F: FnOnce(B) -> T,
{
    type Output = ControlFlow<T>;

    fn map_br(self, f: F) -> Self::Output {
        match self {
            ControlFlow::Continue(t) => ControlFlow::Continue(t),
            ControlFlow::Break(t) => ControlFlow::Break(f(t)),
        }
    }
}

pub(crate) struct Frame<Ty>
where
    Ty: Types,
{
    closure: Root<Closure<Ty>>,
    ip: InstrId,
    register_variadic: VariadicRegister<Ty>,
}

impl<Ty> Frame<Ty>
where
    Ty: Types,
{
    pub(crate) fn new(
        closure: Root<Closure<Ty>>,
        heap: &mut Heap<Ty>,
        chunk_cache: &dyn ChunkCache,
        mut stack: StackGuard<Ty>,
    ) -> Result<Self, MalformedClosureError> {
        use crate::error::{CapturesMismatch, MissingChunk, MissingFunction};
        use repr::chunk::Function;

        let cls = &heap[&closure];
        let fn_ptr = cls.fn_ptr();

        let function = chunk_cache
            .chunk(fn_ptr.chunk_id)
            .ok_or(MissingChunk(fn_ptr.chunk_id))?
            .get_function(fn_ptr.function_id)
            .ok_or(MissingFunction(fn_ptr))?;

        let Function { signature, .. } = function;

        // Verify that closure provides exact same number of upvalues
        // that is expected by the function.
        if signature.upvalue_count != cls.upvalues().len() {
            let err = CapturesMismatch {
                expected: signature.upvalue_count,
                closure: cls.upvalues().len(),
            };

            return Err(err.into());
        }

        // Adjust stack, move varargs into register if needed.
        let mut stack = stack.lua_frame();
        let call_height = StackSlot(0) + signature.arg_count;

        let varargs: Vec<_> = if signature.is_variadic {
            stack.adjust_height(call_height).collect()
        } else {
            let _ = stack.adjust_height(call_height);
            Default::default()
        };

        let register_variadic = VariadicRegister::new(varargs, heap);

        // Ensure that disassociated upvalues are properly reattached.
        // stack.sync_upvalues(heap);

        let r = Frame {
            closure,
            ip: Default::default(),
            register_variadic,
        };

        Ok(r)
    }
}

impl<Ty> Frame<Ty>
where
    Ty: Types,
{
    pub(crate) fn closure(&self) -> &Root<Closure<Ty>> {
        &self.closure
    }

    pub(crate) fn current_ip(&self) -> InstrId {
        self.ip - 1
    }
}

impl<Ty> Frame<Ty>
where
    Ty: Types,
{
    pub(crate) fn activate<'a>(
        &'a mut self,
        ctx: Context<'a, Ty>,
    ) -> Result<ActiveFrame<'a, Ty>, MalformedClosureError>
    where
        WeakValue<Ty>: DisplayWith<Heap<Ty>>,
    {
        use crate::error::{MissingChunk, MissingFunction};
        use upvalue_register::preload_upvalues;

        let Context {
            core,
            internal_cache,
            chunk_cache,
            current_thread_id,
            thread_store,
            mut stack,
            upvalues,
        } = ctx;

        let closure_body = &core.gc[&self.closure];
        let fn_ptr = closure_body.fn_ptr();
        let origin = closure_body.origin_thread();

        let r = (|| {
            let chunk = chunk_cache
                .chunk(fn_ptr.chunk_id)
                .ok_or(MissingChunk(fn_ptr.chunk_id))?;
            let function = chunk
                .get_function(fn_ptr.function_id)
                .ok_or(MissingFunction(fn_ptr))?;
            Ok((chunk, function))
        })();

        let (chunk, function) = match r {
            Ok(t) => t,
            Err(err) => return Err(err),
        };

        let Frame {
            closure,
            ip: store_ip,
            register_variadic,
        } = self;

        let ip = *store_ip;

        let recipes = &chunk.closure_recipes;
        let constants = &chunk.constants;
        let opcodes = &function.opcodes;

        let register_upvalue = {
            let stack = if origin == current_thread_id {
                stack.reborrow()
            } else {
                thread_store
                    .stack_of(origin)
                    .expect("threads should never get deallocated")
            };

            upvalues.fill(preload_upvalues(
                closure_body.upvalues().as_ref(),
                stack,
                &core.gc,
            ));
            upvalues
        };
        let register_variadic = register_variadic.values();

        tracing::trace!(
            stack = stack.to_pretty_string(&core.gc),
            "activated Lua frame"
        );

        let r = ActiveFrame {
            current_thread_id,
            core,
            internal_cache,
            closure,
            store_ip,
            recipes,
            constants,
            opcodes,
            ip,
            stack,
            register_upvalue,
            register_variadic,
        };

        Ok(r)
    }

    pub(super) fn backtrace(
        &self,
        heap: &Heap<Ty>,
        chunk_cache: &dyn ChunkCache,
    ) -> BacktraceFrame {
        use crate::backtrace::{FrameSource, Location};

        let ptr = heap[&self.closure].fn_ptr();
        // Instruction pointer always points at the *next* instruction.
        let ip = self.current_ip();
        let (name, location) = chunk_cache
            .chunk(ptr.chunk_id)
            .and_then(|chunk| chunk.debug_info.as_ref())
            .and_then(|info| info.functions.get(ptr.function_id).map(|func| (info, func)))
            .map(|(chunk, function)| {
                let name = function.name.clone();
                let location = function.opcodes.get(ip).map(|info| {
                    let chunk_location =
                        chunk_cache
                            .location(ptr.chunk_id)
                            .unwrap_or_else(|| Location {
                                file: "<no file location is present>".to_string(),
                                line: 0,
                                column: 0,
                            });

                    let (line, column) = chunk.line_column(info.start());

                    // The first line of chunk might be offset from the beginning of the line.
                    let column = if line == 1 {
                        column + chunk_location.column
                    } else {
                        column
                    };
                    let line = line + chunk_location.line;

                    Location {
                        file: chunk_location.file,
                        line,
                        column,
                    }
                });

                (Some(name), location)
            })
            .unwrap_or_default();

        BacktraceFrame {
            name,
            source: FrameSource::Lua,
            location,
        }
    }
}

impl<Ty> Debug for Frame<Ty>
where
    Ty: Types,
    StrongValue<Ty>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Frame")
            .field("closure", &self.closure)
            .field("ip", &self.ip)
            .field("register_variadic", &self.register_variadic)
            .finish()
    }
}

pub struct ActiveFrame<'rt, Ty>
where
    Ty: Types,
{
    current_thread_id: ThreadId,
    core: &'rt mut Core<Ty>,
    internal_cache: &'rt Cache<Ty>,
    recipes: &'rt TiSlice<RecipeId, ClosureRecipe>,
    constants: &'rt TiSlice<ConstId, Literal>,
    opcodes: &'rt TiSlice<InstrId, OpCode>,

    closure: &'rt Root<Closure<Ty>>,
    store_ip: &'rt mut InstrId,

    ip: InstrId,
    stack: StackGuard<'rt, Ty>,
    register_upvalue: &'rt mut UpvalueRegister<Ty>,
    register_variadic: &'rt [WeakValue<Ty>],
}

impl<Ty> ActiveFrame<'_, Ty>
where
    Ty: Types,
{
    fn get_constant(&self, index: ConstId) -> Result<&Literal, MissingConstId> {
        self.constants.get(index).ok_or(MissingConstId(index))
    }

    fn get_stack(&self, index: StackSlot) -> Result<&WeakValue<Ty>, MissingStackSlot> {
        self.stack.get(index).ok_or(MissingStackSlot(index))
    }

    fn current_ip(&self) -> InstrId {
        self.ip - 1
    }

    fn increment_ip(&mut self, offset: InstrOffset) -> Result<(), IpOutOfBounds> {
        let err = IpOutOfBounds(self.ip);

        let new_ip = self.ip.checked_add(offset).ok_or(err)?;
        if new_ip > self.opcodes.next_key() {
            Err(err)
        } else {
            self.ip = new_ip;
            Ok(())
        }
    }

    fn decrement_ip(&mut self, offset: InstrOffset) -> Result<(), IpOutOfBounds> {
        let err = IpOutOfBounds(self.ip);

        self.ip = self.ip.checked_sub_offset(offset).ok_or(err)?;
        Ok(())
    }

    fn sync(&mut self) {
        self.register_upvalue.sync(&mut self.core.gc);
        self.stack.lua_frame().sync(&mut self.core.gc);
    }

    fn alloc_literal(&mut self, literal: Literal) -> StrongValue<Ty> {
        match literal {
            Literal::Nil => Value::Nil,
            Literal::Bool(t) => Value::Bool(t),
            Literal::Int(t) => Value::Int(t),
            Literal::Float(t) => Value::Float(t),
            Literal::String(s) => {
                let ptr = self.alloc_string(s.into());
                Value::String(LuaPtr(ptr))
            }
        }
    }

    fn alloc_string(&mut self, s: Ty::String) -> Root<Interned<Ty::String>> {
        match self.core.gc.try_intern(s) {
            Ok(ptr) => ptr,
            Err(value) => {
                self.sync();
                self.core.gc.intern(value)
            }
        }
    }

    fn alloc_table(&mut self) -> RootCell<Ty::Table> {
        match self.core.gc.try_alloc_cell(Default::default()) {
            Ok(ptr) => ptr,
            Err(value) => {
                self.sync();
                self.core.gc.alloc_cell(value)
            }
        }
    }
}

impl<Ty> ActiveFrame<'_, Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    pub(crate) fn eval(&mut self) -> Result<ChangeFrame<Ty>, RefAccessOrError<opcode_err::Error>> {
        let r = loop {
            match self.step() {
                Ok(ControlFlow::Continue(())) => (),
                Ok(ControlFlow::Break(command)) => break Ok(command),
                Err(err) => break Err(err),
            }
        };

        *self.store_ip = self.ip;

        r
    }

    pub(super) fn step(
        &mut self,
    ) -> Result<ControlFlow<ChangeFrame<Ty>>, RefAccessOrError<opcode_err::Error>> {
        let Some(opcode) = self.next_opcode() else {
            return Ok(ControlFlow::Break(ChangeFrame::Return(
                self.stack.lua_frame().next_slot(),
            )));
        };

        self.exec(opcode).map_err(|cause| {
            cause.map_other(|cause| opcode_err::Error {
                fn_ptr: self.core.gc.get_root(self.closure).fn_ptr(),
                ip: self.current_ip(),
                cause,
            })
        })
    }

    fn exec(
        &mut self,
        opcode: OpCode,
    ) -> Result<ControlFlow<ChangeFrame<Ty>>, RefAccessOrError<opcode_err::Cause>> {
        use opcode_err::Cause;
        use repr::opcode::OpCode::*;

        let r = match opcode {
            Panic => return Err(opcode_err::Panic.into()),
            Invoke(slot) => {
                // It is extremely annoying to keep callable on the stack (either caller or callee),
                // however current approach causes stack adjustments on every single fn call.
                // I would like to implement it differently,
                // but somewhat frustratingly this seems to be the simplest workable option.
                // The only other approach I can think of is constructing dedicated callable *stack*
                // (single-value register doesn't work due to nested calls)
                // and make fn invocation into two instructions: StoreCallable + Invoke.
                // Not sure if it will work better.
                let value = self
                    .stack
                    .lua_frame()
                    .remove(slot)
                    .ok_or(MissingStackSlot(slot))?;
                let type_ = value.type_();

                let callable = self.prepare_invoke(value, slot).map_err(|err| match err {
                    AlreadyDroppedOr::Dropped(err) => RefAccessOrError::Dropped(err),
                    AlreadyDroppedOr::Other(_) => {
                        RefAccessOrError::Other(opcode_err::Invoke(type_).into())
                    }
                })?;

                let start = slot;

                ControlFlow::Break(ChangeFrame::Invoke(None, callable, start))
            }
            Return(slot) => ControlFlow::Break(ChangeFrame::Return(slot)),
            MakeClosure(fn_id) => {
                let closure = self.construct_closure(fn_id)?;

                self.stack
                    .lua_frame()
                    .push(Value::Function(Callable::Lua(LuaPtr(closure.downgrade()))));

                ControlFlow::Continue(())
            }
            LoadConstant(index) => {
                let constant = self.get_constant(index)?.clone();
                let value = self.alloc_literal(constant);
                self.stack.lua_frame().push(value.downgrade());

                ControlFlow::Continue(())
            }
            LoadVariadic => {
                self.stack
                    .lua_frame()
                    .extend(self.register_variadic.iter().copied(), true);

                ControlFlow::Continue(())
            }
            LoadStack(slot) => {
                let value = *self.get_stack(slot)?;
                self.stack.lua_frame().push(value);

                ControlFlow::Continue(())
            }
            StoreStack(slot) => {
                let mut stack = self.stack.lua_frame();
                let [value] = stack.take1()?;
                stack.set(slot, value);

                ControlFlow::Continue(())
            }
            AdjustStack(height) => {
                let _ = self.stack.lua_frame().adjust_height(height);

                ControlFlow::Continue(())
            }
            LoadUpvalue(slot) => {
                let value = self
                    .register_upvalue
                    .load(slot)
                    .ok_or(MissingUpvalue(slot))?;
                let mut stack = self.stack.lua_frame();

                // Source tracking is to be removed.
                stack.push_raw(value);

                ControlFlow::Continue(())
            }
            StoreUpvalue(slot) => {
                let mut stack = self.stack.lua_frame();
                let [value] = stack.take1()?;

                self.register_upvalue
                    .store(slot, value)
                    .map_err(|_| MissingUpvalue(slot))?;

                ControlFlow::Continue(())
            }
            UnaOp(op) => {
                let args = self.stack.lua_frame().take1()?;
                self.exec_una_op(args, op)
                    // TODO: Temporary patch, investigate necessity of RefAccessOr
                    .map_err(|err| match err {
                        AlreadyDroppedOr::Dropped(err) => RefAccessOrError::Dropped(err),
                        AlreadyDroppedOr::Other(err) => RefAccessOrError::Other(err.into()),
                    })?
                    .map_br(Into::into)
            }
            BinOp(op) => {
                let args = self.stack.lua_frame().take2()?;
                self.exec_bin_op(args, op)
                    .map_err(|err| match err {
                        AlreadyDroppedOr::Dropped(err) => RefAccessOrError::Dropped(err),
                        AlreadyDroppedOr::Other(err) => RefAccessOrError::Other(err.into()),
                    })?
                    .map_br(Into::into)
            }
            Jump { offset } => {
                self.ip -= InstrOffset(1);
                self.increment_ip(offset)?;

                ControlFlow::Continue(())
            }
            JumpIf { cond, offset } => {
                let [value] = self.stack.lua_frame().take1()?;

                if value.to_bool() == cond {
                    self.ip -= InstrOffset(1);
                    self.increment_ip(offset)?;
                }

                ControlFlow::Continue(())
            }
            Loop { offset } => {
                self.ip -= InstrOffset(1);
                self.decrement_ip(offset)?;

                ControlFlow::Continue(())
            }
            TabCreate => {
                let value = self.alloc_table().downgrade();

                self.stack.lua_frame().push(Value::Table(LuaPtr(value)));

                ControlFlow::Continue(())
            }
            TabGet => {
                let args = self.stack.lua_frame().take2()?;
                self.exec_tab_get(args)
                    .map_err(|err| err.map_other(Cause::TabGet))?
                    .map_br(|(callable, start)| {
                        ChangeFrame::Invoke(Some(Event::Index), callable, start)
                    })
            }
            TabSet => {
                let args = self.stack.lua_frame().take3()?;
                self.exec_tab_set(args)
                    .map_err(|err| err.map_other(Cause::TabSet))?
                    .map_br(|(callable, start)| {
                        ChangeFrame::Invoke(Some(Event::NewIndex), callable, start)
                    })
            }
        };

        tracing::trace!(
            stack = self.stack.to_pretty_string(&self.core.gc),
            "executed opcode"
        );

        Ok(r)
    }

    fn exec_una_op(
        &mut self,
        args: [WeakValue<Ty>; 1],
        op: UnaOp,
    ) -> Result<ControlFlow<Invoke<Ty>>, AlreadyDroppedOr<opcode_err::UnaOpCause>> {
        use crate::builtins::coerce::CoerceArgs;
        use crate::builtins::find_metavalue;
        use crate::builtins::raw::{unary_op, MetamethodRequired};
        use ControlFlow::*;

        let [val] = &args;
        let err = opcode_err::UnaOpCause { arg: val.type_() };

        let args = self.core.dialect.unary_op(op, args);
        let eval = match (op, args) {
            // Lua spec dictates that len op (`#`) on table must try metamethod before raw builtin.
            (UnaOp::StrLen, [Value::Table(_)]) => ControlFlow::Break(MetamethodRequired),
            (op, args) => unary_op(op, args, &self.core.gc)?,
        };

        match eval {
            Continue(value) => {
                self.stack.lua_frame().push(value);
                Ok(Continue(()))
            }
            Break(MetamethodRequired) => {
                let event = match op {
                    UnaOp::AriNeg => Event::Neg,
                    UnaOp::BitNot => Event::BitNot,
                    UnaOp::StrLen => Event::Len,
                    UnaOp::LogNot => unreachable!("logical NOT should always resolve a value"),
                };

                let key = self.internal_cache.lookup_event(event);
                let metavalue =
                    find_metavalue(args, key, &self.core.gc, &self.core.metatable_registry)?;

                match metavalue {
                    Value::Nil => match (op, args) {
                        // Trigger table len builtin on failed metamethod lookup.
                        (UnaOp::StrLen, [Value::Table(LuaPtr(tab))]) => {
                            let border = self.core.gc.get(tab).ok_or(AlreadyDroppedError)?.border();
                            self.stack.lua_frame().push(Value::Int(border));

                            Ok(Continue(()))
                        }
                        _ => Err(AlreadyDroppedOr::Other(err)),
                    },
                    metavalue => {
                        let start = self.stack.next_slot();
                        let [arg] = args;
                        self.stack.lua_frame().push(arg);

                        let callable = self
                            .prepare_invoke(metavalue, start)
                            .map_err(|e| e.map_other(|_| err))?;

                        Ok(Break(Invoke(event, callable, start)))
                    }
                }
            }
        }
    }

    fn exec_bin_op(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: BinOp,
    ) -> Result<std::ops::ControlFlow<Invoke<Ty>>, AlreadyDroppedOr<opcode_err::BinOpCause>> {
        use crate::builtins::coerce::CoerceArgs;
        use crate::builtins::find_metavalue;
        use crate::builtins::raw::{inner_binary_op, AsHeap, Intern, MetamethodRequired};

        struct Inner<'a, 'rt, Ty>(&'a mut ActiveFrame<'rt, Ty>)
        where
            Ty: Types;

        impl<Ty> AsHeap<Ty> for Inner<'_, '_, Ty>
        where
            Ty: Types,
        {
            fn as_heap(&self) -> &Heap<Ty> {
                &self.0.core.gc
            }
        }

        impl<Ty> Intern<Ty::String> for Inner<'_, '_, Ty>
        where
            Ty: Types,
        {
            fn intern(&mut self, value: Ty::String) -> Root<Interned<Ty::String>> {
                self.0.alloc_string(value)
            }
        }

        let err = opcode_err::BinOpCause {
            lhs: args[0].type_(),
            rhs: args[1].type_(),
        };

        let coerce_policy = self.core.dialect;
        let args = coerce_policy.binary_op(op, args, |value| self.alloc_string(value));
        let cmp_int_flt = CoerceArgs::<Ty>::cmp_float_and_int(&coerce_policy);
        let eval = inner_binary_op(op, args, cmp_int_flt, &mut Inner(self))?;

        match eval {
            ControlFlow::Continue(Some(value)) => {
                // Value contains no reference in this case.
                self.stack.lua_frame().push(value);
                Ok(ControlFlow::Continue(()))
            }
            ControlFlow::Continue(None) => Err(AlreadyDroppedOr::Other(err)),
            ControlFlow::Break(MetamethodRequired) => {
                let event: Event = op.into();

                // Swap arguments for greater/greater-or-eq comparisons.
                // Those desugar into Lt/LtEq metamethods with swapped arguments.
                let args = match op {
                    BinOp::Rel(RelBinOp::Gt | RelBinOp::GtEq) => {
                        let [rhs, lhs] = args;
                        [lhs, rhs]
                    }
                    _ => args,
                };

                let key = self.internal_cache.lookup_event(event);
                let metavalue =
                    find_metavalue(args, key, &self.core.gc, &self.core.metatable_registry)?;

                match metavalue {
                    Value::Nil => {
                        // Fallback on raw comparison for (in)equality in case metamethod is not found.
                        let fallback_result = match op {
                            BinOp::Eq(eq_op) => {
                                let [lhs, rhs] = args;
                                match eq_op {
                                    EqBinOp::Eq => Some(Value::Bool(lhs == rhs)),
                                    EqBinOp::Neq => Some(Value::Bool(lhs != rhs)),
                                }
                            }
                            _ => None,
                        };

                        if let Some(r) = fallback_result {
                            self.stack.lua_frame().push(r);
                            Ok(ControlFlow::Continue(()))
                        } else {
                            Err(AlreadyDroppedOr::Other(err))
                        }
                    }
                    metavalue => {
                        let start = self.stack.next_slot();
                        self.stack.lua_frame().extend(args, false);

                        let callable = metavalue;
                        let callable = self
                            .prepare_invoke(callable, start)
                            .map_err(|e| e.map_other(|_| err))?;

                        Ok(ControlFlow::Break(Invoke(event, callable, start)))
                    }
                }
            }
        }
    }

    #[allow(clippy::type_complexity)]
    fn exec_tab_get(
        &mut self,
        args: [WeakValue<Ty>; 2],
    ) -> Result<
        ControlFlow<(Callable<Strong, Ty>, StackSlot)>,
        RefAccessOrError<opcode_err::TabCause>,
    > {
        use crate::builtins::coerce::CoerceArgs;
        use opcode_err::TabCause::*;
        use ControlFlow::*;

        let [mut table, index] = args;

        'outer: loop {
            // First: try raw table access.
            if let Value::Table(LuaPtr(table)) = &table {
                let index = self.core.dialect.tab_get(index);

                let key = index.try_into().map_err(InvalidKey)?;
                let table = *table;
                let value = self
                    .core
                    .gc
                    .get(table)
                    .ok_or(AlreadyDroppedError)?
                    .get(&key);

                // It succeeds if any non-nil value is produced.
                if value != Value::Nil {
                    // We know that the value originated from the table in this slot.
                    let mut stack = self.stack.lua_frame();
                    stack.push(value);
                    return Ok(Continue(()));
                }
            };

            // Second: try detecting compatible metavalue
            let key = self.internal_cache.lookup_event(Event::Index);

            loop {
                let metavalue = self
                    .core
                    .metatable_of(&table)?
                    .map(|mt| {
                        self.core
                            .gc
                            .get(mt)
                            .ok_or(AlreadyDroppedError)
                            .map(|table| table.get(&key))
                    })
                    .transpose()?
                    .unwrap_or_default();

                match metavalue {
                    // Keyes associated with nil are considered to be absent from table.
                    Value::Nil => {
                        if matches!(table, Value::Table(_)) {
                            // Third: Fallback to producing nil.
                            // This can only be reached if raw table access returned nil and there is no metamethod.

                            self.stack.lua_frame().push(Value::Nil);
                            return Ok(Continue(()));
                        } else {
                            return Err(TableTypeMismatch(table.type_()).into());
                        }
                    }
                    // Table simply goes on another spin of recursive table lookup.
                    tab @ Value::Table(_) => {
                        table = tab;
                        continue 'outer;
                    }
                    // Function is invoked with table and *original* (before coercions!) index.
                    // It only picks up the latest "table" value which metamethod we tried to look up.
                    Value::Function(callable) => {
                        let callable =
                            callable.upgrade(&self.core.gc).ok_or(AlreadyDroppedError)?;
                        let start = self.stack.next_slot();
                        let mut stack = self.stack.lua_frame();
                        stack.push(table);
                        stack.push(index);

                        return Ok(Break((callable, start)));
                    }
                    // If everything fails, try to recursively lookup metamethod in the new value.
                    // Lua affirms to ignore function-like (e.g. with __call metamethod) objects in table indexing.
                    value => {
                        table = value;
                    }
                }
            }
        }
    }

    #[allow(clippy::type_complexity)]
    fn exec_tab_set(
        &mut self,
        args: [WeakValue<Ty>; 3],
    ) -> Result<
        ControlFlow<(Callable<Strong, Ty>, StackSlot)>,
        RefAccessOrError<opcode_err::TabCause>,
    > {
        use crate::builtins::coerce::CoerceArgs;
        use opcode_err::TabCause::*;
        use ControlFlow::*;

        let [mut table, index, value] = args;

        'outer: loop {
            // First: try raw table access.
            if let Value::Table(LuaPtr(table)) = &table {
                let index = self.core.dialect.tab_set(index);
                let key = index.try_into().map_err(InvalidKey)?;
                let table = self.core.gc.get_mut(*table).ok_or(AlreadyDroppedError)?;

                // It succeeds if key is already populated.
                let r = if table.contains_key(&key) {
                    Break(())
                } else {
                    Continue(())
                };

                if matches!(r, Break(())) {
                    table.set(key, value);
                    return Ok(Continue(()));
                }
            };

            // Second: try detecting compatible metavalue
            let key = self.internal_cache.lookup_event(Event::NewIndex);
            loop {
                let metavalue = self
                    .core
                    .metatable_of(&table)?
                    .map(|mt| {
                        self.core
                            .gc
                            .get(mt)
                            .ok_or(AlreadyDroppedError)
                            .map(|table| table.get(&key))
                    })
                    .transpose()?
                    .unwrap_or_default();

                match metavalue {
                    // Keyes associated with nil are considered to be absent from table.
                    Value::Nil => {
                        if let Value::Table(LuaPtr(table)) = table {
                            // Third: Fallback to raw assignment.
                            // This can only be reached if raw table access returned nil and there is no metamethod.

                            let index = self.core.dialect.tab_set(index);
                            let key = index.try_into().map_err(InvalidKey)?;

                            self.core
                                .gc
                                .get_mut(table)
                                .ok_or(AlreadyDroppedError)?
                                .set(key, value);

                            return Ok(Continue(()));
                        } else {
                            return Err(TableTypeMismatch(table.type_()).into());
                        }
                    }
                    // Table simply goes on another spin of recursive table assignment.
                    tab @ Value::Table(_) => {
                        table = tab;
                        continue 'outer;
                    }
                    // Function is invoked with table, *original* (before coercions!) index and value.
                    // It only picks up the latest "table" value which metamethod we tried to look up.
                    Value::Function(callable) => {
                        let callable =
                            callable.upgrade(&self.core.gc).ok_or(AlreadyDroppedError)?;
                        let start = self.stack.next_slot();
                        let mut stack = self.stack.lua_frame();
                        stack.push(table);
                        stack.push(index);
                        stack.push(value);

                        return Ok(Break((callable, start)));
                    }
                    // If everything fails, try to recursively lookup metamethod in the new value.
                    // Lua affirms to ignore function-like (e.g. with __call metamethod) objects in table indexing.
                    value => {
                        table = value;
                    }
                }
            }
        }
    }

    fn prepare_invoke(
        &mut self,
        callable: WeakValue<Ty>,
        start: StackSlot,
    ) -> Result<Callable<Strong, Ty>, AlreadyDroppedOr<NotCallableError>> {
        use crate::builtins::inner_prepare_invoke;

        let key = self.internal_cache.lookup_event(Event::Call);
        let mut stack = self.stack.guard_at(start).unwrap();
        let stack = stack.transient();

        inner_prepare_invoke(
            callable,
            stack,
            &self.core.gc,
            &self.core.metatable_registry,
            key,
        )
    }

    pub fn next_opcode(&mut self) -> Option<OpCode> {
        let r = *self.opcodes.get(self.ip)?;

        tracing::trace!(ip = self.ip.0, opcode = %r, "next opcode");

        self.ip += 1;

        Some(r)
    }

    pub(crate) fn construct_closure(
        &mut self,
        recipe_id: RecipeId,
    ) -> Result<Root<Closure<Ty>>, opcode_err::Cause> {
        use crate::runtime::FunctionPtr;

        let recipe = self
            .recipes
            .get(recipe_id)
            .ok_or(opcode_err::MissingRecipe(recipe_id))?;

        let ClosureRecipe {
            function_id,
            upvalues,
        } = recipe;

        let chunk_id = self.core.gc[self.closure].fn_ptr().chunk_id;

        let fn_ptr = FunctionPtr {
            chunk_id,
            function_id: *function_id,
        };

        let stack = self.stack.lua_frame();
        let closure = &self.core.gc[self.closure];
        let upvalues = upvalues
            .iter()
            .map(|&source| -> Result<_, opcode_err::Cause> {
                use repr::chunk::UpvalueSource;

                match source {
                    UpvalueSource::Temporary(slot) => {
                        let _ = stack.get_slot(slot).ok_or(MissingStackSlot(slot));
                        Ok(UpvaluePlace::Stack(stack.boundary() + slot))
                    }
                    UpvalueSource::Upvalue(slot) => closure
                        .upvalues()
                        .get(slot)
                        .copied()
                        .ok_or(opcode_err::MissingUpvalue(slot).into()),
                }
            })
            .collect::<Result<_, _>>()?;

        let closure = Closure::from_raw_parts(fn_ptr, self.current_thread_id, upvalues);

        let closure = match self.core.gc.try_alloc(closure) {
            Ok(ptr) => ptr,
            Err(closure) => {
                // All Gc references inside the closure are copies of upvalues of current frame
                // which are going to be rooted in the process.
                self.sync();
                self.core.gc.alloc(closure)
            }
        };

        self.stack
            .lua_frame()
            .register_closure(&closure, &self.core.gc);

        Ok(closure)
    }
}

impl<Ty> Debug for ActiveFrame<'_, Ty>
where
    Ty: Debug + Types,
    WeakValue<Ty>: Debug,
    StrongValue<Ty>: Debug,
    Ty::Table: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ActiveFrame")
            .field("current_thread_id", &self.current_thread_id)
            .field("core", &self.core)
            .field("internal_cache", &self.internal_cache)
            .field("recipes", &self.recipes)
            .field("constants", &self.constants)
            .field("opcodes", &self.opcodes)
            .field("closure", &self.closure)
            .field("store_ip", &self.store_ip)
            .field("ip", &self.ip)
            .field("stack", &self.stack)
            .field("register_upvalue", &self.register_upvalue)
            .field("register_variadic", &self.register_variadic)
            .finish()
    }
}

#[derive(Debug)]
pub(super) enum RefAccessOrError<E> {
    Dropped(AlreadyDroppedError),
    Borrowed(BorrowError),
    Other(E),
}

impl<E> RefAccessOrError<E> {
    fn map_other<T>(self, f: impl FnOnce(E) -> T) -> RefAccessOrError<T> {
        use RefAccessOrError::*;

        match self {
            Borrowed(err) => Borrowed(err),
            Dropped(err) => Dropped(err),
            Other(err) => Other(f(err)),
        }
    }
}

impl<E> From<RefAccessError> for RefAccessOrError<E> {
    fn from(value: RefAccessError) -> Self {
        match value {
            RefAccessError::Dropped(err) => RefAccessOrError::Dropped(err),
            RefAccessError::Borrowed(err) => RefAccessOrError::Borrowed(err),
        }
    }
}

impl<E> From<AlreadyDroppedError> for RefAccessOrError<E> {
    fn from(value: AlreadyDroppedError) -> Self {
        RefAccessOrError::Dropped(value)
    }
}

impl<E> From<BorrowError> for RefAccessOrError<E> {
    fn from(value: BorrowError) -> Self {
        RefAccessOrError::Borrowed(value)
    }
}

impl<T> From<T> for RefAccessOrError<opcode_err::Cause>
where
    T: Into<opcode_err::Cause>,
{
    fn from(value: T) -> Self {
        RefAccessOrError::Other(value.into())
    }
}

impl<T> From<T> for RefAccessOrError<opcode_err::UnaOpCause>
where
    T: Into<opcode_err::UnaOpCause>,
{
    fn from(value: T) -> Self {
        RefAccessOrError::Other(value.into())
    }
}

impl<T> From<T> for RefAccessOrError<opcode_err::BinOpCause>
where
    T: Into<opcode_err::BinOpCause>,
{
    fn from(value: T) -> Self {
        RefAccessOrError::Other(value.into())
    }
}

impl<T> From<T> for RefAccessOrError<opcode_err::TabCause>
where
    T: Into<opcode_err::TabCause>,
{
    fn from(value: T) -> Self {
        RefAccessOrError::Other(value.into())
    }
}

impl<E, Value> From<RefAccessOrError<E>> for RuntimeError<Value>
where
    E: Into<RuntimeError<Value>>,
{
    fn from(value: RefAccessOrError<E>) -> Self {
        match value {
            RefAccessOrError::Dropped(err) => err.into(),
            RefAccessOrError::Borrowed(err) => err.into(),
            RefAccessOrError::Other(err) => err.into(),
        }
    }
}
