mod upvalue_register;
mod variadic_register;

use std::fmt::Debug;
use std::ops::ControlFlow;

use gc::{Root, RootCell};
use repr::chunk::{Chunk, ClosureRecipe};
use repr::index::{ConstId, InstrId, InstrOffset, RecipeId, StackSlot};
use repr::literal::Literal;
use repr::opcode::{AriBinOp, BinOp, BitBinOp, EqBinOp, OpCode, RelBinOp, StrBinOp, UnaOp};
use repr::tivec::TiSlice;

use super::super::stack::StackGuard;
use crate::backtrace::BacktraceFrame;
use crate::chunk_cache::ChunkCache;
use crate::error::opcode::{
    self as opcode_err, IpOutOfBounds, MissingConstId, MissingStackSlot, MissingUpvalue,
};
use crate::error::{
    AlreadyDroppedError, BorrowError, MalformedClosureError, RefAccessError, RuntimeError,
};
use crate::gc::{DisplayWith, Heap, LuaPtr};
use crate::runtime::closure::UpvaluePlace;
use crate::runtime::orchestrator::ThreadStore;
use crate::runtime::{Closure, Core, Interned, ThreadId};
use crate::value::callable::Callable;
use crate::value::{Concat, Len, Strong, StrongValue, TableIndex, Types, Value, WeakValue};

use super::Event;
pub(crate) use upvalue_register::UpvalueRegister;
use variadic_register::VariadicRegister;

pub(crate) struct Context<'a, Ty>
where
    Ty: Types,
{
    pub(crate) core: &'a mut Core<Ty>,
    pub(crate) chunk_cache: &'a dyn ChunkCache,
    pub(crate) current_thread_id: ThreadId,
    pub(crate) thread_store: &'a mut ThreadStore<Ty>,
    pub(crate) stack: StackGuard<'a, Ty>,
    pub(crate) upvalues: &'a mut UpvalueRegister<Ty>,
}

impl<'a, Ty> Context<'a, Ty>
where
    Ty: Types,
{
    pub(crate) fn reborrow(&mut self) -> Context<'_, Ty> {
        let Context {
            core,
            chunk_cache,
            current_thread_id,
            thread_store,
            stack,
            upvalues,
        } = self;

        Context {
            core,
            chunk_cache: *chunk_cache,
            current_thread_id: *current_thread_id,
            thread_store,
            stack: stack.reborrow(),
            upvalues,
        }
    }
}

pub(crate) enum ChangeFrame<Ty>
where
    Ty: Types,
{
    Return(StackSlot),
    Invoke(Option<Event>, Callable<Strong, Ty>, StackSlot),
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
    closure: RootCell<Closure<Ty>>,
    ip: InstrId,
    register_variadic: VariadicRegister<Ty>,
}

impl<Ty> Frame<Ty>
where
    Ty: Types,
{
    pub(crate) fn new(
        closure: RootCell<Closure<Ty>>,
        heap: &mut Heap<Ty>,
        chunk_cache: &dyn ChunkCache,
        mut stack: StackGuard<Ty>,
    ) -> Result<Self, MalformedClosureError> {
        use crate::error::{MissingChunk, MissingFunction, UpvalueCountMismatch};
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
            let err = UpvalueCountMismatch {
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
    pub(crate) fn closure(&self) -> &RootCell<Closure<Ty>> {
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
            ip,
            register_variadic,
        } = self;

        let constants = &chunk.constants;
        let opcodes = &function.opcodes;
        // let stack = stack.guard(stack_start).unwrap();

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
            closure,
            chunk,
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
    closure: &'rt RootCell<Closure<Ty>>,
    core: &'rt mut Core<Ty>,
    chunk: &'rt Chunk,
    constants: &'rt TiSlice<ConstId, Literal>,
    opcodes: &'rt TiSlice<InstrId, OpCode>,
    // Not sure about this one.
    // It might be better to keep a copy internally and only sync when frame is deactivated.
    ip: &'rt mut InstrId,
    stack: StackGuard<'rt, Ty>,
    register_upvalue: &'rt mut UpvalueRegister<Ty>,
    register_variadic: &'rt [WeakValue<Ty>],
}

impl<'rt, Ty> ActiveFrame<'rt, Ty>
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
        *self.ip - 1
    }

    fn increment_ip(&mut self, offset: InstrOffset) -> Result<(), IpOutOfBounds> {
        let err = IpOutOfBounds(*self.ip);

        let new_ip = self.ip.checked_add(offset).ok_or(err)?;
        if new_ip > self.opcodes.next_key() {
            Err(err)
        } else {
            *self.ip = new_ip;
            Ok(())
        }
    }

    fn decrement_ip(&mut self, offset: InstrOffset) -> Result<(), IpOutOfBounds> {
        let err = IpOutOfBounds(*self.ip);

        *self.ip = self.ip.checked_sub_offset(offset).ok_or(err)?;
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
        match self.core.string_interner.try_insert(s, &mut self.core.gc) {
            Ok(ptr) => ptr,
            Err(value) => {
                self.sync();
                self.core.string_interner.insert(value, &mut self.core.gc)
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

impl<'rt, Ty> ActiveFrame<'rt, Ty>
where
    Ty: Types<LuaClosure = Closure<Ty>>,
    WeakValue<Ty>: DisplayWith<Heap<Ty>>,
{
    pub(crate) fn enter(&mut self) -> Result<ChangeFrame<Ty>, RefAccessOrError<opcode_err::Error>> {
        loop {
            match self.step() {
                Ok(ControlFlow::Continue(())) => (),
                Ok(ControlFlow::Break(command)) => break Ok(command),
                Err(err) => break Err(err),
            }
        }
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

                let callable = self
                    .prepare_invoke(value, slot)
                    .map_err(|err| err.map_other(|_| opcode_err::Invoke(type_).into()))?;

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
                    .map_err(|err| err.map_other(Into::into))?
                    .map_br(|(event, callable, start)| {
                        ChangeFrame::Invoke(Some(event), callable, start)
                    })
            }
            BinOp(op) => {
                let args = self.stack.lua_frame().take2()?;
                self.exec_bin_op(args, op)
                    .map_err(|err| err.map_other(Into::into))?
                    .map_br(|(event, callable, start)| {
                        ChangeFrame::Invoke(Some(event), callable, start)
                    })
            }
            Jump { offset } => {
                *self.ip -= InstrOffset(1);
                self.increment_ip(offset)?;

                ControlFlow::Continue(())
            }
            JumpIf { cond, offset } => {
                let [value] = self.stack.lua_frame().take1()?;

                if value.to_bool() == cond {
                    *self.ip -= InstrOffset(1);
                    self.increment_ip(offset)?;
                }

                ControlFlow::Continue(())
            }
            Loop { offset } => {
                *self.ip -= InstrOffset(1);
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

    #[allow(clippy::type_complexity)]
    fn exec_una_op(
        &mut self,
        args: [WeakValue<Ty>; 1],
        op: UnaOp,
    ) -> Result<
        ControlFlow<(Event, Callable<Strong, Ty>, StackSlot)>,
        RefAccessOrError<opcode_err::UnaOpCause>,
    > {
        use crate::value::{Float, Int};
        use ControlFlow::*;

        let [val] = &args;

        let err = opcode_err::UnaOpCause { arg: val.type_() };

        let eval = match op {
            UnaOp::AriNeg => match args {
                [Value::Int(val)] => Continue((-Int(val)).into()),
                [Value::Float(val)] => Continue((-Float(val)).into()),
                args => Break((Event::Neg, args)),
            },
            UnaOp::BitNot => {
                use crate::runtime::CoerceArgs;

                let args = self.core.dialect.coerce_una_op_bit(args);

                match args {
                    [Value::Int(val)] => Continue((!Int(val)).into()),
                    args => Break((Event::BitNot, args)),
                }
            }
            UnaOp::StrLen => {
                match args {
                    [Value::String(LuaPtr(val))] => {
                        let val = self.core.gc.get(val).ok_or(AlreadyDroppedError)?;
                        let len = val.len().try_into().unwrap();

                        Continue(Value::Int(len))
                    }
                    // Table builtin triggers after metamethod attempt.
                    args => Break((Event::Len, args)),
                }
            }
            UnaOp::LogNot => {
                let [arg] = args;
                let r = Value::Bool(!arg.to_bool());
                self.stack.lua_frame().push(r);

                return Ok(Continue(()));
            }
        };

        match eval {
            Continue(value) => {
                // Value contains no reference in this case.
                self.stack.lua_frame().push(value);
                Ok(Continue(()))
            }
            Break((event, args)) => {
                let [arg] = &args;

                let metatable = self.core.metatable_of(arg)?;
                let key = self.core.lookup_event(event);
                let heap = &self.core.gc;
                let metavalue = metatable
                    .map(|mt| {
                        heap.get(mt)
                            .ok_or(AlreadyDroppedError)
                            .map(|table| table.get(&key))
                    })
                    .transpose()?
                    .unwrap_or_default();

                match metavalue {
                    Value::Nil => match (op, args) {
                        // Trigger table len builtin on failed metamethod lookup.
                        (UnaOp::StrLen, [Value::Table(LuaPtr(tab))]) => {
                            let border = heap.get(tab).ok_or(AlreadyDroppedError)?.border();
                            self.stack.lua_frame().push(Value::Int(border));

                            Ok(Continue(()))
                        }
                        _ => Err(err.into()),
                    },
                    metavalue => {
                        let start = self.stack.next_slot();

                        let [arg] = args;
                        let mut stack = self.stack.lua_frame();
                        stack.push(arg);
                        let callable = metavalue;
                        let callable = self
                            .prepare_invoke(callable, start)
                            .map_err(|e| e.map_other(|_| err))?;

                        Ok(Break((event, callable, start)))
                    }
                }
            }
        }
    }

    #[allow(clippy::type_complexity)]
    fn exec_bin_op(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: BinOp,
    ) -> Result<
        std::ops::ControlFlow<(Event, Callable<Strong, Ty>, StackSlot)>,
        RefAccessOrError<opcode_err::BinOpCause>,
    > {
        let err = opcode_err::BinOpCause {
            lhs: args[0].type_(),
            rhs: args[1].type_(),
        };

        let eval = match op {
            BinOp::Ari(op) => self.exec_bin_op_ari(args, op),
            BinOp::Bit(op) => self.exec_bin_op_bit(args, op),
            BinOp::Eq(op) => self.exec_bin_op_eq(args, op),
            BinOp::Rel(op) => self.exec_bin_op_rel(args, op)?,
            BinOp::Str(op) => self.exec_bin_op_str(args, op)?,
        };

        match eval {
            ControlFlow::Continue(Some(value)) => {
                // Value contains no reference in this case.
                self.stack.lua_frame().push(value);
                Ok(ControlFlow::Continue(()))
            }
            ControlFlow::Continue(None) => Err(err.into()),
            ControlFlow::Break(args) => {
                let event: Event = op.into();

                // Swap arguments for greater/greater-or-eq comparisons.
                // Those desugar into Lt/LtEq metamethods with swapped arguments.
                let [lhs, rhs] = match op {
                    BinOp::Rel(RelBinOp::Gt | RelBinOp::GtEq) => {
                        let [rhs, lhs] = args;
                        [lhs, rhs]
                    }
                    _ => args,
                };

                let key = self.core.lookup_event(event);
                let lookup_event = |value: &WeakValue<Ty>| -> Result<_, AlreadyDroppedError> {
                    let r = self
                        .core
                        .metatable_of(value)?
                        .map(|mt| {
                            self.core
                                .gc
                                .get(mt)
                                .ok_or(AlreadyDroppedError)
                                .map(|table| table.get(&key))
                        })
                        .transpose()?
                        .unwrap_or_default();

                    Ok(r)
                };

                let metavalue = match lookup_event(&lhs)? {
                    Value::Nil => lookup_event(&rhs)?,
                    value => value,
                };

                match metavalue {
                    Value::Nil => {
                        // Fallback on raw comparison for (in)equality in case metamethod is not found.
                        let fallback_result = match op {
                            BinOp::Eq(eq_op) => match eq_op {
                                EqBinOp::Eq => Some(Value::Bool(lhs == rhs)),
                                EqBinOp::Neq => Some(Value::Bool(lhs != rhs)),
                            },
                            _ => None,
                        };

                        if let Some(r) = fallback_result {
                            self.stack.lua_frame().push(r);
                            Ok(ControlFlow::Continue(()))
                        } else {
                            Err(err.into())
                        }
                    }
                    metavalue => {
                        let start = self.stack.next_slot();
                        let mut stack = self.stack.lua_frame();
                        stack.push(lhs);
                        stack.push(rhs);

                        let callable = metavalue;
                        let callable = self
                            .prepare_invoke(callable, start)
                            .map_err(|e| e.map_other(|_| err))?;

                        Ok(ControlFlow::Break((event, callable, start)))
                    }
                }
            }
        }
    }

    #[allow(clippy::type_complexity)]
    fn exec_bin_op_str(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: StrBinOp,
    ) -> Result<ControlFlow<[WeakValue<Ty>; 2], Option<WeakValue<Ty>>>, RefAccessError> {
        use crate::runtime::dialect::CoerceArgs;

        let dialect = self.core.dialect;
        let args = dialect.coerce_bin_op_str(op, args, |value| self.alloc_string(value));

        match args {
            [Value::String(LuaPtr(lhs)), Value::String(LuaPtr(rhs))] => match op {
                StrBinOp::Concat => {
                    let mut r = self
                        .core
                        .gc
                        .get(lhs)
                        .ok_or(AlreadyDroppedError)?
                        .as_ref()
                        .clone();
                    let rhs = self.core.gc.get(rhs).ok_or(AlreadyDroppedError)?;
                    r.concat(rhs.as_ref());

                    let s = self.alloc_string(r).downgrade();

                    Ok(ControlFlow::Continue(Some(Value::String(LuaPtr(s)))))
                }
            },
            args => Ok(ControlFlow::Break(args)),
        }
    }

    fn exec_bin_op_eq(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: EqBinOp,
    ) -> ControlFlow<[WeakValue<Ty>; 2], Option<WeakValue<Ty>>> {
        use crate::runtime::dialect::CoerceArgs;
        use crate::value::{Float, Int};
        use EqBinOp::*;

        let cmp = <_ as CoerceArgs<Ty>>::cmp_float_and_int(&self.core.dialect);

        let equal = match args {
            [Value::Int(lhs), Value::Float(rhs)] if cmp => Int(lhs) == Float(rhs),
            [Value::Float(lhs), Value::Int(rhs)] if cmp => Float(lhs) == Int(rhs),

            [Value::Table(lhs), Value::Table(rhs)] if lhs != rhs => {
                let args = [Value::Table(lhs), Value::Table(rhs)];
                return ControlFlow::Break(args);
            }

            [lhs, rhs] => lhs == rhs,
        };

        let r = match op {
            Eq => equal,
            Neq => !equal,
        };

        ControlFlow::Continue(Some(Value::Bool(r)))
    }

    #[allow(clippy::type_complexity)]
    fn exec_bin_op_rel(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: RelBinOp,
    ) -> Result<ControlFlow<[WeakValue<Ty>; 2], Option<WeakValue<Ty>>>, RefAccessError> {
        use crate::runtime::dialect::CoerceArgs;
        use crate::value;
        use RelBinOp::*;
        use Value::*;

        let cmp = <_ as CoerceArgs<Ty>>::cmp_float_and_int(&self.core.dialect);

        let ord = match &args {
            [Int(lhs), Int(rhs)] => PartialOrd::partial_cmp(lhs, rhs),
            [Float(lhs), Float(rhs)] => PartialOrd::partial_cmp(lhs, rhs),
            [String(LuaPtr(lhs)), String(LuaPtr(rhs))] => {
                let lhs = self.core.gc.get(*lhs).ok_or(AlreadyDroppedError)?;
                let rhs = self.core.gc.get(*rhs).ok_or(AlreadyDroppedError)?;

                PartialOrd::partial_cmp(lhs, rhs)
            }
            [Int(lhs), Float(rhs)] if cmp => {
                PartialOrd::partial_cmp(&value::Int(*lhs), &value::Float(*rhs))
            }
            [Float(lhs), Int(rhs)] if cmp => {
                PartialOrd::partial_cmp(&value::Float(*lhs), &value::Int(*rhs))
            }
            _ => None,
        };

        let Some(ord) = ord else {
            return Ok(ControlFlow::Break(args));
        };

        let r = match op {
            Lt => ord.is_lt(),
            LtEq => ord.is_le(),
            Gt => ord.is_gt(),
            GtEq => ord.is_ge(),
        };

        Ok(ControlFlow::Continue(Some(Value::Bool(r))))
    }

    fn exec_bin_op_bit(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: BitBinOp,
    ) -> ControlFlow<[WeakValue<Ty>; 2], Option<WeakValue<Ty>>> {
        use crate::runtime::dialect::CoerceArgs;
        use crate::value::Int;

        let args = self.core.dialect.coerce_bin_op_bit(op, args);

        let r = match args {
            [Value::Int(lhs), Value::Int(rhs)] => match op {
                BitBinOp::And => (Int(lhs) & Int(rhs)).into(),
                BitBinOp::Or => (Int(lhs) | Int(rhs)).into(),
                BitBinOp::Xor => (Int(lhs) ^ Int(rhs)).into(),
                BitBinOp::ShL => (Int(lhs) << Int(rhs)).into(),
                BitBinOp::ShR => (Int(lhs) >> Int(rhs)).into(),
            },
            args => return ControlFlow::Break(args),
        };

        ControlFlow::Continue(Some(r))
    }

    fn exec_bin_op_ari(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: AriBinOp,
    ) -> ControlFlow<[WeakValue<Ty>; 2], Option<WeakValue<Ty>>> {
        use crate::runtime::dialect::CoerceArgs;
        use crate::value::{Float, Int};
        use AriBinOp::*;

        // Coercions.
        let args = self.core.dialect.coerce_bin_op_ari(op, args);

        let r = match args {
            [Value::Int(lhs), Value::Int(rhs)] => match op {
                Add => Some((Int(lhs) + Int(rhs)).into()),
                Sub => Some((Int(lhs) - Int(rhs)).into()),
                Mul => Some((Int(lhs) * Int(rhs)).into()),
                Div | FloorDiv | Rem if rhs == 0 => None,
                Rem => Some((Int(lhs) % Int(rhs)).into()),
                Div => Some((Int(lhs) / Int(rhs)).into()),
                FloorDiv => Some(Int(lhs).floor_div(Int(rhs)).into()),
                Pow => Int(lhs).exp(Int(rhs)).map(Into::into),
            },
            [Value::Float(lhs), Value::Float(rhs)] => match op {
                Add => Some((Float(lhs) + Float(rhs)).into()),
                Sub => Some((Float(lhs) - Float(rhs)).into()),
                Mul => Some((Float(lhs) * Float(rhs)).into()),
                Div => Some((Float(lhs) / Float(rhs)).into()),
                FloorDiv => Some(Float(lhs).floor_div(Float(rhs)).into()),
                Rem => Some((Float(lhs) % Float(rhs)).into()),
                Pow => Some(Float(lhs).exp(Float(rhs)).into()),
            },
            args => return ControlFlow::Break(args),
        };

        ControlFlow::Continue(r)
    }

    #[allow(clippy::type_complexity)]
    fn exec_tab_get(
        &mut self,
        args: [WeakValue<Ty>; 2],
    ) -> Result<
        ControlFlow<(Callable<Strong, Ty>, StackSlot)>,
        RefAccessOrError<opcode_err::TabCause>,
    > {
        use crate::runtime::dialect::CoerceArgs;
        use opcode_err::TabCause::*;
        use ControlFlow::*;

        let [mut table, index] = args;

        'outer: loop {
            // First: try raw table access.
            if let Value::Table(LuaPtr(table)) = &table {
                let index = self.core.dialect.coerce_tab_get(index);

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
            let key = self.core.lookup_event(Event::Index);

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
        use crate::runtime::dialect::CoerceArgs;
        use opcode_err::TabCause::*;
        use ControlFlow::*;

        let [mut table, index, value] = args;

        'outer: loop {
            // First: try raw table access.
            if let Value::Table(LuaPtr(table)) = &table {
                let index = self.core.dialect.coerce_tab_set(index);
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
            let key = self.core.lookup_event(Event::NewIndex);
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

                            let index = self.core.dialect.coerce_tab_set(index);
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
        mut callable: WeakValue<Ty>,
        start: StackSlot,
    ) -> Result<Callable<Strong, Ty>, RefAccessOrError<NotCallableError>> {
        loop {
            callable = match callable {
                Value::Function(callable) => {
                    let r = callable.upgrade(&self.core.gc).ok_or(AlreadyDroppedError)?;
                    return Ok(r);
                }
                t => t,
            };

            let new_callable = self
                .core
                .metatable_of(&callable)?
                .map(|mt| {
                    let key = self.core.lookup_event(Event::Call);
                    self.core
                        .gc
                        .get(mt)
                        .ok_or(AlreadyDroppedError)
                        .map(|table| table.get(&key))
                })
                .transpose()?
                .unwrap_or_default();

            // Keys associated with nil are not considered part of the table.
            if new_callable == Value::Nil {
                return Err(RefAccessOrError::Other(NotCallableError));
            }

            self.stack.lua_frame().insert(start, callable);
            callable = new_callable;
        }
    }

    pub fn next_opcode(&mut self) -> Option<OpCode> {
        let r = *self.opcodes.get(*self.ip)?;

        tracing::trace!(ip = self.ip.0, opcode = %r, "next opcode");

        *self.ip += 1;

        Some(r)
    }

    pub(crate) fn construct_closure(
        &mut self,
        recipe_id: RecipeId,
    ) -> Result<RootCell<Closure<Ty>>, opcode_err::Cause> {
        use crate::runtime::FunctionPtr;

        let recipe = self
            .chunk
            .get_recipe(recipe_id)
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

        let closure = match self.core.gc.try_alloc_cell(closure) {
            Ok(ptr) => ptr,
            Err(closure) => {
                // All Gc references inside the closure are copies of upvalues of current frame
                // which are going to be rooted in the process.
                self.sync();
                self.core.gc.alloc_cell(closure)
            }
        };

        self.stack
            .lua_frame()
            .register_closure(&closure, &self.core.gc);

        Ok(closure)
    }
}

impl<'rt, Ty> Debug for ActiveFrame<'rt, Ty>
where
    Ty: Debug + Types,
    WeakValue<Ty>: Debug,
    StrongValue<Ty>: Debug,
    Ty::Table: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ActiveFrame")
            .field("core", &self.core)
            .field("closure", &self.closure)
            .field("chunk", &self.chunk)
            .field("constants", &self.constants)
            .field("opcodes", &self.opcodes)
            .field("ip", &self.ip)
            .field("stack", &self.stack)
            .field("register_variadic", &self.register_variadic)
            .finish()
    }
}

struct NotCallableError;

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
