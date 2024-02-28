use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::ControlFlow;

use gc::{Collector, Gc, Heap, Root, Trace};
use repr::chunk::{Chunk, ClosureRecipe};
use repr::debug_info::OpCodeDebugInfo;
use repr::index::{ConstId, FunctionId, InstrId, InstrOffset, RecipeId, StackSlot, UpvalueSlot};
use repr::literal::Literal;
use repr::opcode::{AriBinOp, BinOp, BitBinOp, EqBinOp, OpCode, RelBinOp, StrBinOp, UnaOp};
use repr::tivec::{TiSlice, TiVec};

use super::stack::{RawStackSlot, StackGuard};
use super::{Core, RuntimeView};
use crate::backtrace::BacktraceFrame;
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::opcode::{
    self as opcode_err, IpOutOfBounds, MissingConstId, MissingStackSlot, MissingUpvalue,
};
use crate::error::{AlreadyDroppedError, BorrowError, RefAccessError, RuntimeError};
use crate::gc::{FromWithGc, IntoWithGc, TryIntoWithGc};
use crate::value::callable::Callable;
use crate::value::table::KeyValue;
use crate::value::{
    Concat, CoreTypes, Len, Strong, StrongValue, TableIndex, Value, Weak, WeakValue,
};

pub(crate) enum ChangeFrame<Ty>
where
    Ty: CoreTypes,
{
    Return(StackSlot),
    Invoke(Option<Event>, Callable<Strong<Ty>>, RawStackSlot),
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct FunctionPtr {
    pub chunk_id: ChunkId,
    pub function_id: FunctionId,
}

impl Trace for FunctionPtr {
    fn trace(&self, _collector: &mut Collector) {}
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum UpvaluePlace<Value> {
    Stack(RawStackSlot),
    Place(Value),
}

#[derive(Debug, Clone)]
pub struct Closure<Ty>
where
    Ty: CoreTypes,
{
    fn_ptr: FunctionPtr,
    upvalues: TiVec<UpvalueSlot, UpvaluePlace<Gc<WeakValue<Ty>>>>,
}

impl<Ty> Trace for Closure<Ty>
where
    Ty: CoreTypes,
{
    fn trace(&self, collector: &mut Collector) {
        for upvalue in &self.upvalues {
            match upvalue {
                UpvaluePlace::Place(value) => value.trace(collector),
                UpvaluePlace::Stack(_) => (),
            }
        }
    }
}

impl<Ty> Closure<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn new(
        rt: &mut RuntimeView<Ty>,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = WeakValue<Ty>>,
    ) -> Result<Root<Self>, RuntimeError<Ty>> {
        use crate::error::{MissingChunk, MissingFunction};

        let upvalue_count = rt
            .chunk_cache
            .chunk(fn_ptr.chunk_id)
            .ok_or(MissingChunk(fn_ptr.chunk_id))?
            .get_function(fn_ptr.function_id)
            .ok_or(MissingFunction(fn_ptr))?
            .signature
            .upvalue_count;

        let closure = rt.core.gc.pause(|heap| {
            let upvalues = upvalues
                .into_iter()
                .chain(std::iter::repeat_with(|| Value::Nil))
                .take(upvalue_count)
                .map(|value| heap.alloc(value).downgrade())
                .map(UpvaluePlace::Place)
                .collect();

            let closure = Closure { fn_ptr, upvalues };
            heap.alloc(closure)
        });

        Ok(closure)
    }

    pub(crate) fn fn_ptr(&self) -> FunctionPtr {
        self.fn_ptr
    }

    pub(crate) fn upvalues(&self) -> &TiSlice<UpvalueSlot, UpvaluePlace<Gc<WeakValue<Ty>>>> {
        &self.upvalues
    }

    pub(crate) fn upvalues_mut(
        &mut self,
    ) -> &mut TiSlice<UpvalueSlot, UpvaluePlace<Gc<WeakValue<Ty>>>> {
        &mut self.upvalues
    }
}

pub(crate) struct Frame<Ty>
where
    Ty: CoreTypes,
{
    closure: Root<Closure<Ty>>,
    ip: InstrId,
    stack_start: RawStackSlot,
    register_variadic: Vec<StrongValue<Ty>>,
    /// Whether frame was created as result of evaluating metamethod.
    ///
    /// Metamethods in general mimic builtin behavior of opcodes,
    /// therefore need cleanup to ensure correct stack state after frame is exited.
    event: Option<Event>,
}

impl<Ty> Frame<Ty>
where
    Ty: CoreTypes,
    StrongValue<Ty>: Clone,
{
    pub(crate) fn new(
        mut rt: RuntimeView<Ty>,
        closure: Root<Closure<Ty>>,
        event: Option<Event>,
    ) -> Result<Self, RuntimeError<Ty>> {
        use crate::error::{MissingChunk, MissingFunction, UpvalueCountMismatch};
        use repr::chunk::Function;

        let cls = &rt.core.gc[&closure];

        let function = rt
            .chunk_cache
            .chunk(cls.fn_ptr.chunk_id)
            .ok_or(MissingChunk(cls.fn_ptr.chunk_id))?
            .get_function(cls.fn_ptr.function_id)
            .ok_or(MissingFunction(cls.fn_ptr))?;

        let Function { signature, .. } = function;

        // Verify that closure provides exact same number of upvalues
        // that is expected by the function.
        if signature.upvalue_count != cls.upvalues.len() {
            let err = UpvalueCountMismatch {
                expected: signature.upvalue_count,
                closure: cls.upvalues.len(),
            };

            return Err(err.into());
        }

        // Adjust stack, move varargs into register if needed.
        let mut stack = rt.stack.lua_frame();
        let call_height = StackSlot(0) + signature.arg_count;
        let stack_start = stack.boundary();

        let register_variadic = if signature.is_variadic {
            stack
                .adjust_height(call_height)
                .map(|value| value.try_into_with_gc(&mut rt.core.gc))
                .collect::<Result<_, _>>()?
        } else {
            let _ = stack.adjust_height(call_height);
            Default::default()
        };

        // Ensure that disassociated upvalues are properly reassociated.
        stack.sync_upvalues(&mut rt.core.gc);

        let r = Frame {
            closure,
            ip: Default::default(),
            stack_start,
            register_variadic,
            event,
        };

        Ok(r)
    }
}

impl<Ty> Frame<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn closure(&self) -> &Root<Closure<Ty>> {
        &self.closure
    }
}

impl<Ty> Frame<Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Display,
{
    pub(crate) fn activate<'a>(
        self,
        rt: &'a mut RuntimeView<Ty>,
    ) -> Result<ActiveFrame<'a, Ty>, RuntimeError<Ty>> {
        use crate::error::{MissingChunk, MissingFunction};

        let RuntimeView {
            core,
            chunk_cache,
            stack,
            ..
        } = rt;

        let Frame {
            closure,
            ip,
            stack_start,
            register_variadic,
            event,
        } = self;

        let fn_ptr = core.gc[&closure].fn_ptr;

        let chunk = chunk_cache
            .chunk(fn_ptr.chunk_id)
            .ok_or(MissingChunk(fn_ptr.chunk_id))?;
        let function = chunk
            .get_function(fn_ptr.function_id)
            .ok_or(MissingFunction(fn_ptr))?;

        let constants = &chunk.constants;
        let opcodes = &function.opcodes;
        let stack = stack.guard(stack_start).unwrap();

        tracing::trace!(stack = stack.to_pretty_string(), "activated Lua frame");

        let r = ActiveFrame {
            core,
            closure,
            chunk,
            constants,
            opcodes,
            ip,
            stack,
            register_variadic,
            event,
        };

        Ok(r)
    }
}

impl<Ty> Frame<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn backtrace(&self, heap: &Heap, chunk_cache: &dyn ChunkCache) -> BacktraceFrame {
        use crate::backtrace::{FrameSource, Location};

        let ptr = heap[&self.closure].fn_ptr;
        // Instruction pointer always points at the *next* instruction.
        let ip = self.ip - 1;
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
    Ty: CoreTypes,
    StrongValue<Ty>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Frame")
            .field("closure", &self.closure)
            .field("ip", &self.ip)
            .field("stack_start", &self.stack_start)
            .field("register_variadic", &self.register_variadic)
            .field("event", &self.event)
            .finish()
    }
}

pub struct ActiveFrame<'rt, Ty>
where
    Ty: CoreTypes,
{
    closure: Root<Closure<Ty>>,
    core: &'rt mut Core<Ty>,
    chunk: &'rt Chunk,
    constants: &'rt TiSlice<ConstId, Literal>,
    opcodes: &'rt TiSlice<InstrId, OpCode>,
    ip: InstrId,
    stack: StackGuard<'rt, Ty>,
    register_variadic: Vec<StrongValue<Ty>>,
    /// Whether frame was created as result of evaluating metamethod.
    ///
    /// Metamethods in general mimic builtin behavior of opcodes,
    /// therefore need cleanup to ensure correct stack state after frame is exited.
    event: Option<Event>,
}

impl<'rt, Ty> ActiveFrame<'rt, Ty>
where
    Ty: CoreTypes,
{
    fn get_constant(&self, index: ConstId) -> Result<&Literal, MissingConstId> {
        self.constants.get(index).ok_or(MissingConstId(index))
    }

    fn get_stack(&self, index: StackSlot) -> Result<&WeakValue<Ty>, MissingStackSlot> {
        self.stack.get_slot(index).ok_or(MissingStackSlot(index))
    }

    fn upvalue(
        &self,
        index: UpvalueSlot,
    ) -> Result<UpvaluePlace<Gc<WeakValue<Ty>>>, MissingUpvalue> {
        let closure = &self.core.gc[&self.closure];

        closure
            .upvalues()
            .get(index)
            .ok_or(MissingUpvalue(index))
            .cloned()
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
}

impl<'rt, Ty> ActiveFrame<'rt, Ty>
where
    Ty: CoreTypes,
    WeakValue<Ty>: Display,
{
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
                opcode,
                debug_info: self.opcode_debug_info(self.ip - 1),
                cause,
            })
        })
    }

    fn exec(
        &mut self,
        opcode: OpCode,
    ) -> Result<ControlFlow<ChangeFrame<Ty>>, RefAccessOrError<opcode_err::Cause>> {
        use super::stack::Source;
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

                let start = self.stack.boundary() + slot;

                ControlFlow::Break(ChangeFrame::Invoke(None, callable, start))
            }
            Return(slot) => ControlFlow::Break(ChangeFrame::Return(slot)),
            MakeClosure(fn_id) => {
                let closure = self.construct_closure(fn_id)?;

                self.stack.lua_frame().push(
                    Value::Function(Callable::Lua(closure.downgrade().into())),
                    Source::TrustedIsRooted(false),
                );

                ControlFlow::Continue(())
            }
            LoadConstant(index) => {
                let constant = self.get_constant(index)?.clone();
                let value: StrongValue<_> = constant.into_with_gc(&mut self.core.gc);
                self.stack
                    .lua_frame()
                    .push(value.downgrade(), Source::TrustedIsRooted(false));

                ControlFlow::Continue(())
            }
            LoadVariadic => {
                self.stack.lua_frame().extend(
                    self.register_variadic.iter().map(|value| value.downgrade()),
                    true,
                );

                ControlFlow::Continue(())
            }
            LoadStack(slot) => {
                let value = self.get_stack(slot)?.clone();
                self.stack.lua_frame().push(value, Source::StackSlot(slot));

                ControlFlow::Continue(())
            }
            StoreStack(slot) => {
                let mut stack = self.stack.lua_frame();
                let [value] = stack.take1()?;
                stack.set(slot, value, Source::StackSlot(slot));

                ControlFlow::Continue(())
            }
            AdjustStack(height) => {
                let _ = self.stack.lua_frame().adjust_height(height);

                ControlFlow::Continue(())
            }
            LoadUpvalue(slot) => {
                let upvalue = self.upvalue(slot)?;

                let mut stack = self.stack.lua_frame();

                let (value, source) = match upvalue {
                    UpvaluePlace::Place(place) => {
                        let value = self.core.gc.get(place).ok_or(AlreadyDroppedError)?.clone();

                        (value, Source::TrustedIsRooted(true))
                    }
                    UpvaluePlace::Stack(stack_slot) => {
                        let value = stack
                            .get_raw_slot(stack_slot)
                            .ok_or(MissingUpvalue(slot))?
                            .clone();

                        (value, Source::StackSlot(stack_slot))
                    }
                };

                stack.push_raw(value, source);

                ControlFlow::Continue(())
            }
            StoreUpvalue(slot) => {
                let upvalue = self.upvalue(slot)?;
                let mut stack = self.stack.lua_frame();
                let [value] = stack.take1()?;

                match upvalue {
                    UpvaluePlace::Place(place) => {
                        let place = self.core.gc.get_mut(place).ok_or(AlreadyDroppedError)?;

                        *place = value;
                    }
                    UpvaluePlace::Stack(slot) => {
                        stack.set_raw(slot, value, Source::StackSlot(stack.next_slot()));
                    }
                }

                ControlFlow::Continue(())
            }
            UnaOp(op) => {
                let args = self.stack.lua_frame().take1()?;
                self.exec_una_op(args, op)
                    .map_err(|err| err.map_other(Into::into))?
                    .map_br(|(event, callable, start)| {
                        let start = self.stack.boundary() + start;
                        ChangeFrame::Invoke(Some(event), callable, start)
                    })
            }
            BinOp(op) => {
                let args = self.stack.lua_frame().take2()?;
                self.exec_bin_op(args, op)
                    .map_err(|err| err.map_other(Into::into))?
                    .map_br(|(event, callable, start)| {
                        let start = self.stack.boundary() + start;
                        ChangeFrame::Invoke(Some(event), callable, start)
                    })
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
                self.stack.lua_frame().sync_transient(&mut self.core.gc);
                let value = self.core.gc.alloc(Default::default()).downgrade();

                self.stack
                    .lua_frame()
                    .push(Value::Table(value.into()), Source::TrustedIsRooted(false));

                ControlFlow::Continue(())
            }
            TabGet => {
                let args = self.stack.lua_frame().take2()?;
                self.exec_tab_get(args)
                    .map_err(|err| err.map_other(Cause::TabGet))?
                    .map_br(|(callable, start)| {
                        let start = self.stack.boundary() + start;
                        ChangeFrame::Invoke(Some(Event::Index), callable, start)
                    })
            }
            TabSet => {
                let args = self.stack.lua_frame().take3()?;
                self.exec_tab_set(args)
                    .map_err(|err| err.map_other(Cause::TabSet))?
                    .map_br(|(callable, start)| {
                        let start = self.stack.boundary() + start;
                        ChangeFrame::Invoke(Some(Event::NewIndex), callable, start)
                    })
            }
        };

        tracing::trace!(stack = self.stack.to_pretty_string(), "executed opcode");

        Ok(r)
    }

    fn exec_una_op(
        &mut self,
        args: [WeakValue<Ty>; 1],
        op: UnaOp,
    ) -> Result<
        ControlFlow<(Event, Callable<Strong<Ty>>, StackSlot)>,
        RefAccessOrError<opcode_err::UnaOpCause>,
    > {
        use super::stack::Source;
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
                use super::CoerceArgs;

                let args = self.core.dialect.coerce_una_op_bit(args);

                match args {
                    [Value::Int(val)] => Continue((!Int(val)).into()),
                    args => Break((Event::BitNot, args)),
                }
            }
            UnaOp::StrLen => {
                match args {
                    [Value::String(val)] => {
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
                self.stack
                    .lua_frame()
                    .push(r, Source::TrustedIsRooted(false));

                return Ok(Continue(()));
            }
        };

        match eval {
            Continue(value) => {
                // Value contains no reference in this case.
                self.stack
                    .lua_frame()
                    .push(value, Source::TrustedIsRooted(false));
                Ok(Continue(()))
            }
            Break((event, args)) => {
                let [arg] = &args;

                let metatable = self.core.metatable_of(arg)?;
                let heap = &mut self.core.gc;

                let key = event.into_with_gc(heap);
                let metavalue = metatable
                    .map(|mt| {
                        heap.get(mt.into())
                            .ok_or(AlreadyDroppedError)
                            .map(|table| table.get(&key))
                    })
                    .transpose()?
                    .unwrap_or_default();

                match metavalue {
                    Value::Nil => match (op, args) {
                        // Trigger table len builtin on failed metamethod lookup.
                        (UnaOp::StrLen, [Value::Table(tab)]) => {
                            let border = heap.get(tab.into()).ok_or(AlreadyDroppedError)?.border();
                            self.stack
                                .lua_frame()
                                .push(Value::Int(border), Source::TrustedIsRooted(false));

                            Ok(Continue(()))
                        }
                        _ => Err(err.into()),
                    },
                    metavalue => {
                        let start = self.stack.next_slot();

                        let [arg] = args;
                        let mut stack = self.stack.lua_frame();
                        stack.push(arg, Source::StackSlot(stack.next_slot()));
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

    fn exec_bin_op(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: BinOp,
    ) -> Result<
        std::ops::ControlFlow<(Event, Callable<Strong<Ty>>, StackSlot)>,
        RefAccessOrError<opcode_err::BinOpCause>,
    > {
        use super::stack::Source;

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
                self.stack
                    .lua_frame()
                    .push(value, Source::TrustedIsRooted(false));
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

                let key = event.into_with_gc(&mut self.core.gc);
                let lookup_event = |value: &WeakValue<Ty>| -> Result<_, AlreadyDroppedError> {
                    let r = self
                        .core
                        .metatable_of(value)?
                        .map(|mt| {
                            self.core
                                .gc
                                .get(mt.into())
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
                            self.stack
                                .lua_frame()
                                .push(r, Source::TrustedIsRooted(false));
                            Ok(ControlFlow::Continue(()))
                        } else {
                            Err(err.into())
                        }
                    }
                    metavalue => {
                        let start = self.stack.next_slot();
                        let mut stack = self.stack.lua_frame();
                        stack.push(lhs, Source::StackSlot(stack.next_slot()));
                        stack.push(rhs, Source::StackSlot(stack.next_slot() + 1));

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

    fn exec_bin_op_str(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: StrBinOp,
    ) -> Result<ControlFlow<[WeakValue<Ty>; 2], Option<WeakValue<Ty>>>, RefAccessError> {
        use super::CoerceArgs;

        let args = self
            .core
            .dialect
            .coerce_bin_op_str(op, args, &mut self.core.gc);

        match args {
            [Value::String(lhs), Value::String(rhs)] => match op {
                StrBinOp::Concat => {
                    use crate::gc::StringRef;
                    use std::ops::Deref;

                    let mut r = lhs.deref().clone();
                    r.concat(rhs.deref());
                    let s = StringRef::new(r);

                    Ok(ControlFlow::Continue(Some(Value::String(s))))
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
        use super::CoerceArgs;
        use crate::value::{Float, Int};
        use EqBinOp::*;

        let cmp = <_ as CoerceArgs<Weak<Ty>>>::cmp_float_and_int(&self.core.dialect);

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

    fn exec_bin_op_rel(
        &mut self,
        args: [WeakValue<Ty>; 2],
        op: RelBinOp,
    ) -> Result<ControlFlow<[WeakValue<Ty>; 2], Option<WeakValue<Ty>>>, RefAccessError> {
        use super::CoerceArgs;
        use crate::value;
        use RelBinOp::*;
        use Value::*;

        let cmp = <_ as CoerceArgs<Weak<Ty>>>::cmp_float_and_int(&self.core.dialect);

        let ord = match &args {
            [Int(lhs), Int(rhs)] => PartialOrd::partial_cmp(lhs, rhs),
            [Float(lhs), Float(rhs)] => PartialOrd::partial_cmp(lhs, rhs),
            [String(lhs), String(rhs)] => PartialOrd::partial_cmp(lhs, rhs),
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
        use super::CoerceArgs;
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
        use super::CoerceArgs;
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

    fn exec_tab_get(
        &mut self,
        args: [WeakValue<Ty>; 2],
    ) -> Result<
        ControlFlow<(Callable<Strong<Ty>>, StackSlot)>,
        RefAccessOrError<opcode_err::TabCause>,
    > {
        use super::stack::Source;
        use super::CoerceArgs;
        use opcode_err::TabCause::*;
        use ControlFlow::*;

        let [mut table, index] = args;

        'outer: loop {
            // First: try raw table access.
            if let Value::Table(table) = &table {
                let index = self.core.dialect.coerce_tab_get(index.clone());

                let key = index.try_into().map_err(InvalidKey)?;
                let table = *table;
                let value = self
                    .core
                    .gc
                    .get(table.into())
                    .ok_or(AlreadyDroppedError)?
                    .get(&key);

                // It succeeds if any non-nil value is produced.
                if value != Value::Nil {
                    // We know that the value originated from the table in this slot.
                    let mut stack = self.stack.lua_frame();
                    stack.push(value, Source::StackSlot(stack.next_slot()));
                    return Ok(Continue(()));
                }
            };

            // Second: try detecting compatible metavalue
            let key = Event::Index.into_with_gc(&mut self.core.gc);

            loop {
                let metavalue = self
                    .core
                    .metatable_of(&table)?
                    .map(|mt| {
                        self.core
                            .gc
                            .get(mt.into())
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

                            self.stack
                                .lua_frame()
                                .push(Value::Nil, Source::TrustedIsRooted(false));
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
                        let callable = callable.try_into_with_gc(&mut self.core.gc)?;
                        let start = self.stack.next_slot();
                        let mut stack = self.stack.lua_frame();
                        stack.push(table, Source::StackSlot(stack.next_slot()));
                        stack.push(index, Source::StackSlot(stack.next_slot() + 1));

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

    fn exec_tab_set(
        &mut self,
        args: [WeakValue<Ty>; 3],
    ) -> Result<
        ControlFlow<(Callable<Strong<Ty>>, StackSlot)>,
        RefAccessOrError<opcode_err::TabCause>,
    > {
        use super::stack::Source;
        use super::CoerceArgs;
        use opcode_err::TabCause::*;
        use ControlFlow::*;

        let [mut table, index, value] = args;

        'outer: loop {
            // First: try raw table access.
            if let Value::Table(table) = &table {
                let index = self.core.dialect.coerce_tab_set(index.clone());
                let key = index.try_into().map_err(InvalidKey)?;
                let table = *table;
                let table = self
                    .core
                    .gc
                    .get_mut(table.into())
                    .ok_or(AlreadyDroppedError)?;

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
            let key = Event::NewIndex.into_with_gc(&mut self.core.gc);
            loop {
                let metavalue = self
                    .core
                    .metatable_of(&table)?
                    .map(|mt| {
                        self.core
                            .gc
                            .get(mt.into())
                            .ok_or(AlreadyDroppedError)
                            .map(|table| table.get(&key))
                    })
                    .transpose()?
                    .unwrap_or_default();

                match metavalue {
                    // Keyes associated with nil are considered to be absent from table.
                    Value::Nil => {
                        if let Value::Table(table) = table {
                            // Third: Fallback to raw assignment.
                            // This can only be reached if raw table access returned nil and there is no metamethod.

                            let index = self.core.dialect.coerce_tab_set(index);
                            let key = index.try_into().map_err(InvalidKey)?;
                            let value = value.into();

                            self.core
                                .gc
                                .get_mut(table.into())
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
                        let callable = callable.try_into_with_gc(&mut self.core.gc)?;
                        let start = self.stack.next_slot();
                        let mut stack = self.stack.lua_frame();
                        stack.push(table, Source::StackSlot(stack.next_slot()));
                        stack.push(index, Source::StackSlot(stack.next_slot() + 1));
                        stack.push(value, Source::StackSlot(stack.next_slot() + 2));

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
    ) -> Result<Callable<Strong<Ty>>, RefAccessOrError<NotCallableError>> {
        use super::stack::Source;

        loop {
            callable = match callable {
                Value::Function(r) => return Ok(r.try_into_with_gc(&mut self.core.gc)?),
                t => t,
            };

            let new_callable = self
                .core
                .metatable_of(&callable)?
                .map(|mt| {
                    let key = Event::Call.into_with_gc(&mut self.core.gc);
                    self.core
                        .gc
                        .get(mt.into())
                        .ok_or(AlreadyDroppedError)
                        .map(|table| table.get(&key))
                })
                .transpose()?
                .unwrap_or_default();

            // Keys associated with nil are not considered part of the table.
            if new_callable == Value::Nil {
                return Err(RefAccessOrError::Other(NotCallableError));
            }

            self.stack
                .lua_frame()
                .insert(start, callable, Source::StackSlot(start));
            callable = new_callable;
        }
    }

    fn opcode_debug_info(&self, ip: InstrId) -> Option<OpCodeDebugInfo> {
        let heap = &self.core.gc;

        self.chunk
            .debug_info
            .as_ref()
            .and_then(|debug_info| {
                let fn_id = heap[&self.closure].fn_ptr.function_id;
                debug_info.functions.get(fn_id)
            })
            .and_then(|fn_debug_info| fn_debug_info.opcodes.get(ip))
            .cloned()
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
        let recipe = self
            .chunk
            .get_recipe(recipe_id)
            .ok_or(opcode_err::MissingRecipe(recipe_id))?;

        let ClosureRecipe {
            function_id,
            upvalues,
        } = recipe;

        let chunk_id = self.core.gc[&self.closure].fn_ptr.chunk_id;

        let fn_ptr = FunctionPtr {
            chunk_id,
            function_id: *function_id,
        };

        let stack = self.stack.lua_frame();
        let closure = &self.core.gc[&self.closure];
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
                        .upvalues
                        .get(slot)
                        .copied()
                        .ok_or(opcode_err::MissingUpvalue(slot).into()),
                }
            })
            .collect::<Result<_, _>>()?;

        let closure = Closure { fn_ptr, upvalues };

        // Make sure to sync the stack before allocating.
        // Sync itself may allocate, but there is no need to pause.
        // All Gc references inside the closure are copies of upvalues of current frame
        // which are definitely rooted.
        self.stack.lua_frame().sync_transient(&mut self.core.gc);
        let closure = self.core.gc.alloc(closure);

        self.stack
            .lua_frame()
            .register_closure(&closure, &self.core.gc);

        Ok(closure)
    }

    pub(crate) fn suspend(self) -> Frame<Ty> {
        let ActiveFrame {
            closure,
            ip,
            stack,
            register_variadic,
            event,
            ..
        } = self;

        let stack_start = stack.boundary();

        Frame {
            closure,
            ip,
            stack_start,
            register_variadic,
            event,
        }
    }

    pub(crate) fn exit(mut self, returns: StackSlot) -> Result<(), RuntimeError<Ty>> {
        let mut stack = self.stack.lua_frame();

        // All upvalues need to be gone.
        stack.evict_upvalues(..);
        let _ = stack.drain(StackSlot(0)..returns);

        if let Some(event) = self.event {
            stack.adjust_event_returns(event);
        }

        // Sync on exit.
        // If some values were rooted by current frame
        // (e.g. originated from upvalues or variadics)
        // it is possible that its last root is going to get dropped right now.
        // Currently we don't track value origins with sufficient precision to avoid this scenario.
        stack.sync_transient(&mut self.core.gc);

        Ok(())
    }
}

impl<'rt, Ty> Debug for ActiveFrame<'rt, Ty>
where
    Ty: Debug + CoreTypes,
    WeakValue<Ty>: Debug,
    StrongValue<Ty>: Debug,
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
            .field("event", &self.event)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    pub fn to_str(self) -> &'static str {
        use Event::*;

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
            Neq => "__eq",
            Lt => "__lt",
            LtEq => "__le",
            Index => "__index",
            NewIndex => "__newindex",
            Call => "__call",
        }
    }
}

impl<Ty> FromWithGc<Event, Heap> for KeyValue<Weak<Ty>>
where
    Ty: CoreTypes,
    Ty::String: From<&'static str>,
{
    fn from_with_gc(value: Event, _gc: &mut Heap) -> Self {
        let s = crate::gc::StringRef::new(value.to_str().into());
        KeyValue::String(s)
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

impl<E, Ty> From<RefAccessOrError<E>> for RuntimeError<Ty>
where
    Ty: CoreTypes,
    E: Into<RuntimeError<Ty>>,
{
    fn from(value: RefAccessOrError<E>) -> Self {
        match value {
            RefAccessOrError::Dropped(err) => err.into(),
            RefAccessOrError::Borrowed(err) => err.into(),
            RefAccessOrError::Other(err) => err.into(),
        }
    }
}
