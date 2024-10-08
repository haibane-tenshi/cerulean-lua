use std::fmt::Debug;

use gc::{Collector, GcCell, RootCell, Trace};
use repr::index::{FunctionId, UpvalueSlot};
use repr::tivec::{TiSlice, TiVec};

use super::thread::stack::RawStackSlot;
use crate::chunk_cache::ChunkId;
use crate::error::RtError;
use crate::runtime::RuntimeView;
use crate::value::{CoreTypes, Value, WeakValue};

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
    upvalues: TiVec<UpvalueSlot, UpvaluePlace<GcCell<WeakValue<Ty>>>>,
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
    ) -> Result<RootCell<Self>, RtError<Ty>> {
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
                .map(|value| heap.alloc_as(value).downgrade())
                .map(UpvaluePlace::Place)
                .collect();

            let closure = Closure { fn_ptr, upvalues };
            heap.alloc_cell(closure)
        });

        Ok(closure)
    }
}

impl<Ty> Closure<Ty>
where
    Ty: CoreTypes,
{
    pub(crate) fn from_raw_parts(
        fn_ptr: FunctionPtr,
        upvalues: TiVec<UpvalueSlot, UpvaluePlace<GcCell<WeakValue<Ty>>>>,
    ) -> Self {
        Closure { fn_ptr, upvalues }
    }

    pub(crate) fn fn_ptr(&self) -> FunctionPtr {
        self.fn_ptr
    }

    pub(crate) fn upvalues(&self) -> &TiSlice<UpvalueSlot, UpvaluePlace<GcCell<WeakValue<Ty>>>> {
        &self.upvalues
    }

    pub(crate) fn upvalues_mut(
        &mut self,
    ) -> &mut TiSlice<UpvalueSlot, UpvaluePlace<GcCell<WeakValue<Ty>>>> {
        &mut self.upvalues
    }
}
