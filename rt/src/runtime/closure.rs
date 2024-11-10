use std::cell::{Ref, RefCell, RefMut};
use std::fmt::Debug;

use gc::{Collector, GcCell, Root, Trace};
use repr::index::{FunctionId, UpvalueSlot};
use repr::tivec::TiVec;

use super::thread::stack::RawStackSlot;
use crate::chunk_cache::ChunkId;
use crate::error::MalformedClosureError;
use crate::runtime::{RuntimeView, ThreadId};
use crate::value::{Types, Value, WeakValue};

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
    Ty: Types,
{
    fn_ptr: FunctionPtr,
    origin: ThreadId,
    upvalues: RefCell<TiVec<UpvalueSlot, UpvaluePlace<GcCell<WeakValue<Ty>>>>>,
}

impl<Ty> Trace for Closure<Ty>
where
    Ty: Types,
{
    fn trace(&self, collector: &mut Collector) {
        for upvalue in self.upvalues.borrow().iter() {
            match upvalue {
                UpvaluePlace::Place(value) => value.trace(collector),
                UpvaluePlace::Stack(_) => (),
            }
        }
    }
}

impl<Ty> Closure<Ty>
where
    Ty: Types,
{
    pub(crate) fn new(
        rt: &mut RuntimeView<Ty>,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = WeakValue<Ty>>,
    ) -> Result<Root<Self>, MalformedClosureError> {
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
            let upvalues = RefCell::new(upvalues);

            // Use dummy id.
            // Thread with this id is guaranteed to exist which is the only property that matters:
            // all provided upvalues start detached.
            let origin = ThreadId::dummy();

            let closure = Closure {
                fn_ptr,
                origin,
                upvalues,
            };
            heap.alloc(closure)
        });

        Ok(closure)
    }
}

impl<Ty> Closure<Ty>
where
    Ty: Types,
{
    pub(crate) fn from_raw_parts(
        fn_ptr: FunctionPtr,
        origin: ThreadId,
        upvalues: TiVec<UpvalueSlot, UpvaluePlace<GcCell<WeakValue<Ty>>>>,
    ) -> Self {
        Closure {
            fn_ptr,
            origin,
            upvalues: RefCell::new(upvalues),
        }
    }

    pub(crate) fn fn_ptr(&self) -> FunctionPtr {
        self.fn_ptr
    }

    pub(crate) fn origin_thread(&self) -> ThreadId {
        self.origin
    }

    pub(crate) fn upvalues(&self) -> Ref<TiVec<UpvalueSlot, UpvaluePlace<GcCell<WeakValue<Ty>>>>> {
        self.upvalues.borrow()
    }

    pub(crate) fn upvalues_mut(
        &self,
    ) -> RefMut<TiVec<UpvalueSlot, UpvaluePlace<GcCell<WeakValue<Ty>>>>> {
        self.upvalues.borrow_mut()
    }
}
