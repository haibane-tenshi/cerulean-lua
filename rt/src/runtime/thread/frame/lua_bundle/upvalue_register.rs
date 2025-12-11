use std::fmt::Debug;

use bitvec::prelude::BitVec;

use gc::{GcCell, RootCell};
use repr::index::UpvalueSlot;
use repr::tivec::TiVec;

use crate::runtime::closure::UpvaluePlace;
use crate::runtime::thread::stack::StackGuard;
use crate::runtime::Heap;
use crate::value::{Types, WeakValue};

pub(crate) struct UpvalueRegister<Ty>
where
    Ty: Types,
{
    values: TiVec<UpvalueSlot, WeakValue<Ty>>,
    write_tag: BitVec,
    unsync_tag: BitVec,
    mirror: RootCell<Vec<WeakValue<Ty>>>,
}

impl<Ty> UpvalueRegister<Ty>
where
    Ty: Types,
{
    pub(crate) fn new(heap: &mut Heap<Ty>) -> Self {
        UpvalueRegister {
            values: Default::default(),
            write_tag: Default::default(),
            unsync_tag: Default::default(),
            mirror: heap.alloc_cell(Default::default()),
        }
    }

    // pub(super) fn clear(&mut self) {
    //     self.values.clear();
    //     self.write_tag.clear();
    // }

    pub(super) fn fill(&mut self, values: impl IntoIterator<Item = WeakValue<Ty>>) {
        debug_assert_eq!(self.values.len(), self.write_tag.len());
        debug_assert_eq!(self.values.len(), self.unsync_tag.len());

        self.values.extend(values);
        self.write_tag.resize(self.values.len(), false);
        self.unsync_tag.resize(self.values.len(), false);
    }

    pub(crate) fn drain_written(
        &mut self,
    ) -> impl Iterator<Item = (UpvalueSlot, WeakValue<Ty>)> + use<'_, Ty> {
        self.unsync_tag.clear();

        self.values
            .drain_enumerated(..)
            .zip(self.write_tag.drain(..))
            .filter_map(|(pair, is_written)| is_written.then_some(pair))
    }

    pub(crate) fn load(&self, index: UpvalueSlot) -> Option<WeakValue<Ty>> {
        self.values.get(index).copied()
    }

    pub(crate) fn store(
        &mut self,
        index: UpvalueSlot,
        value: WeakValue<Ty>,
    ) -> Result<(), WeakValue<Ty>> {
        let Some(place) = self.values.get_mut(index) else {
            return Err(value);
        };

        let mut unsync_place = self
            .unsync_tag
            .get_mut(index.0)
            .expect("unsync tags should have the same length as values");
        let unsync_bit = (*place == value) && value.is_transient();

        let mut write_place = self
            .write_tag
            .get_mut(index.0)
            .expect("write tags should have the same length as values");
        let write_bit = *place == value;

        *place = value;
        *write_place |= write_bit;
        *unsync_place |= unsync_bit;

        Ok(())
    }

    pub(crate) fn sync(&mut self, heap: &mut Heap<Ty>) {
        if self.unsync_tag.not_any() {
            return;
        }

        let mirror = heap.get_root_mut(&self.mirror);
        mirror.clear();
        mirror.extend_from_slice(self.values.as_ref());

        self.unsync_tag.fill(false);
    }
}

impl<Ty> Debug for UpvalueRegister<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UpvalueRegister")
            .field("values", &self.values)
            .field("write_tag", &self.write_tag)
            .field("unsync_tag", &self.unsync_tag)
            .field("mirror", &self.mirror)
            .finish()
    }
}

pub(super) fn preload_upvalues<'a, 'b, 'c, Ty>(
    upvalues: &'a [UpvaluePlace<GcCell<WeakValue<Ty>>>],
    mut stack: StackGuard<'b, Ty>,
    heap: &'c Heap<Ty>,
) -> impl Iterator<Item = WeakValue<Ty>> + use<'a, 'b, 'c, Ty>
where
    Ty: Types,
{
    upvalues.iter().map(move |place| match *place {
        UpvaluePlace::Stack(slot) => *stack
            .lua_guard()
            .get_raw_slot(slot)
            .expect("upvalues allocated on thread stack should be protected"),
        UpvaluePlace::Place(ptr) => *heap
            .get(ptr)
            .expect("detached upavlues should still be allocated"),
    })
}
