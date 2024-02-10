use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use bitvec::slice::BitSlice;
use bitvec::vec::BitVec;

use super::{Collector, Gc, Location, Root, Trace};
use crate::vec_list::VecList;

pub(crate) trait Arena {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn roots(&self) -> BitVec;
    fn trace(&self, indices: &BitSlice, collector: &mut Collector);
    fn retain(&mut self, indices: &BitSlice);
}

#[derive(Debug)]
pub(crate) struct ArenaStore<T> {
    values: VecList<T>,
    strong_counters: Rc<RefCell<Vec<usize>>>,
}

impl<T> ArenaStore<T> {
    pub(crate) fn new() -> Self {
        let mut r = Self {
            values: VecList::with_capacity(10),
            strong_counters: Default::default(),
        };

        r
    }

    pub(crate) fn get(&self, index: Location) -> Option<&T> {
        let Location { index } = index;

        self.get_index(index)
    }

    pub(crate) fn get_mut(&mut self, index: Location) -> Option<&mut T> {
        let Location { index } = index;

        self.values.get_mut(index)
    }

    fn get_index(&self, index: usize) -> Option<&T> {
        self.values.get(index)
    }

    pub(crate) fn upgrade(&self, ptr: Gc<T>) -> Option<Root<T>> {
        use std::marker::PhantomData;

        let Gc { addr, _marker } = ptr;

        let _ = self.get(addr)?;

        self.strong_counters.borrow_mut()[addr.index] += 1;

        let r = Root {
            addr,
            strong_counters: self.strong_counters.clone(),
            _marker: PhantomData,
        };

        Some(r)
    }

    pub(crate) fn try_insert(&mut self, value: T) -> Result<Root<T>, T> {
        self.values
            .try_insert(value)
            .map(|index| self.make_root(index))
    }

    pub(crate) fn insert(&mut self, value: T) -> Root<T> {
        let index = self.values.insert(value);

        let mut counters = self.strong_counters.borrow_mut();
        let extra = self.values.len() - counters.len();
        counters.extend(std::iter::repeat(0).take(extra));

        self.make_root(index)
    }

    fn make_root(&self, index: usize) -> Root<T> {
        use std::marker::PhantomData;

        let addr = Location { index };

        self.strong_counters.borrow_mut()[index] += 1;

        Root {
            addr,
            strong_counters: self.strong_counters.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> Arena for ArenaStore<T>
where
    T: Trace,
    Self: Any,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn roots(&self) -> BitVec {
        self.strong_counters
            .borrow()
            .iter()
            .map(|counter| *counter != 0)
            .collect()
    }

    fn trace(&self, indices: &BitSlice, collector: &mut Collector) {
        for index in indices.iter_ones() {
            if let Some(value) = self.get_index(index) {
                value.trace(collector)
            }
        }
    }

    fn retain(&mut self, indices: &BitSlice) {
        for index in indices.iter_zeros() {
            self.values.remove(index);
        }
    }
}
