use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

use bitvec::slice::BitSlice;
use bitvec::vec::BitVec;

use super::arena::{HandleStrongRef, Traceable};
use super::{Collector, Trace};
use crate::vec_list::{GenTag, VecList};

#[derive(Debug)]
pub(crate) struct Store<T> {
    values: VecList<Index, Gen, Place<T>>,
    strong_counters: Rc<RefCell<StrongCounters>>,
}

impl<T> Store<T> {
    pub(crate) fn new() -> Self {
        Self {
            values: VecList::with_capacity(10),
            strong_counters: Default::default(),
        }
    }

    fn insert_weak(&mut self, value: T) -> Result<Addr, T> {
        self.values
            .try_insert(Place::new(value))
            .map(|(index, gen)| Addr { index, gen })
            .map_err(|place| place.value)
    }

    pub(crate) fn get(&self, addr: Addr) -> Option<&T> {
        let Addr { index, gen } = addr;

        self.values.get(index, gen).map(|place| &place.value)
    }

    pub(crate) fn get_mut(&mut self, addr: Addr) -> Option<&mut T> {
        let Addr { index, gen } = addr;

        self.values
            .get_mut(index, gen)
            .map(|place| &mut place.value)
    }

    fn get_untagged(&self, index: Index) -> Option<&T> {
        self.values.get_untagged(index).map(|place| &place.value)
    }

    pub(crate) fn upgrade(&self, addr: Addr) -> Option<Counter> {
        let Addr { index, gen } = addr;

        let Place {
            counter: counter_place,
            ..
        } = self.values.get(index, gen)?;

        let index = match counter_place.get() {
            Some(index) => {
                self.strong_counters.borrow_mut().increment(index);
                index
            }
            None => {
                let index = self.strong_counters.borrow_mut().insert(1);
                counter_place.set(Some(index));
                index
            }
        };

        let counter = Counter {
            index,
            counters: self.strong_counters.clone(),
        };

        Some(counter)
    }

    pub(crate) fn try_insert(&mut self, value: T) -> Result<(Addr, Counter), T> {
        self.insert_weak(value).map(|addr| {
            let counter = self.upgrade(addr).unwrap();

            (addr, counter)
        })
    }

    pub(crate) fn insert(&mut self, value: T) -> (Addr, Counter) {
        self.values.grow();

        let Ok(ptr) = self.try_insert(value) else {
            unreachable!()
        };

        ptr
    }
}

impl<T> Traceable for Store<T>
where
    T: Trace,
{
    fn roots(&self) -> BitVec {
        let mut counters = self.strong_counters.borrow_mut();

        self.values
            .iter_occupied()
            .map(|value| match value {
                Some(Place {
                    counter: counter_place,
                    ..
                }) => match counter_place.get() {
                    Some(counter) => match counters.get(counter) {
                        None => false,
                        Some(0) => {
                            counters.remove(counter);
                            counter_place.set(None);

                            false
                        }
                        Some(_) => true,
                    },
                    None => false,
                },
                None => false,
            })
            .collect()
    }

    fn trace(&self, indices: &BitSlice, collector: &mut Collector) {
        for index in indices.iter_ones() {
            if let Some(value) = self.get_untagged(index.into()) {
                value.trace(collector)
            }
        }
    }

    fn retain(&mut self, indices: &BitSlice) {
        for index in indices.iter_zeros() {
            self.values.remove(index.into());
        }
    }
}

impl<T> HandleStrongRef for Store<T> {
    fn validate(&self, counter: &Counter) -> bool {
        Rc::ptr_eq(&self.strong_counters, &counter.counters)
    }

    fn upgrade(&self, addr: Addr) -> Option<Counter> {
        Store::upgrade(self, addr)
    }
}

#[derive(Debug)]
struct Place<T> {
    value: T,
    counter: Cell<Option<CounterIndex>>,
}

impl<T> Place<T> {
    fn new(value: T) -> Self {
        Place {
            value,
            counter: Default::default(),
        }
    }
}

#[derive(Debug, Default)]
struct StrongCounters(VecList<usize, (), usize>);

impl StrongCounters {
    fn insert(&mut self, value: usize) -> CounterIndex {
        let (index, ()) = self
            .0
            .try_insert(value)
            .unwrap_or_else(|_| self.0.insert(value));

        CounterIndex(index)
    }

    fn remove(&mut self, index: CounterIndex) {
        self.0.remove(index.0);
    }

    fn get(&self, index: CounterIndex) -> Option<usize> {
        self.0.get(index.0, ()).copied()
    }

    fn increment(&mut self, index: CounterIndex) {
        if let Some(counter) = self.0.get_mut(index.0, ()) {
            *counter += 1;
        }
    }

    fn decrement(&mut self, index: CounterIndex) {
        if let Some(counter) = self.0.get_mut(index.0, ()) {
            *counter -= 1;
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct CounterIndex(usize);

#[derive(Debug)]
pub(crate) struct Counter {
    index: CounterIndex,
    counters: Rc<RefCell<StrongCounters>>,
}

impl Clone for Counter {
    fn clone(&self) -> Self {
        let Counter { index, counters } = self;

        let index = *index;
        let counters = counters.clone();

        counters.borrow_mut().increment(index);

        Counter { index, counters }
    }
}

impl Drop for Counter {
    fn drop(&mut self) {
        self.counters.borrow_mut().decrement(self.index);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Addr {
    index: Index,
    gen: Gen,
}

impl Addr {
    pub(crate) fn index(self) -> usize {
        self.index.0
    }

    pub(crate) fn gen(self) -> usize {
        self.gen.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Index(usize);

impl From<Index> for usize {
    fn from(value: Index) -> Self {
        value.0
    }
}

impl From<usize> for Index {
    fn from(value: usize) -> Self {
        Index(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Gen(usize);

impl GenTag for Gen {
    fn new() -> Self {
        Gen(0)
    }

    fn next(self) -> Option<Self> {
        let index = self.0.checked_add(1)?;
        Some(Gen(index))
    }
}
