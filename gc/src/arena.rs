use std::any::Any;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

use bitvec::slice::BitSlice;
use bitvec::vec::BitVec;

use super::{Collector, GcCell, Location, RootCell, Trace};
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
    values: VecList<Place<T>>,
    strong_counters: Rc<RefCell<StrongCounters>>,
    gen: usize,
}

impl<T> ArenaStore<T> {
    pub(crate) fn new() -> Self {
        Self {
            values: VecList::with_capacity(10),
            strong_counters: Default::default(),
            gen: Default::default(),
        }
    }

    pub(crate) fn get(&self, index: Location<T>) -> Option<&T> {
        let Location {
            index,
            gen,
            _marker: _,
        } = index;

        self.values
            .get(index)
            .and_then(|place| (place.gen == gen).then_some(&place.value))
    }

    pub(crate) fn get_mut(&mut self, index: Location<T>) -> Option<&mut T> {
        let Location {
            index,
            gen,
            _marker: _,
        } = index;

        self.values
            .get_mut(index)
            .and_then(|place| (place.gen == gen).then_some(&mut place.value))
    }

    fn get_index(&self, index: usize) -> Option<&T> {
        self.values.get(index).map(|place| &place.value)
    }

    pub(crate) fn upgrade(&self, addr: Location<T>) -> Option<Counter> {
        let Place {
            counter: counter_place,
            ..
        } = self.values.get(addr.index)?;

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

    fn insert_weak(&mut self, value: T) -> Result<GcCell<T>, T> {
        self.values
            .try_insert(Place::new(value, self.gen))
            .map(|index| {
                use std::marker::PhantomData;

                GcCell {
                    addr: Location {
                        index,
                        gen: self.gen,
                        _marker: PhantomData,
                    },
                }
            })
            .map_err(|place| place.value)
    }

    pub(crate) fn try_insert(&mut self, value: T) -> Result<RootCell<T>, T> {
        self.insert_weak(value).map(|ptr| {
            let GcCell { addr } = ptr;

            let counter = self.upgrade(addr).unwrap();

            RootCell { addr, counter }
        })
    }

    pub(crate) fn insert(&mut self, value: T) -> RootCell<T> {
        self.values.grow();

        let Ok(ptr) = self.try_insert(value) else {
            unreachable!()
        };

        ptr
    }

    pub(crate) fn validate_counter(&self, counter: &Counter) -> bool {
        Rc::ptr_eq(&counter.counters, &self.strong_counters)
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
        let mut counters = self.strong_counters.borrow_mut();

        self.values
            .place_iter()
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
            if let Some(value) = self.get_index(index) {
                value.trace(collector)
            }
        }
    }

    fn retain(&mut self, indices: &BitSlice) {
        for index in indices.iter_zeros() {
            let place = self.values.remove(index);

            if let Some(Place { gen, .. }) = place {
                self.gen = self.gen.max(gen + 1);
            }
        }
    }
}

#[derive(Debug)]
struct Place<T> {
    value: T,
    counter: Cell<Option<CounterIndex>>,
    gen: usize,
}

impl<T> Place<T> {
    fn new(value: T, gen: usize) -> Self {
        Place {
            value,
            counter: Default::default(),
            gen,
        }
    }
}

#[derive(Debug, Default)]
struct StrongCounters(VecList<usize>);

impl StrongCounters {
    fn insert(&mut self, value: usize) -> CounterIndex {
        let index = self
            .0
            .try_insert(value)
            .unwrap_or_else(|_| self.0.insert(value));

        CounterIndex(index)
    }

    fn remove(&mut self, index: CounterIndex) {
        self.0.remove(index.0);
    }

    fn get(&self, index: CounterIndex) -> Option<usize> {
        self.0.get(index.0).copied()
    }

    fn increment(&mut self, index: CounterIndex) {
        if let Some(counter) = self.0.get_mut(index.0) {
            *counter += 1;
        }
    }

    fn decrement(&mut self, index: CounterIndex) {
        if let Some(counter) = self.0.get_mut(index.0) {
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
