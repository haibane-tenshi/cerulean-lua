use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use bitvec::slice::BitSlice;
use bitvec::vec::BitVec;

use super::{Collector, Gc, Location, Root, Trace};

pub(crate) trait Arena {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn roots(&self) -> BitVec;
    fn trace(&self, indices: &BitSlice, collector: &mut Collector);
    fn retain(&mut self, indices: &BitSlice);
}

#[derive(Debug)]
pub(crate) struct ArenaStore<T> {
    values: Vec<Place<T>>,
    strong_counters: Rc<RefCell<Vec<usize>>>,
    next_open: Option<usize>,
}

impl<T> ArenaStore<T> {
    pub(crate) fn new() -> Self {
        let mut r = Self {
            values: Default::default(),
            next_open: Default::default(),
            strong_counters: Default::default(),
        };

        r.reserve(10);

        r
    }

    pub(crate) fn get(&self, index: Location) -> Option<&T> {
        let Location { index } = index;

        self.get_index(index)
    }

    pub(crate) fn get_mut(&mut self, index: Location) -> Option<&mut T> {
        let Location { index } = index;

        self.values.get_mut(index).and_then(Place::as_mut)
    }

    fn get_index(&self, index: usize) -> Option<&T> {
        self.values.get(index).and_then(Place::as_ref)
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
        use std::marker::PhantomData;

        match self.next_open {
            Some(index) => {
                let place = self.values.get_mut(index).unwrap();

                match place {
                    Place::NextOpen(next) => {
                        self.next_open = *next;
                    }
                    _ => unreachable!(),
                }

                *place = Place::Occupied(value);
                self.strong_counters.borrow_mut()[index] = 1;

                let addr = Location { index };

                let r = Root {
                    addr,
                    strong_counters: self.strong_counters.clone(),
                    _marker: PhantomData,
                };

                Ok(r)
            }
            None => Err(value),
        }
    }

    pub(crate) fn insert(&mut self, value: T) -> Root<T> {
        // Reserve extra.
        self.reserve(10);

        let Ok(ptr) = self.try_insert(value) else {
            unreachable!()
        };

        ptr
    }

    fn reserve(&mut self, len: usize) {
        assert!(len > 0);

        self.values.reserve(len);

        let start = self.values.len();
        // Fill the entire vec capacity.
        // There is no reson not to do it.
        let len = self.values.capacity() - start;

        if let Some(last_open) = self.values.iter_mut().rev().find_map(|place| match place {
            Place::NextOpen(index) => Some(index),
            Place::Occupied(_) => None,
        }) {
            *last_open = Some(start);
        }
        self.values
            .extend((start + 1..start + len).map(|index| Place::NextOpen(Some(index))));
        self.values.push(Place::NextOpen(None));

        self.strong_counters
            .borrow_mut()
            .extend(std::iter::repeat(0).take(len));

        debug_assert_eq!(self.values.len(), self.strong_counters.borrow().len());
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
        let iter = indices.iter_zeros().zip(
            indices
                .iter_zeros()
                .skip(1)
                .map(Some)
                .chain(std::iter::repeat(None)),
        );
        for (index, next_open) in iter {
            if let Some(place) = self.values.get_mut(index) {
                *place = Place::NextOpen(next_open)
            }
        }

        self.next_open = indices.first_zero();
    }
}

#[derive(Debug)]
enum Place<T> {
    Occupied(T),
    NextOpen(Option<usize>),
}

impl<T> Place<T> {
    fn as_ref(&self) -> Option<&T> {
        match self {
            Place::Occupied(t) => Some(t),
            Place::NextOpen(_) => None,
        }
    }

    fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            Place::Occupied(t) => Some(t),
            Place::NextOpen(_) => None,
        }
    }
}
