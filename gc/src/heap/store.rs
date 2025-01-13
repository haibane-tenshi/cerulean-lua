use std::cell::{Cell, RefCell};
use std::fmt::{Debug, Display, Pointer};
use std::hash::Hash;
use std::rc::Rc;

use bitvec::slice::BitSlice;
use bitvec::vec::BitVec;
use nonmax::NonMaxU32;

use super::arena::{AsAny, Getters, HandleStrongRef, Insert, Traceable};
use super::{Collector, Trace};
use crate::userdata::Params;
use crate::vec_list::{GenTag, VecList};

#[derive(Debug)]
pub(crate) struct Store<T> {
    values: VecList<Index, Gen, Place<T>>,
    strong_counters: Rc<RefCell<StrongCounters>>,
    counters_policy: StrongRefKeeper,
}

impl<T> Store<T> {
    pub(crate) fn new() -> Self {
        Self {
            values: VecList::with_capacity(10),
            strong_counters: Default::default(),
            counters_policy: Default::default(),
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
                self.counters_policy.set(index, StrongRefPolicy::Dealloc);
                index
            }
        };

        let counter = Counter {
            index,
            counters: self.strong_counters.clone(),
        };

        Some(counter)
    }

    pub(crate) fn try_insert(
        &mut self,
        value: T,
        policy: StrongRefPolicy,
    ) -> Result<(Addr, Counter), T> {
        self.insert_weak(value).map(|addr| {
            let counter = self.upgrade(addr).unwrap();
            self.counters_policy.set(counter.index, policy);

            (addr, counter)
        })
    }

    pub(crate) fn insert(&mut self, value: T, policy: StrongRefPolicy) -> (Addr, Counter) {
        self.values.grow();

        let Ok(ptr) = self.try_insert(value, policy) else {
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
                        Some(0) if self.counters_policy.get(counter).is_dealloc() => {
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
            // Remove counters for deallocated objects.
            // Some of those may stay alive up to this point if StrongRefPolicy::Keep was set.
            if let Some(place) = self.values.remove(index.into()) {
                if let Some(index) = place.counter.get() {
                    self.strong_counters.borrow_mut().remove(index);
                }
            }
        }
    }
}

impl<T> AsAny for Store<T>
where
    T: 'static,
{
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
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

impl<T, M, P> Getters<M, P> for Store<T>
where
    T: 'static,
    P: Params,
{
    fn get_value(&self, addr: Addr) -> Option<&dyn std::any::Any> {
        Some(self.get(addr)?)
    }

    fn get_value_mut(&mut self, addr: Addr) -> Option<&mut dyn std::any::Any> {
        Some(self.get_mut(addr)?)
    }

    fn get_userdata(&self, _addr: Addr) -> Option<&(dyn crate::userdata::Userdata<P> + 'static)> {
        None
    }

    fn get_userdata_mut(
        &mut self,
        _addr: Addr,
    ) -> Option<&mut (dyn crate::userdata::Userdata<P> + 'static)> {
        None
    }

    fn get_full_userdata(
        &self,
        _addr: Addr,
    ) -> Option<&(dyn crate::userdata::FullUserdata<M, P> + 'static)> {
        None
    }

    fn get_full_userdata_mut(
        &mut self,
        _addr: Addr,
    ) -> Option<&mut (dyn crate::userdata::FullUserdata<M, P> + 'static)> {
        None
    }

    fn set_dispatcher(&mut self, _dispatcher: &dyn std::any::Any) {}
}

impl<T> Insert<T> for Store<T> {
    fn insert(&mut self, value: T) -> (Addr, Counter) {
        Store::insert(self, value, StrongRefPolicy::Dealloc)
    }

    fn try_insert(&mut self, value: T) -> Result<(Addr, Counter), T> {
        Store::try_insert(self, value, StrongRefPolicy::Dealloc)
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
pub(super) struct CounterIndex(usize);

#[derive(Debug)]
pub(crate) struct Counter {
    index: CounterIndex,
    counters: Rc<RefCell<StrongCounters>>,
}

impl Counter {
    fn new(index: CounterIndex, counters: Rc<RefCell<StrongCounters>>) -> Self {
        counters.borrow_mut().increment(index);

        Counter { index, counters }
    }

    pub(crate) fn weaken(self) -> WeakCounter {
        WeakCounter {
            index: self.index,
            counters: self.counters.clone(),
        }
    }
}

impl Clone for Counter {
    fn clone(&self) -> Self {
        let Counter { index, counters } = self;
        Counter::new(*index, counters.clone())
    }
}

impl Drop for Counter {
    fn drop(&mut self) {
        self.counters.borrow_mut().decrement(self.index);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct WeakCounter {
    index: CounterIndex,
    counters: Rc<RefCell<StrongCounters>>,
}

impl WeakCounter {
    pub(crate) fn upgrade(self) -> Counter {
        let WeakCounter { index, counters } = self;

        Counter::new(index, counters)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Addr {
    pub(crate) index: Index,
    pub(crate) gen: Gen,
}

impl Addr {
    pub(crate) fn index(self) -> usize {
        self.index.into()
    }

    // pub(crate) fn gen(self) -> usize {
    //     self.gen.into()
    // }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Index(NonMaxU32);

impl From<Index> for usize {
    fn from(value: Index) -> Self {
        value.0.get().try_into().unwrap()
    }
}

impl From<usize> for Index {
    fn from(value: usize) -> Self {
        let value = value
            .try_into()
            .ok()
            .and_then(NonMaxU32::new)
            .expect("reached limit of `u32::MAX - 1` allocations for a type");

        Index(value)
    }
}

impl Pointer for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.0.get() as *const ())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Gen(u16);

impl From<Gen> for usize {
    fn from(value: Gen) -> Self {
        value.0.into()
    }
}

impl GenTag for Gen {
    fn new() -> Self {
        Gen(0)
    }

    fn next(self) -> Option<Self> {
        let index = self.0.checked_add(1)?;
        Some(Gen(index))
    }
}

impl Display for Gen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Indicates when it is ok to remove strong counter associated with object.
///
/// This is used to simplify userdata handling.
/// Essentially, userdata have to keep `Root<T>` internally, but that is problematic due to potential cycles.
/// Instead, it keeps `WeakRoot<T>`, which still keeps strong ref counter index and reference to counters
/// but doesn't actually increment the counter.
///
/// This scheme has a problem: counter index may change during lifetime of the object,
/// because under normal circumstances store would deallocate counters that reach 0.
/// It means that when retrieving userdata one must also update internal counter index which is all kind of messy.
///
/// The policy allows to suppress such behavior and let object keep the inital strong counter even when it reaches 0.
/// Passing `Keep` option ensures that strong counter index will not change during lifetime of the object.
/// It will still be removed when object is garbage collected.
#[derive(Debug, Clone, Copy)]
pub(crate) enum StrongRefPolicy {
    /// Deallocate strong reference counter when reaching 0.
    Dealloc,

    /// Never deallocate strong reference counter
    Keep,
}

impl StrongRefPolicy {
    fn into_bit(self) -> bool {
        match self {
            StrongRefPolicy::Dealloc => false,
            StrongRefPolicy::Keep => true,
        }
    }

    fn from_bit(bit: bool) -> Self {
        match bit {
            true => StrongRefPolicy::Keep,
            false => StrongRefPolicy::Dealloc,
        }
    }

    fn is_dealloc(self) -> bool {
        matches!(self, StrongRefPolicy::Dealloc)
    }
}

#[derive(Debug, Default)]
struct StrongRefKeeper(RefCell<BitVec>);

impl StrongRefKeeper {
    fn set(&self, index: CounterIndex, policy: StrongRefPolicy) {
        let index = index.0;
        let bit = policy.into_bit();

        let mut bitvec = self.0.borrow_mut();

        if bit && bitvec.len() < index + 1 {
            bitvec.resize(index + 1, false);
        }

        if let Some(mut place) = bitvec.get_mut(index) {
            *place = bit;
        };
    }

    fn get(&self, index: CounterIndex) -> StrongRefPolicy {
        let bit = self
            .0
            .borrow()
            .get(index.0)
            .map(|bit| *bit)
            .unwrap_or_default();
        StrongRefPolicy::from_bit(bit)
    }
}
