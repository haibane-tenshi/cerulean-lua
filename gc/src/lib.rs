mod arena;
mod trace;

use std::any::TypeId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Pointer};
use std::marker::PhantomData;
use std::rc::Rc;

use bitvec::vec::BitVec;

use arena::{Arena, ArenaStore};

pub use trace::{Trace, Untrace};

#[derive(Default)]
pub struct Heap {
    arenas: HashMap<TypeId, Box<dyn Arena>>,
}

impl Heap {
    pub fn new() -> Self {
        Default::default()
    }

    fn arena<T>(&self) -> Option<&ArenaStore<T>>
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();
        let arena = self.arenas.get(&id)?;
        arena.as_any().downcast_ref()
    }

    fn arena_mut<T>(&mut self) -> Option<&mut ArenaStore<T>>
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();
        let arena = self.arenas.get_mut(&id)?;
        arena.as_any_mut().downcast_mut()
    }

    fn arena_or_insert<T>(&mut self) -> &mut ArenaStore<T>
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();
        let arena = self
            .arenas
            .entry(id)
            .or_insert_with(|| Box::new(ArenaStore::<T>::new()));
        arena.as_any_mut().downcast_mut().unwrap()
    }

    pub fn alloc<T>(&mut self, value: T) -> Root<T>
    where
        T: Trace,
    {
        let arena = self.arena_or_insert();
        let value = match arena.try_insert(value) {
            Ok(ptr) => return ptr,
            Err(value) => value,
        };

        self.gc();

        self.arena_mut().unwrap().insert(value)
    }

    pub fn get<T>(&self, ptr: Gc<T>) -> Option<&T>
    where
        T: Trace,
    {
        self.arena()?.get(ptr.index)
    }

    pub fn get_mut<T>(&mut self, ptr: Gc<T>) -> Option<&mut T>
    where
        T: Trace,
    {
        self.arena_mut()?.get_mut(ptr.index)
    }

    pub fn upgrade<T>(&mut self, ptr: Gc<T>) -> Option<Root<T>>
    where
        T: Trace,
    {
        self.arena()?.upgrade(ptr)
    }

    fn collector(&self) -> (Collector, Collector) {
        let masks = self
            .arenas
            .iter()
            .map(|(key, arena)| {
                let enqueued = arena.roots();

                (*key, enqueued)
            })
            .collect();

        let queue = Collector { masks };

        let masks = queue
            .masks
            .iter()
            .map(|(key, mask)| {
                let processed = BitVec::repeat(false, mask.len());
                (*key, processed)
            })
            .collect();

        let processed = Collector { masks };

        (queue, processed)
    }

    pub fn gc(&mut self) {
        let processed = {
            let (mut queue, mut processed) = self.collector();

            let mut last_missed_id = None;
            for (id, arena) in self.arenas.iter().cycle() {
                let mut enqueued = queue.take(id).unwrap();
                let processed = processed.masks.get_mut(id).unwrap();

                if !enqueued.any() || {
                    // Filter the bits that are not yet processed.
                    // In principle this should be
                    //
                    //     enqueued = enqueued & !processed
                    //
                    // But we invert queue to avoid cloning processed bitvec.
                    enqueued = !enqueued;
                    enqueued |= processed.as_bitslice();

                    // Queue is still inverted here.
                    enqueued.all()
                } {
                    match last_missed_id {
                        Some(prev_id) if id == prev_id => break,
                        Some(_) => (),
                        None => last_missed_id = Some(id),
                    }
                    continue;
                } else {
                    last_missed_id = None
                }

                // Invert queue back to normal.
                let enqueued = !enqueued;
                *processed |= &enqueued;

                arena.trace(&enqueued, &mut queue);
            }

            processed
        };

        for (id, arena) in self.arenas.iter_mut() {
            let mask = processed.masks.get(id).unwrap();
            arena.retain(mask);
        }
    }
}

pub struct Collector {
    masks: HashMap<TypeId, BitVec>,
}

impl Collector {
    pub fn mark<T>(&mut self, ptr: Gc<T>)
    where
        T: Trace,
    {
        let id = TypeId::of::<T>();

        let Some(mask) = self.masks.get_mut(&id) else {
            return;
        };

        let Some(mut bit) = mask.get_mut(ptr.index) else {
            return;
        };

        *bit = true;
    }

    fn take(&mut self, id: &TypeId) -> Option<BitVec> {
        let mask = self.masks.get_mut(id)?;

        let empty = BitVec::repeat(false, mask.len());
        let mask = std::mem::replace(mask, empty);

        Some(mask)
    }
}

pub struct Gc<T> {
    index: usize,
    _marker: PhantomData<T>,
}

impl<T> Gc<T> {
    pub fn addr(self) -> usize {
        self.index
    }

    pub fn ptr_eq(self, other: Self) -> bool {
        self.index == other.index
    }
}

impl<T> Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Gc")
            .field(&(self.addr() as *const ()))
            .finish()
    }
}

impl<T> Pointer for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.index as *const ())
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Gc<T> {}

pub struct Root<T> {
    index: usize,
    strong_counters: Rc<RefCell<Vec<usize>>>,
    _marker: PhantomData<T>,
}

impl<T> Root<T> {
    pub fn downgrade(&self) -> Gc<T> {
        let Root {
            index,
            strong_counters: _,
            _marker,
        } = self;

        Gc {
            index: *index,
            _marker: PhantomData,
        }
    }

    pub fn addr(&self) -> usize {
        self.index
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Debug for Root<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Root")
            .field(&(self.addr() as *const ()))
            .finish()
    }
}

impl<T> Pointer for Root<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.addr() as *const ())
    }
}

impl<T> Clone for Root<T> {
    fn clone(&self) -> Self {
        let Root {
            index,
            strong_counters,
            _marker,
        } = self;

        strong_counters.borrow_mut()[*index] += 1;

        Root {
            index: *index,
            strong_counters: strong_counters.clone(),
            _marker: PhantomData,
        }
    }
}

impl<T> Drop for Root<T> {
    fn drop(&mut self) {
        self.strong_counters.borrow_mut()[self.index] -= 1;
    }
}

impl<T> From<Root<T>> for Gc<T> {
    fn from(value: Root<T>) -> Self {
        value.downgrade()
    }
}
