use std::hash::{BuildHasher, Hash};
use std::ops::{Deref, DerefMut};

use gc::{Gc, Heap, Root, Trace};
use hashbrown::HashTable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Default, Hash)]
pub struct Interned<T>(T);

impl<T> Interned<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Trace for Interned<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut gc::Collector) {
        self.0.trace(collector)
    }
}

impl<T> AsRef<T> for Interned<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for Interned<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T> Deref for Interned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Interned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) struct Interner<T, H = std::hash::RandomState> {
    hasher: H,
    hash_table: HashTable<Gc<Interned<T>>>,
}

impl<T, H> Interner<T, H>
where
    T: Trace + Hash + Eq,
    H: BuildHasher,
{
    pub(crate) fn insert(&mut self, value: T, heap: &mut Heap) -> Root<Interned<T>> {
        let hash = self.hasher.hash_one(&value);

        let found = self.hash_table.find(hash, |&ptr| {
            heap.get(ptr)
                .map(|other| value == *other.as_ref())
                .unwrap_or(false)
        });

        if let Some(&ptr) = found {
            return heap.upgrade(ptr).unwrap();
        };

        // This can trigger gc pass.
        // Make sure we do it before sweeping dead references from table.
        let value = heap.alloc(Interned(value));

        // Remove dead references.
        self.hash_table.retain(|&mut ptr| heap.get(ptr).is_some());

        self.hash_table
            .insert_unique(hash, value.downgrade(), |&ptr| {
                self.hasher.hash_one(heap.get(ptr).unwrap())
            });
        value
    }
}

impl<T, H> Default for Interner<T, H>
where
    H: Default,
{
    fn default() -> Self {
        Self {
            hasher: Default::default(),
            hash_table: Default::default(),
        }
    }
}
