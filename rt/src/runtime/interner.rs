use std::fmt::Debug;
use std::hash::{BuildHasher, Hash};
use std::ops::{Deref, DerefMut};

use enumoid::EnumMap;
use gc::userdata::Params;
use gc::{Gc, Heap, Root, Trace};
use hashbrown::HashTable;

use super::frame::BuiltinMetamethod;

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Default, Hash)]
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

pub struct Interner<T, H = std::hash::RandomState> {
    hasher: H,
    hash_table: HashTable<Gc<Interned<T>>>,
    events: EnumMap<BuiltinMetamethod, Root<Interned<T>>>,
}

impl<T> Interner<T, std::hash::RandomState>
where
    T: Trace + From<&'static str>,
{
    pub fn new<M, P>(heap: &mut Heap<M, P>) -> Self
    where
        P: Params,
    {
        let events = EnumMap::new_with(|event: BuiltinMetamethod| {
            heap.alloc(Interned(event.to_str().into()))
        });

        Interner {
            hasher: Default::default(),
            hash_table: Default::default(),
            events,
        }
    }
}

impl<T, H> Interner<T, H>
where
    T: Trace + Hash + Eq + AsRef<[u8]>,
    H: BuildHasher,
{
    pub(crate) fn insert<M, P>(&mut self, value: T, heap: &mut Heap<M, P>) -> Root<Interned<T>>
    where
        P: Params,
    {
        // All metamethods are prefixed with two underscores.
        // Quick check to avoid going through events on every occasion.
        if value.as_ref().strip_prefix(&[b'_', b'_']).is_some() {
            let event = self.events.iter().find_map(|(event, ptr)| {
                (event.to_str().as_bytes() == value.as_ref()).then(|| ptr.clone())
            });

            if let Some(event) = event {
                return event;
            }
        }

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

    pub(crate) fn event(&self, metamethod: BuiltinMetamethod) -> Gc<Interned<T>> {
        self.events.get(metamethod).downgrade()
    }
}

impl<T, H> Debug for Interner<T, H> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Interner").finish_non_exhaustive()
    }
}
