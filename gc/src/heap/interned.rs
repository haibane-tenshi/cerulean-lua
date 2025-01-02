use std::borrow::Borrow;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;

use hashbrown::HashTable;

use super::arena::{AsAny, Getters, HandleStrongRef, Traceable};
use super::store::{Addr, Counter, Store};
use crate::index::ToOwned;
use crate::userdata::Params;
use crate::Trace;

/// Thin wrapper around interned value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Interned<T>(T);

impl<T> Interned<T> {
    pub fn as_inner(&self) -> &T {
        &self.0
    }
}

impl<T> Deref for Interned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> AsRef<T> for Interned<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> Trace for Interned<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut super::Collector) {
        let Interned(inner) = self;
        inner.trace(collector);
    }
}

struct Interner<T> {
    hasher: std::hash::RandomState,
    hash_table: HashTable<Addr>,
    _marker: PhantomData<T>,
}

impl<T> Interner<T> {
    fn get<U>(&self, value: &U, store: &Store<Interned<T>>) -> Option<Addr>
    where
        U: Hash + Eq + ?Sized,
        T: Borrow<U>,
    {
        use std::hash::BuildHasher;

        let hash = self.hasher.hash_one(value);

        self.hash_table
            .find(hash, |&ptr| {
                store
                    .get(ptr)
                    .map(|other| value == other.deref().borrow())
                    .unwrap_or(false)
            })
            .copied()
    }

    fn insert(&mut self, addr: Addr, store: &Store<Interned<T>>)
    where
        T: Hash,
    {
        use std::hash::BuildHasher;

        let value = store.get(addr).unwrap().deref();
        let hash = self.hasher.hash_one(value);

        // Remove dead references.
        // This is required because inserting may trigger rehashing and during that we cannot remove elements.
        self.hash_table.retain(|ptr| store.get(*ptr).is_some());

        self.hash_table.insert_unique(hash, addr, |&ptr| {
            self.hasher.hash_one(store.get(ptr).unwrap())
        });
    }
}

impl<T> Default for Interner<T> {
    fn default() -> Self {
        Self {
            hasher: Default::default(),
            hash_table: Default::default(),
            _marker: PhantomData,
        }
    }
}

pub(crate) struct InternedStore<T> {
    store: Store<Interned<T>>,
    interner: Interner<T>,
}

impl<T> InternedStore<T> {
    pub(crate) fn new() -> Self {
        InternedStore {
            store: Store::new(),
            interner: Default::default(),
        }
    }

    // pub(crate) fn get(&self, addr: Addr) -> Option<&Interned<T>> {
    //     self.store.get(addr)
    // }
}

impl<T> InternedStore<T> {
    fn find<U>(&self, value: &U) -> Option<(Addr, Counter)>
    where
        U: Hash + Eq + ?Sized,
        T: Borrow<U>,
    {
        if let Some(addr) = self.interner.get(value, &self.store) {
            let counter = self.store.upgrade(addr).unwrap();
            Some((addr, counter))
        } else {
            None
        }
    }
}

impl<T> InternedStore<T>
where
    T: Hash + Eq,
{
    pub(crate) fn insert(&mut self, value: T) -> (Addr, Counter) {
        if let Some(r) = self.find(&value) {
            return r;
        }

        let (addr, counter) = self.store.insert(Interned(value));
        self.interner.insert(addr, &self.store);

        (addr, counter)
    }

    pub(crate) fn try_insert(&mut self, value: T) -> Result<(Addr, Counter), T> {
        if let Some(r) = self.find(&value) {
            return Ok(r);
        }

        let (addr, counter) = self.store.try_insert(Interned(value)).map_err(|t| t.0)?;
        self.interner.insert(addr, &self.store);

        Ok((addr, counter))
    }

    pub(crate) fn insert_from<U>(&mut self, value: &U) -> (Addr, Counter)
    where
        U: Hash + Eq + ToOwned<T> + ?Sized,
        T: Borrow<U>,
    {
        if let Some(r) = self.find(value) {
            return r;
        }

        self.insert(value.to_owned())
    }

    pub(crate) fn try_insert_from<U>(&mut self, value: &U) -> Result<(Addr, Counter), T>
    where
        U: Hash + Eq + ToOwned<T> + ?Sized,
        T: Borrow<U>,
    {
        if let Some(r) = self.find(value) {
            return Ok(r);
        }

        self.try_insert(value.to_owned())
    }
}

impl<T> Traceable for InternedStore<T>
where
    T: Trace,
{
    fn roots(&self) -> bitvec::prelude::BitVec {
        self.store.roots()
    }

    fn trace(&self, indices: &bitvec::prelude::BitSlice, collector: &mut super::Collector) {
        self.store.trace(indices, collector);
    }

    fn retain(&mut self, indices: &bitvec::prelude::BitSlice) {
        self.store.retain(indices);
    }
}

impl<T> AsAny for InternedStore<T>
where
    T: 'static,
{
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl<T> HandleStrongRef for InternedStore<T> {
    fn validate(&self, counter: &Counter) -> bool {
        self.store.validate(counter)
    }

    fn upgrade(&self, addr: Addr) -> Option<Counter> {
        self.store.upgrade(addr)
    }
}

impl<T, M, P> Getters<M, P> for InternedStore<T>
where
    T: 'static,
    P: Params,
{
    fn get_value(&self, addr: Addr) -> Option<&dyn std::any::Any> {
        Some(self.store.get(addr)?)
    }

    fn get_value_mut(&mut self, _addr: Addr) -> Option<&mut dyn std::any::Any> {
        None
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
