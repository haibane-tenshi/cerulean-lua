use std::any::Any;
use std::cell::Cell;

use super::arena::{insert, try_insert, Arena, IncompatibleType, Traceable};
use super::store::{Addr, Counter, Store};
use super::{Collector, Trace};
use crate::userdata::{Dispatcher, FullUserdata, Metatable, Params, Userdata};

struct WithDispatcher<T, P: Params> {
    dispatcher: Cell<Dispatcher<T, P>>,
    value: T,
}

impl<T, P> Trace for WithDispatcher<T, P>
where
    T: Trace,
    P: Params,
{
    fn trace(&self, collector: &mut Collector) {
        let WithDispatcher {
            dispatcher: _,
            value,
        } = self;

        value.trace(collector)
    }
}

impl<T, P> Userdata<P> for WithDispatcher<T, P>
where
    P: Params,
{
    fn method(&self, ident: P::Id<'_>, rt: P::Rt<'_>) -> Option<P::Res> {
        (self.dispatcher.get())(&self.value, ident, rt)
    }
}

impl<T, M, P> Metatable<M> for WithDispatcher<T, P>
where
    T: Metatable<M>,
    P: Params,
{
    fn metatable(&self) -> Option<&M> {
        self.value.metatable()
    }

    fn set_metatable(&mut self, mt: Option<M>) -> Option<M> {
        self.value.set_metatable(mt)
    }
}

pub(crate) struct UserdataStore<T, P: Params> {
    dispatcher: Dispatcher<T, P>,
    store: Store<WithDispatcher<T, P>>,
}

impl<T, P> UserdataStore<T, P>
where
    P: Params,
{
    pub(crate) fn new() -> Self {
        Self {
            dispatcher: |_, _, _| None,
            store: Store::new(),
        }
    }

    fn get(&self, addr: Addr) -> Option<&WithDispatcher<T, P>> {
        self.store.get(addr)
    }

    fn get_mut(&mut self, addr: Addr) -> Option<&mut WithDispatcher<T, P>> {
        self.store.get_mut(addr)
    }
}

impl<T, P> UserdataStore<T, P>
where
    T: Trace,
    P: Params,
{
    pub(crate) fn try_insert(&mut self, value: T) -> Result<(Addr, Counter), T> {
        let value = WithDispatcher {
            dispatcher: Cell::new(self.dispatcher),
            value,
        };

        self.store.try_insert(value).map_err(|ud| ud.value)
    }

    pub(crate) fn insert(&mut self, value: T) -> (Addr, Counter) {
        let value = WithDispatcher {
            dispatcher: Cell::new(self.dispatcher),
            value,
        };

        self.store.insert(value)
    }
}

impl<T, P> Traceable for UserdataStore<T, P>
where
    T: Trace,
    P: Params,
{
    fn roots(&self) -> bitvec::prelude::BitVec {
        self.store.roots()
    }

    fn trace(&self, indices: &bitvec::prelude::BitSlice, collector: &mut Collector) {
        self.store.trace(indices, collector)
    }

    fn retain(&mut self, indices: &bitvec::prelude::BitSlice) {
        self.store.retain(indices)
    }
}

impl<T, M, P> Arena<M, P> for UserdataStore<T, P>
where
    T: Trace,
    P: Params,
{
    fn try_insert_any(
        &mut self,
        value: &mut dyn Any,
    ) -> Result<Option<(Addr, Counter)>, IncompatibleType> {
        try_insert(value, |value| self.try_insert(value))
    }

    fn insert_any(&mut self, value: &mut dyn Any) -> Result<(Addr, Counter), IncompatibleType> {
        insert(value, |value| self.insert(value))
    }

    fn validate(&self, counter: &Counter) -> bool {
        <_ as Arena<M, P>>::validate(&self.store, counter)
    }

    fn upgrade(&self, addr: Addr) -> Option<Counter> {
        <_ as Arena<M, P>>::upgrade(&self.store, addr)
    }

    fn get_value(&self, _addr: Addr) -> Option<&dyn Any> {
        None
    }

    fn get_value_mut(&mut self, _addr: Addr) -> Option<&mut dyn Any> {
        None
    }

    fn get_userdata(&self, addr: Addr) -> Option<&(dyn Userdata<P> + 'static)> {
        let value = self.store.get(addr)?;
        value.dispatcher.set(self.dispatcher);
        Some(value)
    }

    fn get_userdata_mut(&mut self, addr: Addr) -> Option<&mut (dyn Userdata<P> + 'static)> {
        let value = self.store.get_mut(addr)?;
        value.dispatcher.set(self.dispatcher);
        Some(value)
    }

    fn get_full_userdata(&self, _addr: Addr) -> Option<&(dyn FullUserdata<M, P> + 'static)> {
        None
    }

    fn get_full_userdata_mut(
        &mut self,
        _addr: Addr,
    ) -> Option<&mut (dyn FullUserdata<M, P> + 'static)> {
        None
    }

    fn set_dispatcher(&mut self, dispatcher: &dyn Any) {
        if let Some(dispatcher) = dispatcher.downcast_ref() {
            self.dispatcher = *dispatcher;
        }
    }
}

pub(crate) struct WithMetatable<T, M> {
    metatable: Option<M>,
    value: T,
}

impl<T, M> Trace for WithMetatable<T, M>
where
    T: Trace,
    M: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        let WithMetatable { metatable, value } = self;
        metatable.trace(collector);
        value.trace(collector);
    }
}

impl<T, M> Metatable<M> for WithMetatable<T, M> {
    fn metatable(&self) -> Option<&M> {
        self.metatable.as_ref()
    }

    fn set_metatable(&mut self, mt: Option<M>) -> Option<M> {
        std::mem::replace(&mut self.metatable, mt)
    }
}

pub(crate) struct FullUserdataStore<T, M, P: Params> {
    // default: Option<M>,
    store: UserdataStore<WithMetatable<T, M>, P>,
}

impl<T, M, P> FullUserdataStore<T, M, P>
where
    P: Params,
{
    pub(crate) fn new() -> Self {
        FullUserdataStore {
            store: UserdataStore::new(),
        }
    }
}

impl<T, M, P> FullUserdataStore<T, M, P>
where
    T: Trace,
    M: Trace,
    P: Params,
{
    pub(crate) fn try_insert(&mut self, value: T) -> Result<(Addr, Counter), T> {
        let value = WithMetatable {
            metatable: None,
            value,
        };

        self.store.try_insert(value).map_err(|ud| ud.value)
    }

    pub(crate) fn insert(&mut self, value: T) -> (Addr, Counter) {
        let value = WithMetatable {
            metatable: None,
            value,
        };

        self.store.insert(value)
    }
}

impl<T, M, P> Traceable for FullUserdataStore<T, M, P>
where
    T: Trace,
    M: Trace,
    P: Params,
{
    fn roots(&self) -> bitvec::prelude::BitVec {
        self.store.roots()
    }

    fn trace(&self, indices: &bitvec::prelude::BitSlice, collector: &mut Collector) {
        self.store.trace(indices, collector)
    }

    fn retain(&mut self, indices: &bitvec::prelude::BitSlice) {
        self.store.retain(indices)
    }
}

impl<T, M, P> Arena<M, P> for FullUserdataStore<T, M, P>
where
    T: Trace,
    M: Trace,
    P: Params,
{
    fn try_insert_any(
        &mut self,
        value: &mut dyn Any,
    ) -> Result<Option<(Addr, Counter)>, IncompatibleType> {
        try_insert(value, |value| self.try_insert(value))
    }

    fn insert_any(&mut self, value: &mut dyn Any) -> Result<(Addr, Counter), IncompatibleType> {
        insert(value, |value| self.insert(value))
    }

    fn validate(&self, counter: &Counter) -> bool {
        <_ as Arena<M, P>>::validate(&self.store, counter)
    }

    fn upgrade(&self, addr: Addr) -> Option<Counter> {
        <_ as Arena<M, P>>::upgrade(&self.store, addr)
    }

    fn get_value(&self, _addr: Addr) -> Option<&dyn Any> {
        None
    }

    fn get_value_mut(&mut self, _addr: Addr) -> Option<&mut dyn Any> {
        None
    }

    fn get_userdata(&self, _addr: Addr) -> Option<&(dyn Userdata<P> + 'static)> {
        None
    }

    fn get_userdata_mut(&mut self, _addr: Addr) -> Option<&mut (dyn Userdata<P> + 'static)> {
        None
    }

    fn get_full_userdata(&self, addr: Addr) -> Option<&(dyn FullUserdata<M, P> + 'static)> {
        let value = self.store.get(addr)?;
        Some(value)
    }

    fn get_full_userdata_mut(
        &mut self,
        addr: Addr,
    ) -> Option<&mut (dyn FullUserdata<M, P> + 'static)> {
        let value = self.store.get_mut(addr)?;
        Some(value)
    }

    fn set_dispatcher(&mut self, dispatcher: &dyn Any) {
        <_ as Arena<M, P>>::set_dispatcher(&mut self.store, dispatcher)
    }
}
