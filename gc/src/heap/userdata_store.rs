use std::any::Any;
use std::cell::Cell;

use bitvec::slice::BitSlice;
use bitvec::vec::BitVec;

use super::arena::{ArenaInfo, AsAny, Getters, HandleStrongRef, HealthCheck, Insert, Traceable};
use super::store::{Addr, Counter, Store, StrongRefPolicy};
use super::{Collector, Trace};
use crate::index::WeakRoot;
use crate::userdata::{Dispatcher, FullUserdata, Metatable, Params, Userdata};

pub(crate) struct UserdataStore<T, P, M>
where
    P: Params,
{
    dispatcher: Dispatcher<T, P>,
    store: Store<UserdataObject<T, P, M>>,
}

impl<T, P, M> UserdataStore<T, P, M>
where
    P: Params,
{
    pub(crate) fn new() -> Self {
        Self {
            dispatcher: |_, _| None,
            store: Store::new(),
        }
    }

    pub(crate) fn get(&self, addr: Addr) -> Option<&UserdataObject<T, P, M>> {
        self.store
            .get(addr)
            .inspect(|value| value.update(self.dispatcher))
    }

    pub(crate) fn get_mut(&mut self, addr: Addr) -> Option<&mut UserdataObject<T, P, M>> {
        self.store
            .get_mut(addr)
            .inspect(|value| value.update(self.dispatcher))
    }

    pub(crate) fn try_insert(
        &mut self,
        value: UserdataObject<T, P, M>,
    ) -> Result<(Addr, Counter), UserdataObject<T, P, M>> {
        self.store.try_insert(value, StrongRefPolicy::Dealloc)
    }

    pub(crate) fn insert(&mut self, value: UserdataObject<T, P, M>) -> (Addr, Counter) {
        self.store.insert(value, StrongRefPolicy::Dealloc)
    }
}

impl<T, P, M> Traceable for UserdataStore<T, P, M>
where
    T: Trace,
    P: Params,
    M: Trace,
{
    fn roots(&self) -> BitVec {
        self.store.roots()
    }

    fn trace(&self, indices: &BitSlice, collector: &mut Collector) {
        self.store.trace(indices, collector);
    }

    fn retain(&mut self, indices: &BitSlice) {
        self.store.retain(indices);
    }
}

impl<T, P, M> AsAny for UserdataStore<T, P, M>
where
    T: 'static,
    P: Params,
    M: 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl<T, P, M> HandleStrongRef for UserdataStore<T, P, M>
where
    P: Params,
{
    fn validate(&self, counter: &Counter) -> bool {
        self.store.validate(counter)
    }

    fn upgrade(&self, addr: Addr) -> Option<Counter> {
        self.store.upgrade(addr)
    }
}

impl<T, M, P> Getters<M, P> for UserdataStore<T, P, M>
where
    T: Trace,
    P: Params,
    M: 'static,
{
    fn get_value(&self, addr: Addr) -> Option<&dyn Any> {
        Some(self.get(addr)?)
    }

    fn get_value_mut(&mut self, addr: Addr) -> Option<&mut dyn Any> {
        Some(self.get_mut(addr)?)
    }

    fn get_userdata(&self, addr: Addr) -> Option<&(dyn Userdata<P> + 'static)> {
        Some(self.get(addr)?)
    }

    fn get_userdata_mut(&mut self, addr: Addr) -> Option<&mut (dyn Userdata<P> + 'static)> {
        Some(self.get_mut(addr)?)
    }

    fn get_full_userdata(&self, addr: Addr) -> Option<&(dyn FullUserdata<M, P> + 'static)> {
        Some(self.get(addr)?)
    }

    fn get_full_userdata_mut(
        &mut self,
        addr: Addr,
    ) -> Option<&mut (dyn FullUserdata<M, P> + 'static)> {
        Some(self.get_mut(addr)?)
    }

    fn set_dispatcher(&mut self, dispatcher: &dyn Any) {
        if let Some(dispatcher) = dispatcher.downcast_ref() {
            self.dispatcher = *dispatcher;
        } else {
            unreachable!("bad dispatcher type")
        }
    }
}

impl<T, P, M> HealthCheck for UserdataStore<T, P, M>
where
    P: Params,
{
    fn health_check(&self) -> ArenaInfo {
        self.store.health_check()
    }
}

impl<T, P, M> Insert<UserdataObject<T, P, M>> for UserdataStore<T, P, M>
where
    P: Params,
{
    fn insert(&mut self, value: UserdataObject<T, P, M>) -> (Addr, Counter) {
        UserdataStore::insert(self, value)
    }

    fn try_insert(
        &mut self,
        value: UserdataObject<T, P, M>,
    ) -> Result<(Addr, Counter), UserdataObject<T, P, M>> {
        UserdataStore::try_insert(self, value)
    }
}

pub(crate) struct UserdataObject<T, P: Params, M> {
    pub(crate) value: WeakRoot<T>,
    pub(crate) dispatcher: Cell<Dispatcher<T, P>>,
    pub(crate) metatable: Option<M>,
}

impl<T, P, M> UserdataObject<T, P, M>
where
    P: Params,
{
    pub(crate) fn new(value: WeakRoot<T>, metatable: Option<M>) -> Self {
        UserdataObject {
            value,
            dispatcher: Cell::new(|_, _| None),
            metatable,
        }
    }

    fn update(&self, dispatcher: Dispatcher<T, P>) {
        self.dispatcher.set(dispatcher);
    }
}

impl<T, P, M> Userdata<P> for UserdataObject<T, P, M>
where
    P: Params,
{
    fn method(&self, ident: <P as Params>::Id<'_>) -> Option<<P as Params>::Res> {
        let this = self.value.clone().upgrade();

        (self.dispatcher.get())(this, ident)
    }
}

impl<T, P, M> Metatable<M> for UserdataObject<T, P, M>
where
    P: Params,
{
    fn metatable(&self) -> Option<&M> {
        self.metatable.as_ref()
    }

    fn set_metatable(&mut self, mt: Option<M>) -> Option<M> {
        std::mem::replace(&mut self.metatable, mt)
    }
}

impl<T, P, M> Trace for UserdataObject<T, P, M>
where
    T: 'static,
    P: Params,
    M: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        self.metatable.trace(collector)
    }
}
