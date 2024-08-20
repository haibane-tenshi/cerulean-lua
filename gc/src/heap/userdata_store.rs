use std::any::Any;
use std::cell::Cell;

use super::arena::{Arena, IncompatibleType, Traceable};
use super::store::{Addr, Counter, Store};
use super::{Collector, Trace};
use crate::userdata::{Dispatcher, Params, Userdata};

struct LightUserdata<T, P: Params> {
    dispatcher: Cell<Dispatcher<T, P>>,
    value: T,
}

impl<T, P> Trace for LightUserdata<T, P>
where
    T: Trace,
    P: Params,
{
    fn trace(&self, collector: &mut Collector) {
        let LightUserdata {
            dispatcher: _,
            value,
        } = self;

        value.trace(collector)
    }
}

impl<T, P> Userdata<P> for LightUserdata<T, P>
where
    P: Params,
{
    fn method(&self, ident: P::Id<'_>, rt: P::Rt<'_>) -> Option<P::Res> {
        (self.dispatcher.get())(&self.value, ident, rt)
    }
}

pub(crate) struct UserdataStore<T, P: Params> {
    dispatcher: Dispatcher<T, P>,
    store: Store<LightUserdata<T, P>>,
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

impl<T, P> Arena<P> for UserdataStore<T, P>
where
    T: Trace,
    P: Params,
{
    fn try_insert_any(
        &mut self,
        value: &mut dyn Any,
    ) -> Result<Option<(Addr, Counter)>, IncompatibleType> {
        let place = value.downcast_mut::<Option<T>>().ok_or(IncompatibleType)?;
        let value = place.take().ok_or(IncompatibleType)?;

        let value = LightUserdata {
            dispatcher: Cell::new(self.dispatcher),
            value,
        };

        let r = match self.store.try_insert(value) {
            Ok(ptr) => Some(ptr),
            Err(LightUserdata { value, .. }) => {
                *place = Some(value);
                None
            }
        };

        Ok(r)
    }

    fn insert_any(&mut self, value: &mut dyn Any) -> Result<(Addr, Counter), IncompatibleType> {
        let value = value
            .downcast_mut::<Option<T>>()
            .ok_or(IncompatibleType)?
            .take()
            .ok_or(IncompatibleType)?;

        let value = LightUserdata {
            dispatcher: Cell::new(self.dispatcher),
            value,
        };

        let r = self.store.insert(value);

        Ok(r)
    }

    fn validate(&self, counter: &Counter) -> bool {
        <_ as Arena<P>>::validate(&self.store, counter)
    }

    fn upgrade(&self, addr: Addr) -> Option<Counter> {
        <_ as Arena<P>>::upgrade(&self.store, addr)
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

    fn set_dispatcher(&mut self, dispatcher: &dyn Any) {
        if let Some(dispatcher) = dispatcher.downcast_ref() {
            self.dispatcher = *dispatcher;
        }
    }
}
