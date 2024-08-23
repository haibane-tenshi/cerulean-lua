use std::any::Any;
use std::cell::Cell;
use std::fmt::Debug;

use bitvec::slice::BitSlice;
use bitvec::vec::BitVec;

use super::arena::{AsAny, Getters, HandleStrongRef, Traceable};
use super::store::{Addr, Counter, Store};
use super::{Collector, Trace};
use crate::userdata::{Dispatcher, FullUserdata, Metatable, Params, Userdata};

#[derive(Debug)]
pub(crate) struct UserdataStore<T, M, P>
where
    T: Dispatched<M, P>,
{
    dispatcher: <T as Dispatched<M, P>>::Dispatcher,
    store: Store<T>,
}

impl<T, M, P> UserdataStore<T, M, P>
where
    T: Dispatched<M, P>,
{
    pub(crate) fn new() -> Self {
        Self {
            dispatcher: <T as Dispatched<M, P>>::default(),
            store: Store::new(),
        }
    }

    pub(crate) fn get(&self, addr: Addr) -> Option<&T> {
        self.store
            .get(addr)
            .inspect(|value| value.set(self.dispatcher))
    }

    pub(crate) fn get_mut(&mut self, addr: Addr) -> Option<&mut T> {
        self.store
            .get_mut(addr)
            .inspect(|value| value.set(self.dispatcher))
    }

    pub(crate) fn try_insert(&mut self, value: T) -> Result<(Addr, Counter), T> {
        self.store.try_insert(value)
    }

    pub(crate) fn insert(&mut self, value: T) -> (Addr, Counter) {
        self.store.insert(value)
    }
}

impl<T, M, P> Traceable for UserdataStore<T, M, P>
where
    T: Trace + Dispatched<M, P>,
{
    fn roots(&self) -> BitVec {
        self.store.roots()
    }

    fn trace(&self, indices: &BitSlice, collector: &mut Collector) {
        self.store.trace(indices, collector)
    }

    fn retain(&mut self, indices: &BitSlice) {
        self.store.retain(indices)
    }
}

impl<T, M, P> AsAny for UserdataStore<T, M, P>
where
    T: Dispatched<M, P> + 'static,
    M: 'static,
    P: Params,
{
    // fn as_any(&self) -> &dyn Any {
    //     self
    // }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl<T, M, P> HandleStrongRef for UserdataStore<T, M, P>
where
    T: Dispatched<M, P>,
    P: Params,
{
    fn validate(&self, counter: &Counter) -> bool {
        self.store.validate(counter)
    }

    fn upgrade(&self, addr: Addr) -> Option<Counter> {
        self.store.upgrade(addr)
    }
}

impl<T, M, P> Getters<M, P> for UserdataStore<T, M, P>
where
    T: Trace + Dispatched<M, P>,
    P: Params,
{
    fn get_value(&self, addr: Addr) -> Option<&dyn Any> {
        let value = self.get(addr)?.as_inner();
        Some(value)
    }

    fn get_value_mut(&mut self, addr: Addr) -> Option<&mut dyn Any> {
        let value = self.get_mut(addr)?.as_inner_mut();
        Some(value)
    }

    fn get_userdata(&self, addr: Addr) -> Option<&(dyn Userdata<P> + 'static)> {
        self.get(addr)?.as_light()
    }

    fn get_userdata_mut(&mut self, addr: Addr) -> Option<&mut (dyn Userdata<P> + 'static)> {
        self.get_mut(addr)?.as_light_mut()
    }

    fn get_full_userdata(&self, addr: Addr) -> Option<&(dyn FullUserdata<M, P> + 'static)> {
        self.get(addr)?.as_full()
    }

    fn get_full_userdata_mut(
        &mut self,
        addr: Addr,
    ) -> Option<&mut (dyn FullUserdata<M, P> + 'static)> {
        self.get_mut(addr)?.as_full_mut()
    }

    fn set_dispatcher(&mut self, dispatcher: &dyn Any) {
        if let Some(dispatcher) = dispatcher.downcast_ref() {
            self.dispatcher = *dispatcher;
        }
    }
}

pub(crate) trait Dispatched<M, P> {
    type Inner: 'static;
    type Dispatcher: Debug + Copy + 'static;

    fn default() -> Self::Dispatcher;
    fn set(&self, dispatcher: Self::Dispatcher);

    fn as_inner(&self) -> &Self::Inner;
    fn as_inner_mut(&mut self) -> &mut Self::Inner;

    fn as_light(&self) -> Option<&(dyn Userdata<P> + 'static)>;
    fn as_light_mut(&mut self) -> Option<&mut (dyn Userdata<P> + 'static)>;

    fn as_full(&self) -> Option<&(dyn FullUserdata<M, P> + 'static)>;
    fn as_full_mut(&mut self) -> Option<&mut (dyn FullUserdata<M, P> + 'static)>;
}

pub(crate) struct Concrete<T> {
    pub(crate) value: T,
}

impl<T> Concrete<T> {
    pub(crate) fn new(value: T) -> Self {
        Concrete { value }
    }
}

impl<T> Trace for Concrete<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        let Concrete { value } = self;

        value.trace(collector);
    }
}

impl<T, M, P> Dispatched<M, P> for Concrete<T>
where
    T: 'static,
{
    type Inner = T;
    type Dispatcher = ();

    fn default() -> Self::Dispatcher {}
    fn set(&self, _dispatcher: Self::Dispatcher) {}

    fn as_inner(&self) -> &Self::Inner {
        &self.value
    }

    fn as_inner_mut(&mut self) -> &mut Self::Inner {
        &mut self.value
    }

    fn as_light(&self) -> Option<&(dyn Userdata<P> + 'static)> {
        None
    }

    fn as_light_mut(&mut self) -> Option<&mut (dyn Userdata<P> + 'static)> {
        None
    }

    fn as_full(&self) -> Option<&(dyn FullUserdata<M, P> + 'static)> {
        None
    }

    fn as_full_mut(&mut self) -> Option<&mut (dyn FullUserdata<M, P> + 'static)> {
        None
    }
}

pub(crate) struct LightUd<T, P: Params> {
    pub(crate) value: T,
    pub(crate) dispatcher: Cell<Dispatcher<T, P>>,
}

impl<T, P> LightUd<T, P>
where
    T: 'static,
    P: Params,
{
    pub(crate) fn new(value: T) -> Self {
        LightUd {
            value,
            dispatcher: Cell::new(<Self as Dispatched<(), _>>::default()),
        }
    }
}

impl<T, P> Trace for LightUd<T, P>
where
    T: Trace,
    P: Params,
{
    fn trace(&self, collector: &mut Collector) {
        let LightUd {
            value,
            dispatcher: _,
        } = self;

        value.trace(collector);
    }
}

impl<T, M, P> Dispatched<M, P> for LightUd<T, P>
where
    T: 'static,
    P: Params,
{
    type Inner = T;
    type Dispatcher = Dispatcher<T, P>;

    fn default() -> Self::Dispatcher {
        |_, _, _| None
    }

    fn set(&self, dispatcher: Self::Dispatcher) {
        self.dispatcher.set(dispatcher);
    }

    fn as_inner(&self) -> &Self::Inner {
        &self.value
    }

    fn as_inner_mut(&mut self) -> &mut Self::Inner {
        &mut self.value
    }

    fn as_light(&self) -> Option<&(dyn Userdata<P> + 'static)> {
        Some(self)
    }

    fn as_light_mut(&mut self) -> Option<&mut (dyn Userdata<P> + 'static)> {
        Some(self)
    }

    fn as_full(&self) -> Option<&(dyn FullUserdata<M, P> + 'static)> {
        None
    }

    fn as_full_mut(&mut self) -> Option<&mut (dyn FullUserdata<M, P> + 'static)> {
        None
    }
}

impl<T, P> Userdata<P> for LightUd<T, P>
where
    P: Params,
{
    fn method(
        &self,
        ident: <P as Params>::Id<'_>,
        rt: <P as Params>::Rt<'_>,
    ) -> Option<<P as Params>::Res> {
        (self.dispatcher.get())(&self.value, ident, rt)
    }
}

pub(crate) struct FullUd<T, M, P: Params> {
    pub(crate) value: T,
    pub(crate) dispacher: Cell<Dispatcher<T, P>>,
    pub(crate) metatable: Option<M>,
}

impl<T, M, P> FullUd<T, M, P>
where
    T: 'static,
    M: 'static,
    P: Params,
{
    pub(crate) fn new(value: T) -> Self {
        FullUd {
            value,
            dispacher: Cell::new(Self::default()),
            metatable: None,
        }
    }
}

impl<T, M, P> Trace for FullUd<T, M, P>
where
    T: Trace,
    M: Trace,
    P: Params,
{
    fn trace(&self, collector: &mut Collector) {
        let FullUd {
            value,
            dispacher: _,
            metatable,
        } = self;

        value.trace(collector);
        metatable.trace(collector);
    }
}

impl<T, M, P> Dispatched<M, P> for FullUd<T, M, P>
where
    T: 'static,
    M: 'static,
    P: Params,
{
    type Inner = T;
    type Dispatcher = Dispatcher<T, P>;

    fn default() -> Self::Dispatcher {
        |_, _, _| None
    }

    fn set(&self, dispatcher: Self::Dispatcher) {
        self.dispacher.set(dispatcher);
    }

    fn as_inner(&self) -> &Self::Inner {
        &self.value
    }

    fn as_inner_mut(&mut self) -> &mut Self::Inner {
        &mut self.value
    }

    fn as_light(&self) -> Option<&(dyn Userdata<P> + 'static)> {
        Some(self)
    }

    fn as_light_mut(&mut self) -> Option<&mut (dyn Userdata<P> + 'static)> {
        Some(self)
    }

    fn as_full(&self) -> Option<&(dyn FullUserdata<M, P> + 'static)> {
        Some(self)
    }

    fn as_full_mut(&mut self) -> Option<&mut (dyn FullUserdata<M, P> + 'static)> {
        Some(self)
    }
}

impl<T, M, P> Userdata<P> for FullUd<T, M, P>
where
    P: Params,
{
    fn method(&self, ident: P::Id<'_>, rt: P::Rt<'_>) -> Option<P::Res> {
        (self.dispacher.get())(&self.value, ident, rt)
    }
}

impl<T, M, P> Metatable<M> for FullUd<T, M, P>
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
