use bitvec::prelude::{BitSlice, BitVec};
use std::any::Any;

use super::userdata_store::{Dispatched, UserdataStore};
use super::{Addr, Collector, Counter};
use crate::userdata::{FullUserdata, Params, Userdata};

pub(crate) trait Traceable {
    fn roots(&self) -> BitVec;
    fn trace(&self, indices: &BitSlice, collector: &mut Collector);
    fn retain(&mut self, indices: &BitSlice);
}

pub(crate) trait AsAny {
    // fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

pub(crate) trait HandleStrongRef {
    fn validate(&self, counter: &Counter) -> bool;
    fn upgrade(&self, addr: Addr) -> Option<Counter>;
}

pub(crate) trait Getters<M, P: Params> {
    fn get_value(&self, addr: Addr) -> Option<&dyn Any>;
    fn get_value_mut(&mut self, addr: Addr) -> Option<&mut dyn Any>;

    fn get_userdata(&self, addr: Addr) -> Option<&(dyn Userdata<P> + 'static)>;
    fn get_userdata_mut(&mut self, addr: Addr) -> Option<&mut (dyn Userdata<P> + 'static)>;

    fn get_full_userdata(&self, addr: Addr) -> Option<&(dyn FullUserdata<M, P> + 'static)>;
    fn get_full_userdata_mut(
        &mut self,
        addr: Addr,
    ) -> Option<&mut (dyn FullUserdata<M, P> + 'static)>;

    fn set_dispatcher(&mut self, dispatcher: &dyn Any);
}

pub(crate) trait Arena<M, P: Params>:
    Traceable + AsAny + HandleStrongRef + Getters<M, P>
{
}

impl<M, P, T> Arena<M, P> for T
where
    P: Params,
    T: Traceable + AsAny + HandleStrongRef + Getters<M, P>,
{
}

impl<M, P> dyn Arena<M, P> + '_
where
    M: 'static,
    P: Params,
{
    pub(crate) fn get<T>(&self, addr: Addr) -> Option<&T>
    where
        T: 'static,
    {
        self.get_value(addr)?.downcast_ref()
    }

    pub(crate) fn get_mut<T>(&mut self, addr: Addr) -> Option<&mut T>
    where
        T: 'static,
    {
        self.get_value_mut(addr)?.downcast_mut()
    }

    // pub(crate) fn cast_as<T>(&self) -> Option<&UserdataStore<T, M, P>>
    // where
    //     T: Dispatched<M, P> + 'static
    // {
    //     self.as_any().downcast_ref()
    // }

    pub(crate) fn cast_as_mut<T>(&mut self) -> Option<&mut UserdataStore<T, M, P>>
    where
        T: Dispatched<M, P> + 'static,
    {
        self.as_any_mut().downcast_mut()
    }
}
