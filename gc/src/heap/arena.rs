use bitvec::prelude::{BitSlice, BitVec};
use std::any::Any;

use super::{Addr, Collector, Counter};
use crate::userdata::{Params, Userdata};

pub(crate) trait Traceable {
    fn roots(&self) -> BitVec;
    fn trace(&self, indices: &BitSlice, collector: &mut Collector);
    fn retain(&mut self, indices: &BitSlice);
}

pub(crate) trait Arena<P: Params>: Traceable {
    fn try_insert_any(
        &mut self,
        value: &mut dyn Any,
    ) -> Result<Option<(Addr, Counter)>, IncompatibleType>;
    fn insert_any(&mut self, value: &mut dyn Any) -> Result<(Addr, Counter), IncompatibleType>;

    fn validate(&self, counter: &Counter) -> bool;
    fn upgrade(&self, addr: Addr) -> Option<Counter>;

    fn get_value(&self, addr: Addr) -> Option<&dyn Any>;
    fn get_value_mut(&mut self, addr: Addr) -> Option<&mut dyn Any>;

    fn get_userdata(&self, addr: Addr) -> Option<&(dyn Userdata<P> + 'static)>;
    fn get_userdata_mut(&mut self, addr: Addr) -> Option<&mut (dyn Userdata<P> + 'static)>;

    fn set_dispatcher(&mut self, dispatcher: &dyn Any);
}

impl<P> dyn Arena<P> + '_
where
    P: Params,
{
    pub(crate) fn try_insert<T>(&mut self, value: T) -> Result<(Addr, Counter), T>
    where
        T: 'static,
    {
        let mut value = Some(value);
        let out = self.try_insert_any(&mut value);

        match (value, out) {
            (None, Ok(Some(ptr))) => Ok(ptr),
            (Some(value), Ok(None) | Err(_)) => Err(value),
            _ => unreachable!(),
        }
    }

    pub(crate) fn insert<T>(&mut self, value: T) -> Result<(Addr, Counter), T>
    where
        T: 'static,
    {
        let mut value = Some(value);
        let out = self.insert_any(&mut value);

        match (value, out) {
            (None, Ok(ptr)) => Ok(ptr),
            (Some(value), Err(_)) => Err(value),
            _ => unreachable!(),
        }
    }

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
}

pub(crate) struct IncompatibleType;
