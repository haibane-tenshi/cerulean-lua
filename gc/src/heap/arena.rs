use bitvec::prelude::{BitSlice, BitVec};
use std::alloc::Layout;
use std::any::Any;

use super::{Addr, Collector, Counter};
use crate::userdata::{FullUserdata, Params, Userdata};

pub(crate) trait Traceable {
    fn roots(&self) -> BitVec;
    fn trace(&self, indices: &BitSlice, collector: &mut Collector);
    fn retain(&mut self, indices: &BitSlice);
}

pub(crate) trait AsAny {
    fn as_any(&self) -> &dyn Any;
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

pub(crate) trait HealthCheck {
    fn health_check(&self) -> ArenaInfo;
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ArenaInfo {
    /// Name of type contained inside arena.
    ///
    /// This is only intended for diagnostic needs.
    /// Generated using [`std::any::type_name`], so the same caveats apply.
    #[expect(dead_code)]
    pub(crate) type_name: &'static str,

    /// Layout of a single object.
    ///
    /// Note that some auxiliary structures may be allocated alongside the object which are included here,
    /// so the exact layout might be bigger than layout of the underlying type.
    pub(crate) object_layout: Layout,

    /// Number of occupied memory slots.
    ///
    /// Each occupied slot contains one alive object.
    pub(crate) occupied: usize,

    /// Number of reserved memory slots.
    pub(crate) reserved: usize,

    /// Number of dead memory slots.
    ///
    /// Dead slots exhausted their generation tags and cannot be allocated into ever again.
    pub(crate) dead: usize,
}

pub(crate) trait Arena<M, P: Params>:
    Traceable + AsAny + HandleStrongRef + Getters<M, P> + HealthCheck
{
}

impl<M, P, T> Arena<M, P> for T
where
    P: Params,
    T: Traceable + AsAny + HandleStrongRef + Getters<M, P> + HealthCheck,
{
}

impl<M, P> dyn Arena<M, P> + '_
where
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

    pub(crate) fn downcast_ref<T>(&self) -> Option<&T>
    where
        T: 'static,
    {
        self.as_any().downcast_ref()
    }

    pub(crate) fn downcast_mut<T>(&mut self) -> Option<&mut T>
    where
        T: 'static,
    {
        self.as_any_mut().downcast_mut()
    }
}

pub(crate) trait Insert<T> {
    fn insert(&mut self, value: T) -> (Addr, Counter);
    fn try_insert(&mut self, value: T) -> Result<(Addr, Counter), T>;
}
