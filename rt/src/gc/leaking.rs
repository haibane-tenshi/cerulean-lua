use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::ControlFlow;

use super::{Gc, GcUserdata, Sweeper};
use crate::error::RefAccessError;
use crate::value::callable::RustCallable;
use crate::value::string::PossiblyUtf8Vec;
use crate::value::traits::{Borrow, TypeProvider};
use crate::value::userdata::{FullUserdata, Userdata};
use crate::value::Table;

#[derive(Default)]
pub struct LeakingGc(PhantomData<()>);

impl LeakingGc {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Debug for LeakingGc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("LeakingGc").finish()
    }
}

impl TypeProvider for LeakingGc {
    type String = PossiblyUtf8Vec;
    type String = &'static PossiblyUtf8Vec;
    type RustCallable = RustCallable<Self>;
    type Table = Table<Self>;
    type Table = &'static LeakedTableHandle<Self::Table>;
    type FullUserdata = FullUserdata<Self>;
    type FullUserdata = &'static LeakedUserdataHandle<Self::FullUserdata>;
}

impl Gc for LeakingGc {
    type Sweeper<'this> = LeakingGcSweeper;

    fn sweeper(&mut self) -> Self::Sweeper<'_> {
        LeakingGcSweeper(PhantomData)
    }

    fn alloc_string(&mut self, value: Self::String) -> Self::String {
        Box::leak(Box::new(value))
    }

    fn alloc_table(&mut self, value: Self::Table) -> Self::Table {
        Box::leak(Box::new(LeakedTableHandle(RefCell::new(value))))
    }
}

impl<T> GcUserdata<T> for LeakingGc
where
    T: Userdata<Self> + 'static,
{
    fn alloc_userdata_with_meta(
        &mut self,
        value: T,
        metatable: Option<Self::Table>,
    ) -> Self::FullUserdata {
        use crate::value::userdata::UserdataValue;

        let value = LeakedUserdataHandle(UserdataValue {
            value,
            metatable: RefCell::new(metatable),
        });

        Box::leak(Box::new(value))
    }
}

pub struct LeakingGcSweeper(PhantomData<()>);

impl Sweeper for LeakingGcSweeper {
    type Gc = LeakingGc;

    fn mark_string(&mut self, _: &<LeakingGc as TypeProvider>::String) {}

    fn mark_table(&mut self, _: &<LeakingGc as TypeProvider>::Table) -> ControlFlow<()> {
        ControlFlow::Break(())
    }

    fn mark_userdata(&mut self, _: &<LeakingGc as TypeProvider>::FullUserdata) {}

    fn sweep(self) {}
}

pub struct LeakedTableHandle<T>(RefCell<T>);

impl<Ty> Debug for LeakedTableHandle<Table<Ty>>
where
    Ty: TypeProvider<Table = &'static Self> + 'static,
    Ty::String: Debug,
    Ty::RustCallable: Debug,
    Ty::FullUserdata: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("LeakedTableHandle");
        debug.field("addr", &(self as *const _));

        match self.0.try_borrow() {
            Ok(t) => debug.field("table", &t),
            Err(_) => debug.field("table", &"<mutably borrowed>"),
        };

        debug.finish()
    }
}

impl<T> Display for LeakedTableHandle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let addr = self as *const _;

        write!(f, "{{table <{addr:p}>}}")
    }
}

impl<T> PartialEq for LeakedTableHandle<T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<T> Eq for LeakedTableHandle<T> {}

impl<T> Hash for LeakedTableHandle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

impl<T> Borrow<T> for LeakedTableHandle<T> {
    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, RefAccessError> {
        self.0.with_ref(f)
    }

    fn with_mut<R>(&self, f: impl FnOnce(&mut T) -> R) -> Result<R, RefAccessError> {
        self.0.with_mut(f)
    }
}

pub struct LeakedUserdataHandle<T: ?Sized>(T);

impl<T> Debug for LeakedUserdataHandle<T>
where
    T: Debug + ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LeakedTableHandle")
            .field("addr", &(self as *const _))
            .field("userdata", &&self.0)
            .finish()
    }
}

impl<T> Display for LeakedUserdataHandle<T>
where
    T: ?Sized,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let addr = self as *const _;

        write!(f, "{{userdata <{addr:p}>}}")
    }
}

impl<T> PartialEq for LeakedUserdataHandle<T>
where
    T: ?Sized,
{
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<T> Eq for LeakedUserdataHandle<T> where T: ?Sized {}

impl<T> Hash for LeakedUserdataHandle<T>
where
    T: ?Sized,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state);
    }
}

impl<T> Borrow<T> for LeakedUserdataHandle<T>
where
    T: ?Sized,
{
    fn with_ref<R>(&self, f: impl FnOnce(&T) -> R) -> Result<R, RefAccessError> {
        Ok(f(&self.0))
    }

    fn with_mut<R>(&self, _: impl FnOnce(&mut T) -> R) -> Result<R, RefAccessError> {
        Err(crate::error::BorrowError::Mut.into())
    }
}
