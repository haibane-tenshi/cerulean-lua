use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsString;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::rc::{Rc, Weak};

use super::{Gc, GcUserdata, Sweeper};
use crate::error::{DroppedOrBorrowedError, RuntimeError};
use crate::runtime::RuntimeView;
use crate::value::callable::RustCallable;
use crate::value::string::PossiblyUtf8Vec;
use crate::value::traits::{Borrow, TypeProvider};
use crate::value::userdata::{FullUserdata, Userdata, UserdataValue};
use crate::value::{Table, Value};

#[derive(Debug)]
pub struct RcGc<C> {
    tables: HashMap<usize, (Rc<RefCell<Table<Self>>>, bool)>,
    userdata: HashMap<usize, (Rc<FullUserdata<Self, C>>, bool)>,
    _marker: PhantomData<C>,
}

impl<C> RcGc<C> {
    pub fn new() -> Self {
        Default::default()
    }
}

impl<C> Default for RcGc<C> {
    fn default() -> Self {
        Self {
            tables: Default::default(),
            userdata: Default::default(),
            _marker: Default::default(),
        }
    }
}

impl<C> TypeProvider for RcGc<C> {
    type String = PossiblyUtf8Vec;
    type StringRef = StringRef;
    type RustCallable = RustCallable<Self, C>;
    type Table = Table<Self>;
    type TableRef = TableRef<Self>;
    type FullUserdata = FullUserdata<Self, C>;
    type FullUserdataRef = UserdataRef<Self, C>;
}

impl<C> Gc for RcGc<C> {
    type Sweeper<'this> = RcSweeper<'this, C>
    where
        Self: 'this;

    fn sweeper(&mut self) -> Self::Sweeper<'_> {
        RcSweeper(self)
    }

    fn alloc_string(&mut self, value: Self::String) -> Self::StringRef {
        StringRef(Rc::new(value))
    }

    fn alloc_table(&mut self, value: Self::Table) -> Self::TableRef {
        let handle = Rc::new(RefCell::new(value));
        let index = Rc::as_ptr(&handle) as usize;
        let r = Rc::downgrade(&handle);

        self.tables.insert(index, (handle, false));

        TableRef(r)
    }
}

impl<C, T> GcUserdata<T> for RcGc<C>
where
    T: Userdata<Self, C> + 'static,
{
    fn alloc_userdata_with_meta(
        &mut self,
        value: T,
        metatable: Option<Self::TableRef>,
    ) -> Self::FullUserdataRef {
        let value = UserdataValue {
            value,
            metatable: RefCell::new(metatable),
        };

        let handle = Rc::new(value);
        let index = Rc::as_ptr(&handle) as usize;
        let r = Rc::downgrade(&handle);

        self.userdata.insert(index, (handle, false));

        UserdataRef(r)
    }
}

pub struct RcSweeper<'a, C>(&'a mut RcGc<C>);

impl<'a, C> Sweeper<RcGc<C>> for RcSweeper<'a, C> {
    fn mark_string(&mut self, _: &<RcGc<C> as TypeProvider>::StringRef) {}

    fn mark_table(&mut self, rf: &<RcGc<C> as TypeProvider>::TableRef) -> ControlFlow<()> {
        let index = Weak::as_ptr(&rf.0) as usize;
        if let Some((_, flag)) = self.0.tables.get_mut(&index) {
            let r = if *flag {
                ControlFlow::Break(())
            } else {
                ControlFlow::Continue(())
            };

            *flag = true;

            r
        } else {
            ControlFlow::Break(())
        }
    }

    fn mark_userdata(&mut self, rf: &<RcGc<C> as TypeProvider>::FullUserdataRef) {
        let index = Weak::as_ptr(&rf.0) as *const () as usize;
        if let Some((_, flag)) = self.0.userdata.get_mut(&index) {
            *flag = true;
        }
    }

    fn sweep(self) {
        let Self(gc) = self;

        gc.tables.retain(|_, (_, flag)| {
            let r = *flag;
            *flag = false;
            r
        });

        gc.userdata.retain(|_, (_, flag)| {
            let r = *flag;
            *flag = false;
            r
        });
    }
}

pub struct TableRef<Gc>(pub Weak<RefCell<Table<Gc>>>)
where
    Gc: TypeProvider<TableRef = Self>;

impl<Gc> Borrow<Table<Gc>> for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn with_ref<R>(&self, f: impl FnOnce(&Table<Gc>) -> R) -> Result<R, DroppedOrBorrowedError> {
        self.0.with_ref(f)
    }

    fn with_mut<R>(
        &self,
        f: impl FnOnce(&mut Table<Gc>) -> R,
    ) -> Result<R, DroppedOrBorrowedError> {
        self.0.with_mut(f)
    }
}

impl<Gc> Debug for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
    Gc::StringRef: Debug,
    Gc::RustCallable: Debug,
    Gc::FullUserdataRef: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("TableRef");
        debug.field("addr", &Weak::as_ptr(&self.0));

        match self.0.upgrade() {
            Some(inner) => match inner.try_borrow() {
                Ok(table) => debug.field("table", &table),
                Err(_) => debug.field("table", &"<borrowed>"),
            },
            None => debug.field("table", &"<dropped>"),
        };

        debug.finish()
    }
}

impl<Gc> Display for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let addr = Weak::as_ptr(&self.0);
        let dropped = if Weak::strong_count(&self.0) == 0 {
            "[dropped]"
        } else {
            ""
        };

        write!(f, "{{table{dropped} <{addr:p}>}}")
    }
}

impl<Gc> Clone for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Gc> PartialEq for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl<Gc> Eq for TableRef<Gc> where Gc: TypeProvider<TableRef = Self> {}

impl<Gc> Hash for TableRef<Gc>
where
    Gc: TypeProvider<TableRef = Self>,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Weak::as_ptr(&self.0).hash(state)
    }
}

impl<Gc> From<TableRef<Gc>> for Value<Gc>
where
    Gc: TypeProvider<TableRef = TableRef<Gc>>,
{
    fn from(value: TableRef<Gc>) -> Self {
        Value::Table(value)
    }
}

pub struct UserdataRef<Gc: TypeProvider, C>(
    pub Weak<UserdataValue<dyn Userdata<Gc, C>, Gc::TableRef>>,
);

impl<Gc, C> Userdata<Gc, C> for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn method(
        &self,
        scope: &str,
        name: &str,
        rt: RuntimeView<'_, Gc, C>,
    ) -> Option<Result<(), RuntimeError<Gc>>> {
        use crate::error::AlreadyDroppedError;
        let inner = self.0.clone().upgrade().ok_or(AlreadyDroppedError);
        match inner {
            Ok(inner) => inner.as_ref().method(scope, name, rt),
            Err(err) => Some(Err(err.into())),
        }
    }
}

impl<Gc, C> Borrow<FullUserdata<Gc, C>> for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn with_ref<R>(
        &self,
        f: impl FnOnce(&FullUserdata<Gc, C>) -> R,
    ) -> Result<R, DroppedOrBorrowedError> {
        use crate::error::AlreadyDroppedError;
        let inner = self.0.clone().upgrade().ok_or(AlreadyDroppedError)?;
        Ok(f(&inner))
    }

    fn with_mut<R>(
        &self,
        _f: impl FnOnce(&mut FullUserdata<Gc, C>) -> R,
    ) -> Result<R, DroppedOrBorrowedError> {
        use crate::error::BorrowError;
        Err(BorrowError::Mut.into())
    }
}

impl<Gc, C> Debug for UserdataRef<Gc, C>
where
    Gc: TypeProvider<TableRef = TableRef<Gc>, FullUserdataRef = Self>,
    Gc::StringRef: Debug,
    Gc::RustCallable: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("UserdataRef");
        debug.field("addr", &Weak::as_ptr(&self.0));

        match self.0.upgrade() {
            Some(inner) => {
                debug.field("value", &"<omitted>");
                match inner.metatable.try_borrow() {
                    Ok(metatable) => debug.field("metatable", &metatable),
                    Err(_) => debug.field("metatable", &"<borrowed>"),
                }
            }
            None => debug.field("value", &"<dropped>"),
        };

        debug.finish()
    }
}

impl<Gc, C> Display for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let addr = Weak::as_ptr(&self.0);
        let dropped = if Weak::strong_count(&self.0) == 0 {
            "[dropped]"
        } else {
            ""
        };

        write!(f, "{{userdata{dropped} <{addr:p}>}}")
    }
}

impl<Gc, C> Clone for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<Gc, C> PartialEq for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.0, &other.0)
    }
}

impl<Gc, C> Eq for UserdataRef<Gc, C> where Gc: TypeProvider {}

impl<Gc, C> Hash for UserdataRef<Gc, C>
where
    Gc: TypeProvider,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Weak::as_ptr(&self.0).hash(state);
    }
}

impl<Gc, C> From<UserdataRef<Gc, C>> for Value<Gc>
where
    Gc: TypeProvider<FullUserdataRef = UserdataRef<Gc, C>>,
{
    fn from(value: UserdataRef<Gc, C>) -> Self {
        Value::Userdata(value)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringRef(pub Rc<PossiblyUtf8Vec>);

impl Borrow<PossiblyUtf8Vec> for StringRef {
    fn with_ref<R>(
        &self,
        f: impl FnOnce(&PossiblyUtf8Vec) -> R,
    ) -> Result<R, DroppedOrBorrowedError> {
        self.0.with_ref(f)
    }

    fn with_mut<R>(
        &self,
        f: impl FnOnce(&mut PossiblyUtf8Vec) -> R,
    ) -> Result<R, DroppedOrBorrowedError> {
        self.0.with_mut(f)
    }
}

impl Debug for StringRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0.as_ref())
    }
}

impl Display for StringRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.as_ref())
    }
}

impl From<StringRef> for Vec<u8> {
    fn from(value: StringRef) -> Self {
        value.0.as_ref().into()
    }
}

impl TryFrom<StringRef> for String {
    type Error = <PossiblyUtf8Vec as TryInto<String>>::Error;

    fn try_from(value: StringRef) -> Result<Self, Self::Error> {
        value.0.as_ref().try_into()
    }
}

impl TryFrom<StringRef> for OsString {
    type Error = <PossiblyUtf8Vec as TryInto<OsString>>::Error;

    fn try_from(value: StringRef) -> Result<Self, Self::Error> {
        value.0.as_ref().try_into()
    }
}

impl TryFrom<StringRef> for PathBuf {
    type Error = <PossiblyUtf8Vec as TryInto<PathBuf>>::Error;

    fn try_from(value: StringRef) -> Result<Self, Self::Error> {
        value.0.as_ref().try_into()
    }
}
