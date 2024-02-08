pub mod leaking;
pub mod rc;

use crate::error::BorrowError;
use crate::value::traits::TypeProvider;
use std::ops::ControlFlow;

pub use leaking::LeakingGc;
pub use rc::RcGc;

pub trait Gc: TypeProvider {
    type Sweeper<'this>: Sweeper<Gc = Self>
    where
        Self: 'this;

    fn sweeper(&mut self) -> Self::Sweeper<'_>;

    fn alloc_string(&mut self, value: Self::String) -> Self::StringRef;
    fn alloc_table(&mut self, value: Self::Table) -> Self::TableRef;
}

pub trait GcUserdata<T>: Gc {
    fn alloc_userdata_with_meta(
        &mut self,
        value: T,
        metatable: Option<Self::TableRef>,
    ) -> Self::FullUserdataRef;
    fn alloc_userdata(&mut self, value: T) -> Self::FullUserdataRef {
        self.alloc_userdata_with_meta(value, None)
    }
}

pub trait Sweeper: Sized {
    type Gc: TypeProvider;

    fn mark_string(&mut self, rf: &<Self::Gc as TypeProvider>::StringRef);
    fn mark_table(&mut self, rf: &<Self::Gc as TypeProvider>::TableRef) -> ControlFlow<()>;
    fn mark_userdata(&mut self, rf: &<Self::Gc as TypeProvider>::FullUserdataRef);

    fn sweep(self);

    fn mark_with_visitor(
        &mut self,
        iter: impl IntoIterator<Item = impl Visit<Self>>,
    ) -> Result<(), BorrowError> {
        for item in iter.into_iter() {
            item.visit(self)?;
        }

        Ok(())
    }
}

pub trait Visit<S> {
    fn visit(&self, sweeper: &mut S) -> Result<(), BorrowError>;
}

impl<'a, T, S> Visit<S> for &'a T
where
    T: Visit<S>,
{
    fn visit(&self, sweeper: &mut S) -> Result<(), BorrowError> {
        <T as Visit<S>>::visit(self, sweeper)
    }
}

pub fn visit_borrow<S, T, U>(value: &T, sweeper: &mut S) -> Result<(), BorrowError>
where
    T: crate::value::Borrow<U>,
    U: Visit<S>,
{
    use crate::error::RefAccessError;

    match value.with_ref(|value| value.visit(sweeper)) {
        Ok(Ok(())) | Err(RefAccessError::Dropped(_)) => Ok(()),
        Ok(Err(err)) | Err(RefAccessError::Borrowed(err)) => Err(err),
    }
}
