pub mod leaking;
pub mod rc;

use crate::value::traits::TypeProvider;
use std::ops::ControlFlow;

pub use leaking::LeakingGc;
pub use rc::RcGc;

pub trait Gc: TypeProvider {
    type Sweeper<'this>: Sweeper<Self>
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

pub trait Sweeper<Ty: TypeProvider>: Sized {
    fn mark_string(&mut self, rf: &<Ty as TypeProvider>::StringRef);
    fn mark_table(&mut self, rf: &<Ty as TypeProvider>::TableRef) -> ControlFlow<()>;
    fn mark_userdata(&mut self, rf: &<Ty as TypeProvider>::FullUserdataRef);

    fn sweep(self);

    fn mark_with_visitor(&mut self, iter: impl IntoIterator<Item = impl Visit<Self>>) {
        for item in iter.into_iter() {
            item.visit(self)
        }
    }
}

pub trait Visit<S> {
    fn visit(&self, sweeper: &mut S);
}
