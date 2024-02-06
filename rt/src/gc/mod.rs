pub mod leaking;

use crate::value::traits::TypeProvider;
use std::ops::ControlFlow;

pub use leaking::LeakingGc;

pub trait Gc: TypeProvider {
    type Sweeper: Sweeper<Self>;

    fn sweeper(&mut self) -> Self::Sweeper;

    fn alloc_string(&mut self, value: Self::String) -> Self::String;
    fn alloc_table(&mut self, value: Self::Table) -> Self::TableRef;
}

pub trait GcUserdata<T>: Gc {
    fn alloc_userdata(&mut self, value: T) -> Self::FullUserdataRef;
}

pub trait Sweeper<Ty: TypeProvider>: Sized {
    fn mark_string(&mut self, rf: &<Ty as TypeProvider>::String);
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
