use gc::Root;
use std::fmt::Debug;

use crate::gc::Heap;
use crate::value::{Types, WeakValue};

pub(crate) struct VariadicRegister<Ty>
where
    Ty: Types,
{
    values: Vec<WeakValue<Ty>>,
    mirror: Root<Vec<WeakValue<Ty>>>,
}

impl<Ty> VariadicRegister<Ty>
where
    Ty: Types,
{
    pub(crate) fn new(
        values: impl IntoIterator<Item = WeakValue<Ty>>,
        heap: &mut Heap<Ty>,
    ) -> Self {
        let values: Vec<_> = values.into_iter().collect();
        let mirror = heap.alloc(values.clone());

        VariadicRegister { values, mirror }
    }

    pub(crate) fn values(&self) -> &[WeakValue<Ty>] {
        &self.values
    }
}

impl<Ty> Debug for VariadicRegister<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VariadicRegister")
            .field("values", &self.values)
            .finish()
    }
}
