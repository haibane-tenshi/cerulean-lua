use crate::value::{Nil, StrongValue, Types, Value};

pub trait Convert<Ty, Conv, Heap, T>
where
    Ty: Types,
{
    fn convert(&mut self, gc: &mut Heap, value: T) -> StrongValue<Ty>;
}

#[derive(Debug, Clone, Copy)]
pub struct Default;

impl<Ty, Conv, Heap> Convert<Ty, Conv, Heap, Nil> for Default
where
    Ty: Types,
{
    fn convert(&mut self, _gc: &mut Heap, _value: Nil) -> StrongValue<Ty> {
        Value::Nil
    }
}
