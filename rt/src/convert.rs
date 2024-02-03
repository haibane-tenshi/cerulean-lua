use crate::value::Value;

pub trait Convert<T, C> {
    type Error;
    
    fn convert(&self, gc: &mut (), value: T) -> Result<Value<C>, (T, Self::Error)>;
}

#[derive(Debug, Clone, Copy)]
pub struct Default;

impl<C> Convert<crate::value::Nil, C> for Default {
    type Error = std::convert::Infallible;

    fn convert(&self, gc: &mut (), value: crate::value::Nil) -> Result<Value<C>, (crate::value::Nil, Self::Error)> {
        use crate::value::Nil;
        let Nil = value;
        Ok(Value::Nil)
    }
}
