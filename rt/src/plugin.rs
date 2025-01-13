use crate::runtime::Runtime;
use crate::value::Types;

pub trait Plugin<Ty, C>
where
    Ty: Types,
{
    fn build(self, rt: &mut Runtime<Ty, C>);
}
