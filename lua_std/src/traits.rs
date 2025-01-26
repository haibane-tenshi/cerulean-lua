use gc::RootCell;

use rt::runtime::Core;
use rt::value::Types;

pub type RootTable<Ty> = RootCell<<Ty as Types>::Table>;

pub trait TableEntry<Ty>
where
    Ty: Types,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>);
}

impl<Ty> TableEntry<Ty> for ()
where
    Ty: Types,
{
    fn build(self, _value: &RootTable<Ty>, _core: &mut Core<Ty>) {}
}

impl<Ty, A, B> TableEntry<Ty> for (A, B)
where
    Ty: Types,
    A: TableEntry<Ty>,
    B: TableEntry<Ty>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let (a, b) = self;
        a.build(value, core);
        b.build(value, core);
    }
}
