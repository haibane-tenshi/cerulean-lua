use gc::RootCell;

use rt::runtime::Core;
use rt::value::Types;

pub type RootTable<Ty> = RootCell<<Ty as Types>::Table>;

pub trait TableEntry<Ty, Ex = ()>
where
    Ty: Types,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, _: &mut Ex);
}

impl<Ty, Ex> TableEntry<Ty, Ex> for ()
where
    Ty: Types,
{
    fn build(self, _table: &RootTable<Ty>, _core: &mut Core<Ty>, _: &mut Ex) {}
}

impl<Ty, Ex, A, B> TableEntry<Ty, Ex> for (A, B)
where
    Ty: Types,
    A: TableEntry<Ty, Ex>,
    B: TableEntry<Ty, Ex>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, extra: &mut Ex) {
        let (a, b) = self;
        a.build(table, core, extra);
        b.build(table, core, extra);
    }
}
