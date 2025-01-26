use gc::RootCell;

use rt::runtime::Core;
use rt::value::Types;

pub type RootTable<Ty> = RootCell<<Ty as Types>::Table>;

pub trait TableEntry<Ty>
where
    Ty: Types,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>);
}

pub trait TableEntryEx<Ty, Ex = ()>
where
    Ty: Types,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, _: &mut Ex);
}

impl<Ty> TableEntry<Ty> for ()
where
    Ty: Types,
{
    fn build(self, _table: &RootTable<Ty>, _core: &mut Core<Ty>) {}
}

impl<Ty, A, B> TableEntry<Ty> for (A, B)
where
    Ty: Types,
    A: TableEntry<Ty>,
    B: TableEntry<Ty>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let (a, b) = self;
        a.build(table, core);
        b.build(table, core);
    }
}

impl<Ty, Ex> TableEntryEx<Ty, Ex> for ()
where
    Ty: Types,
{
    fn build(self, _table: &RootTable<Ty>, _core: &mut Core<Ty>, _: &mut Ex) {}
}

impl<Ty, Ex, A, B> TableEntryEx<Ty, Ex> for (A, B)
where
    Ty: Types,
    A: TableEntryEx<Ty, Ex>,
    B: TableEntryEx<Ty, Ex>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, extra: &mut Ex) {
        let (a, b) = self;
        a.build(table, core, extra);
        b.build(table, core, extra);
    }
}
