use gc::RootCell;

use rt::gc::LuaPtr;
use rt::plugin::Plugin;
use rt::runtime::Core;
use rt::value::{Types, Value};

pub(crate) type RootTable<Ty> = RootCell<<Ty as Types>::Table>;

pub trait StdPlugin<Ty>
where
    Ty: Types,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>);
}

impl<Ty> StdPlugin<Ty> for ()
where
    Ty: Types,
{
    fn build(self, _value: &RootTable<Ty>, _core: &mut Core<Ty>) {}
}

impl<Ty, A, B> StdPlugin<Ty> for (A, B)
where
    Ty: Types,
    A: StdPlugin<Ty>,
    B: StdPlugin<Ty>,
{
    fn build(self, value: &RootTable<Ty>, core: &mut Core<Ty>) {
        let (a, b) = self;
        a.build(value, core);
        b.build(value, core);
    }
}

pub struct Std<P>(P);

impl Std<()> {
    pub fn empty() -> Self {
        Std(())
    }
}

impl<P> Std<P> {
    pub fn with<T>(self, part: T) -> Std<(P, T)> {
        let Std(p) = self;
        Std((p, part))
    }
}

impl<Ty, C, T> Plugin<Ty, C> for Std<T>
where
    Ty: Types,
    T: StdPlugin<Ty>,
{
    fn build(self, rt: &mut rt::runtime::Runtime<Ty, C>) {
        let value = rt.core.gc.alloc_cell(Default::default());
        let Std(builder) = self;
        builder.build(&value, &mut rt.core);

        rt.core.global_env = Value::Table(LuaPtr(value));
    }
}
