use rt::gc::LuaPtr;
use rt::plugin::Plugin;
use rt::value::{Types, Value};

use crate::traits::TableEntry;

pub struct Std<P>(P);

impl Std<()> {
    pub fn empty() -> Self {
        Std(())
    }
}

impl<P> Std<P> {
    pub fn include<T>(self, part: T) -> Std<(P, T)> {
        let Std(p) = self;
        Std((p, part))
    }
}

impl<Ty, C, T> Plugin<Ty, C> for Std<T>
where
    Ty: Types,
    T: TableEntry<Ty>,
{
    fn build(self, rt: &mut rt::runtime::Runtime<Ty, C>) {
        let table = if let Value::Table(LuaPtr(t)) = &rt.core.global_env {
            t.clone()
        } else {
            rt.core.gc.alloc_cell(Default::default())
        };

        let Std(builder) = self;
        builder.build(&table, &mut rt.core);

        rt.core.global_env = Value::Table(LuaPtr(table));
    }
}
