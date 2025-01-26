use rt::ffi::DLuaFfi;
use rt::plugin::Plugin;
use rt::runtime::{Core, Runtime};
use rt::value::Types;

use crate::traits::{RootTable, TableEntry};

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
    Ty::Table: std::fmt::Debug,
{
    fn build(self, rt: &mut Runtime<Ty, C>) {
        use rt::gc::LuaPtr;
        use rt::value::Value;

        let table = if let Value::Table(LuaPtr(t)) = &rt.core.global_env {
            t.clone()
        } else {
            rt.core.gc.alloc_cell(Default::default())
        };

        let Std(builder) = self;
        builder.build(&table, &mut rt.core, &mut ());

        rt.core.global_env = Value::Table(LuaPtr(table));
    }
}

/// Math utilities library residing in `math` table.
///
/// This library module will use table under `math` key in parent table or construct a new one otherwise.
/// All included items will be put into the table, potentially overriding existing entries.
///
/// [`Math::full`] will construct a table including all functions from [Lua std's `math` library][lua#6.7]
/// except `random` and `randomseed` which exist in a dedicated [`MathRand`] module.
/// Read below for full list of provided APIs.
///
/// Alternatively, you can start with [`Math::empty`] and manually fill in functions from [`std::math`](crate::std::math) or elsewhere:
///
/// ```
/// # use lua_std::lib::{Math, Std};
/// use lua_std::std;
///
/// let global_env = Std::empty()
///     .include(
///         Math::empty()
///             .include(std::math::min)
///             .include(std::math::max)
///     );
/// ```
///
/// # Provided APIs
///
/// **Constants:**
///
/// * [`mininteger`](crate::std::math::mininteger)
/// * [`maxinteger`](crate::std::math::maxinteger)
/// * [`huge`](crate::std::math::huge) - float positive infinity constant
/// * [`pi`](crate::std::math::pi)
///
/// **Float-to-integer conversions:**
///
/// * [`floor`](crate::std::math::floor)
/// * [`ceil`](crate::std::math::ceil)
/// * [`modf`](crate::std::math::modf) - separate float into integral and fractional part
/// * [`tointeger`](crate::std::math::tointeger) - convert float without fractional part into integer
///
/// **Utility:**
///
/// * [`type`](crate::std::math::type_) - discriminate between integers and floats
/// * [`min`](crate::std::math::min)
/// * [`max`](crate::std::math::max)
/// * [`fmod`](crate::std::math::fmod) - compute remainder where quotient is rounded towards 0
/// * [`ult`](crate::std::math::ult) - compare two integers reinterpreted as unsigned integers
///
/// **Trigonometry:**
///
/// * [`sin`](crate::std::math::sin)
/// * [`cos`](crate::std::math::cos)
/// * [`tan`](crate::std::math::tan)
/// * [`asin`](crate::std::math::asin)
/// * [`acos`](crate::std::math::acos)
/// * [`atan`](crate::std::math::atan)
/// * [`deg`](crate::std::math::deg)
/// * [`rad`](crate::std::math::rad)
///
/// **Fundamental functions:**
///
/// * [`abs`](crate::std::math::abs)
/// * [`sqrt`](crate::std::math::sqrt)
/// * [`exp`](crate::std::math::exp)
/// * [`log`](crate::std::math::log)
///
/// [lua#6.7]: https://www.lua.org/manual/5.4/manual.html#6.7
pub struct Math<P>(P);

impl Math<()> {
    /// Construct empty module.
    ///
    /// This library module will use table under `math` key in parent table or construct a new one otherwise.
    /// All included items will be put into the table, potentially overriding existing entries.
    ///
    /// Table entries can included using [`include`](Math::include) method.
    pub fn empty() -> Self {
        Math(())
    }

    /// Construct table filled with all [Lua std's math APIs][lua#6.7] excluding random number generation.
    ///
    /// This library module will use table under `math` key in parent table or construct a new one otherwise.
    /// All included items will be put into the table, potentially overriding existing entries.
    ///
    /// See [provided APIs](Math#provided-apis) for full list.
    ///
    /// [lua#6.7]: https://www.lua.org/manual/5.4/manual.html#6.7
    pub fn full<Ty>() -> Math<impl TableEntry<Ty>>
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    {
        use crate::std::math;

        Math::empty()
            .include(math::abs)
            .include(math::acos)
            .include(math::asin)
            .include(math::atan)
            .include(math::cos)
            .include(math::sin)
            .include(math::tan)
            .include(math::ceil)
            .include(math::floor)
            .include(math::deg)
            .include(math::rad)
            .include(math::exp)
            .include(math::log)
            .include(math::sqrt)
            .include(math::fmod)
            .include(math::modf)
            .include(math::ult)
            .include(math::tointeger)
            .include(math::type_)
            .include(math::max)
            .include(math::min)
            .include(math::huge)
            .include(math::maxinteger)
            .include(math::mininteger)
            .include(math::pi)
    }
}

impl<P> Math<P> {
    /// Include an API in the module.
    pub fn include<T>(self, part: T) -> Math<(P, T)> {
        let Math(p) = self;
        Math((p, part))
    }
}

impl<Ty, P> TableEntry<Ty> for Math<P>
where
    Ty: Types,
    P: TableEntry<Ty>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, _: &mut ()) {
        use rt::gc::LuaPtr;
        use rt::value::{KeyValue, TableIndex, Value};

        let key = core.gc.intern("math".into());
        let key = KeyValue::String(LuaPtr(key.downgrade()));
        let local_table = if let Value::Table(LuaPtr(ptr)) = core.gc[table].get(&key) {
            core.gc.upgrade(ptr).unwrap()
        } else {
            core.gc.alloc_cell(Default::default())
        };

        let Math(builder) = self;

        builder.build(&local_table, core, &mut ());

        let value = Value::Table(LuaPtr(local_table.downgrade()));
        core.gc[table].set(key, value);
    }
}
