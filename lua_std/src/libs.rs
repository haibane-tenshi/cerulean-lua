use std::marker::PhantomData;

use gc::{RootCell, Trace};
use rt::ffi::DLuaFfi;
use rt::plugin::Plugin;
use rt::runtime::{Core, Runtime};
use rt::value::Types;

use crate::traits::{RootTable, TableEntry, TableEntryEx};

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
        builder.build(&table, &mut rt.core);

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
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
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

        builder.build(&local_table, core);

        let value = Value::Table(LuaPtr(local_table.downgrade()));
        core.gc[table].set(key, value);
    }
}

#[doc(hidden)]
pub struct DelayInit<R>(PhantomData<R>);

pub struct MathRand<R, P> {
    rng_state: R,
    builder: P,
}

impl MathRand<(), ()> {
    pub fn empty_with<R>(state: RootCell<R>) -> MathRand<RootCell<R>, ()> {
        MathRand {
            rng_state: state,
            builder: (),
        }
    }

    pub fn empty_from_entropy<R>() -> MathRand<DelayInit<R>, ()> {
        MathRand {
            rng_state: DelayInit(PhantomData),
            builder: (),
        }
    }

    pub fn empty_with_xoshiro() -> MathRand<DelayInit<rand_xoshiro::Xoshiro256StarStar>, ()> {
        MathRand::empty_from_entropy()
    }

    pub fn empty_with_chacha() -> MathRand<DelayInit<rand_chacha::ChaCha12Rng>, ()> {
        MathRand::empty_from_entropy()
    }

    pub fn empty() -> MathRand<DelayInit<rand_xoshiro::Xoshiro256StarStar>, ()> {
        MathRand::empty_with_xoshiro()
    }

    pub fn full_with<R, Ty>(
        state: RootCell<R>,
    ) -> MathRand<RootCell<R>, impl TableEntryEx<Ty, RootCell<R>>>
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
        R: rand::Rng + rand::SeedableRng + 'static,
    {
        use crate::std::math;

        MathRand::empty_with(state)
            .include(math::random)
            .include(math::randomseed)
    }

    pub fn full_from_entropy<R, Ty>() -> MathRand<DelayInit<R>, impl TableEntryEx<Ty, RootCell<R>>>
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
        R: rand::Rng + rand::SeedableRng + 'static,
    {
        use crate::std::math;

        MathRand::empty_from_entropy()
            .include(math::random)
            .include(math::randomseed)
    }

    pub fn full_with_xoshiro<Ty>() -> MathRand<
        DelayInit<rand_xoshiro::Xoshiro256StarStar>,
        impl TableEntryEx<Ty, RootCell<rand_xoshiro::Xoshiro256StarStar>>,
    >
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    {
        MathRand::full_from_entropy()
    }

    pub fn full_with_chacha<Ty>() -> MathRand<
        DelayInit<rand_chacha::ChaCha12Rng>,
        impl TableEntryEx<Ty, RootCell<rand_chacha::ChaCha12Rng>>,
    >
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    {
        MathRand::full_from_entropy()
    }

    pub fn full<Ty>() -> MathRand<
        DelayInit<rand_xoshiro::Xoshiro256StarStar>,
        impl TableEntryEx<Ty, RootCell<rand_xoshiro::Xoshiro256StarStar>>,
    >
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    {
        MathRand::full_with_xoshiro()
    }
}

impl<R, P> MathRand<R, P> {
    pub fn include<T>(self, part: T) -> MathRand<R, (P, T)> {
        let MathRand { rng_state, builder } = self;

        MathRand {
            rng_state,
            builder: (builder, part),
        }
    }
}

impl<Ty, R, P> TableEntry<Ty> for MathRand<RootCell<R>, P>
where
    Ty: Types,
    P: TableEntryEx<Ty, RootCell<R>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        use rt::gc::LuaPtr;
        use rt::value::{KeyValue, TableIndex, Value};

        let key = core.gc.intern("math".into());
        let key = KeyValue::String(LuaPtr(key.downgrade()));
        let local_table = if let Value::Table(LuaPtr(ptr)) = core.gc[table].get(&key) {
            core.gc.upgrade(ptr).unwrap()
        } else {
            core.gc.alloc_cell(Default::default())
        };

        let MathRand {
            mut rng_state,
            builder,
        } = self;

        builder.build(&local_table, core, &mut rng_state);

        let value = Value::Table(LuaPtr(local_table.downgrade()));
        core.gc[table].set(key, value);
    }
}

impl<Ty, R, P> TableEntry<Ty> for MathRand<DelayInit<R>, P>
where
    Ty: Types,
    P: TableEntryEx<Ty, RootCell<R>>,
    R: Trace + rand::SeedableRng,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        use rt::gc::LuaPtr;
        use rt::value::{KeyValue, TableIndex, Value};

        let key = core.gc.intern("math".into());
        let key = KeyValue::String(LuaPtr(key.downgrade()));
        let local_table = if let Value::Table(LuaPtr(ptr)) = core.gc[table].get(&key) {
            core.gc.upgrade(ptr).unwrap()
        } else {
            core.gc.alloc_cell(Default::default())
        };

        let MathRand {
            rng_state: _,
            builder,
        } = self;
        let mut rng_state = core.gc.alloc_cell(R::from_entropy());

        builder.build(&local_table, core, &mut rng_state);

        let value = Value::Table(LuaPtr(local_table.downgrade()));
        core.gc[table].set(key, value);
    }
}
