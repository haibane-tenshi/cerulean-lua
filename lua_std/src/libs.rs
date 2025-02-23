use std::marker::PhantomData;
use std::process::Command;

use gc::{RootCell, Trace};
use rt::ffi::{DLuaFfi, LuaFfi};
use rt::plugin::Plugin;
use rt::runtime::{Core, Runtime};
use rt::value::Types;

use crate::traits::{RootTable, TableEntry, TableEntryEx};

pub struct Std<P>(P);

impl Std<empty::Empty> {
    pub fn empty() -> Self {
        Std(empty::Empty(()))
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

/// Place APIs into a custom table.
///
/// This library module will use table under specified name in parent table or construct a new one otherwise.
/// All included items will be put into the table, potentially overriding existing entries.
///
/// This module allows you to introduce customized structures:
///
/// ```
/// # use lua_std::lib::{Std, Table};
/// use lua_std::std;
///
/// let std = Std::empty()
///     .include(Table::with_name("math")
///         .include(std::math::type_)
///         .include(Table::with_name("float")
///             .include(std::math::pi)
///             .include(std::math::sin)
///         )
///     );
/// ```
pub struct Table<S, P> {
    name: S,
    builder: P,
}

impl<S> Table<S, empty::Empty>
where
    S: AsRef<str>,
{
    /// Construct new module and place under specified name into parent table.
    pub fn with_name(name: S) -> Self {
        Table {
            name,
            builder: empty::Empty(()),
        }
    }
}

impl<S, P> Table<S, P> {
    /// Include API in the module.
    pub fn include<T>(self, part: T) -> Table<S, (P, T)> {
        let Table { name, builder } = self;

        Table {
            name,
            builder: (builder, part),
        }
    }
}

impl<Ty, S, P> TableEntry<Ty> for Table<S, P>
where
    Ty: Types,
    S: AsRef<str>,
    P: TableEntry<Ty>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        use rt::gc::LuaPtr;
        use rt::value::{KeyValue, TableIndex, Value};

        let Table { name, builder } = self;

        let key = core.gc.intern(name.as_ref().into());
        let key = KeyValue::String(LuaPtr(key.downgrade()));
        let local_table = if let Value::Table(LuaPtr(ptr)) = core.gc[table].get(&key) {
            core.gc.upgrade(ptr).unwrap()
        } else {
            core.gc.alloc_cell(Default::default())
        };

        builder.build(&local_table, core);

        let value = Value::Table(LuaPtr(local_table.downgrade()));
        core.gc[table].set(key, value);
    }
}

impl<Ty, Ex, S, P> TableEntryEx<Ty, Ex> for Table<S, P>
where
    Ty: Types,
    S: AsRef<str>,
    P: TableEntryEx<Ty, Ex>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, extra: &mut Ex) {
        use rt::gc::LuaPtr;
        use rt::value::{KeyValue, TableIndex, Value};

        let Table { name, builder } = self;

        let key = core.gc.intern(name.as_ref().into());
        let key = KeyValue::String(LuaPtr(key.downgrade()));
        let local_table = if let Value::Table(LuaPtr(ptr)) = core.gc[table].get(&key) {
            core.gc.upgrade(ptr).unwrap()
        } else {
            core.gc.alloc_cell(Default::default())
        };

        builder.build(&local_table, core, extra);

        let value = Value::Table(LuaPtr(local_table.downgrade()));
        core.gc[table].set(key, value);
    }
}

/// Inline a group of APIs directly into parent table.
///
/// Unlike [`Table`], using it will not introduce an intermediate table and place all APIs directly into parent.
/// This can be useful for organizational purposes.
pub struct Inline<P>(P);

impl Inline<empty::Empty> {
    /// Construct new inlined module.
    #[expect(clippy::new_without_default)]
    pub fn new() -> Self {
        Inline(empty::Empty(()))
    }
}

impl<P> Inline<P> {
    /// Include API in the module.
    pub fn inlcude<T>(self, part: T) -> Inline<(P, T)> {
        let Inline(builder) = self;
        Inline((builder, part))
    }
}

impl<Ty, P> TableEntry<Ty> for Inline<P>
where
    Ty: Types,
    P: TableEntry<Ty>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let Inline(builder) = self;
        builder.build(table, core);
    }
}

impl<Ty, Ex, P> TableEntryEx<Ty, Ex> for Inline<P>
where
    Ty: Types,
    P: TableEntryEx<Ty, Ex>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>, extra: &mut Ex) {
        let Inline(builder) = self;
        builder.build(table, core, extra);
    }
}

/// Math utilities library residing in `math` table.
///
/// This library module will use table under `math` key in parent table or construct a new one otherwise.
/// All included items will be put into the table, potentially overriding existing entries.
///
/// [`Math::full`] will construct module introducing all math APIs included into [Lua std's math library][lua#6.7]
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
/// # From Lua documentation
///
/// This library provides basic mathematical functions.
/// It provides all its functions and constants inside the table `math`.
/// Functions with the annotation "integer/float" give integer results for integer arguments and float results for non-integer arguments.
/// The rounding functions `math.ceil`, `math.floor`, and `math.modf` return an integer when the result fits in the range of an integer, or a float otherwise.
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

impl Math<empty::Empty> {
    /// Construct empty module.
    ///
    /// This library module will use table under `math` key in parent table or construct a new one otherwise.
    /// All included items will be put into the table, potentially overriding existing entries.
    ///
    /// Table entries can included using [`include`](Math::include) method.
    pub fn empty() -> Math<empty::Empty> {
        use empty::Empty;

        Math(Empty(()))
    }

    /// Construct module introducing all math APIs included into [Lua std's math library][lua#6.7] excluding random number generation.
    ///
    /// This library module will use table under `math` key in parent table or construct a new one otherwise.
    /// All included items will be put into the table, potentially overriding existing entries.
    ///
    /// See [provided APIs](Math#provided-apis) for full list.
    ///
    /// [lua#6.7]: https://www.lua.org/manual/5.4/manual.html#6.7
    pub fn full() -> Math<math::Full> {
        use math::Full;

        Math(Full(()))
    }
}

impl<P> Math<P> {
    /// Include API in the module.
    ///
    /// Requires [`T: TableEntry<Ty>`](TableEntry) bound.
    /// It is not spelled out because it leaks `Ty` into signature and in some situations compiler requires type hints to figure this type.
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
        let Math(builder) = self;
        Table::with_name("math").include(builder).build(table, core);
    }
}

/// Random number generation library residing in `math` table.
///
/// This library module will use table under `math` key in parent table or construct a new one otherwise.
/// All included items will be put into the table, potentially overriding existing entries.
///
/// This library module is designed to introduce `random` and `randomseed` functions from [Lua std's `math` library][lua#6.7].
/// It is separated from [`Math`] module because, first, functions inside are intended to share state,
/// second, you likely want to configure it to your needs.
///
/// The easiest way to get started is to use [`MathRand::full`] method.
/// This will include both `random` and `randomseed` functions and configure them to use default PRNG.
/// Alternatively, you can start with [`MathRand::empty`] to decide which APIs to include:
///
/// ```
/// # use lua_std::lib::{Std, MathRand};
/// use lua_std::std;
///
/// let global_env = Std::empty()
///     .include(
///         MathRand::empty()
///             .include(std::math::random)
///             .include(std::math::randomseed)
///     );
/// ```
///
/// [lua#6.7]: https://www.lua.org/manual/5.4/manual.html#6.7
///
/// # Default configuration
///
/// By default this library will choose for you [`xoshiro256**` algorithm](rand_xoshiro::Xoshiro256StarStar).
/// It is a known general purpose algorithm providing both high quality and performance.
/// However, it is **not cryptographically secure** and should not be used as such.
///
/// Default state is seeded using [system entropy](rand::SeedableRng::from_entropy).
///
/// Note that default configuration may change between releases (although it is unlikely).
/// If you want to have stable behavior consider providing explicit configuration.
///
/// # Custom configuration
///
/// Functions that can be [`include`](MathRand::include)d into this module require to implement [`TableEntryEx`] trait.
/// The [`build`](TableEntryEx::build) method will receive reference to RNG state in the extra parameter (as `RootCell<Rng>`).
/// The entries are expected to memoize it internally, which both `random` and `randomseed` provided by this library do.
///
/// This setup means that the library module is in charge of configuring and constructing RNG state.
/// You can do so by going through [`MathRand::builder`].
/// It is a simple 2-step builder, where first you get to specify RNG and the way it is initialized
/// and next you can choose library configuration (`empty` which does nothing or `full` which introduces all provided APIs).
/// When it comes to RNG configuration there are a few options.
///
/// [`with_default`](MathRandBuilder::with_default) will simply choose default RNG.
///
/// If you only intend to fix algorithm, you may use [`with_xoshiro`](MathRandBuilder::with_xoshiro):
///
/// ```
/// # use lua_std::lib::{Std, MathRand};
/// let global_env = Std::empty()
///     .include(
///         MathRand::builder()
///             .with_xoshiro()
///             .full()
///     );
/// ```
///
/// We also provide a preconfig for [ChaCha algorithm with 12 rounds](rand_chacha::ChaCha12Rng).
/// It is **cryptographically secure** algorithm, and can be easily configured using [`with_chacha`](MathRandBuilder::with_chacha):
///
/// ```
/// # use lua_std::lib::{Std, MathRand};
/// use lua_std::std;
///
/// let global_env = Std::empty()
///     .include(
///         MathRand::builder()
///             .with_chacha()
///             .empty()
///             .include(std::math::random)
///     );
/// ```
///
/// Note that you may want to remove or replace `randomseed`:
/// for certain inputs it permits seeding state with only 64 bits which is not suitable for cryptography.
/// Obviously, if you actually intend to use this for cryptography it might be better to use custom configuration attuned for your needs,
/// or even expose a set of dedicated APIs for the purpose.
/// This configuration is provided on merits of an easy-to-enable stopgap measure.
///
/// Lastly, you can provide a custom RNG directly using [`with`](MathRandBuilder::with):
///
/// ```
/// # use rand::SeedableRng;
/// # use rand_hc::Hc128Rng;
/// # use gc::Heap;
/// # use gc::userdata::UnitParams;
/// # use lua_std::lib::{Std, MathRand};
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// use lua_std::lib::UntraceRng;
/// use lua_std::std;
///
/// // Init your own RNG.
/// // `UntraceRng` provides noop `Trace` impl which is required for alloc.
/// let rng = heap.alloc_cell(UntraceRng(Hc128Rng::from_entropy()));
///
/// let global_env = Std::empty()
///     .include(
///         MathRand::builder()
///             .with(rng)
///             .empty()
///             .include(std::math::random)
///     );
/// ```
///
/// Additionally, there is [`from_entropy`](MathRandBuilder::from_entropy) as a convenience for the above.
/// It will construct RNG instance from system entropy and place it on heap for you.
/// However, it does require to specify type parameter explicitly:
///
/// ```
/// # use rand::SeedableRng;
/// # use rand_hc::Hc128Rng;
/// # use gc::Heap;
/// # use gc::userdata::UnitParams;
/// # use lua_std::lib::{Std, MathRand};
/// #
/// # let mut heap = Heap::<(), UnitParams>::new();
/// use lua_std::std;
///
/// let global_env = Std::empty()
///     .include(
///         MathRand::builder()
///             .from_entropy::<Hc128Rng>()
///             // Wrap type in `UntraceRng` so it can be placed on the heap.
///             // You can also do it directly when passing to `from_entropy`.
///             // This way it looks a bit cleaner.
///             .untrace()
///             .empty()
///             .include(std::math::random)
///     );
/// ```
///
/// # Provided APIs
///
/// **Random number generation:**
///
/// * [`random`](crate::std::math::random)
/// * [`randomseed`](crate::std::math::randomseed)
pub struct MathRand<R, P> {
    rng_state: R,
    builder: P,
}

impl MathRand<(), empty::Empty> {
    /// Construct a builder to configure this module.
    ///
    /// See [`MathRand` documentation](MathRand#custom-configuration) for usage examples.
    pub fn builder() -> MathRandBuilder<math_rand_builder::Start> {
        use math_rand_builder::Start;

        MathRandBuilder(Start(()))
    }

    /// Construct empty module.
    ///
    /// This library module will use table under `math` key in parent table or construct a new one otherwise.
    /// All included items will be put into the table, potentially overriding existing entries.
    ///
    /// Module will be configured to use [default RNG configuration](MathRand#default-configuration).
    ///
    /// Table entries can included using [`include`](MathRand::include) method.
    pub fn empty() -> MathRand<DelayInit<impl rand::Rng + rand::SeedableRng + Trace>, empty::Empty>
    {
        MathRand::builder().with_xoshiro().empty()
    }

    /// Construct module introducing all APIs for random number generation included into [Lua std's math library][lua#6.7].
    ///
    /// This library module will use table under `math` key in parent table or construct a new one otherwise.
    /// All included items will be put into the table, potentially overriding existing entries.
    ///
    /// Module will be configured to use [default RNG configuration](MathRand#default-configuration).
    ///
    /// See [provided APIs](MathRand#provided-apis) for full list.
    ///
    /// [lua#6.7]: https://www.lua.org/manual/5.4/manual.html#6.7
    pub fn full() -> MathRand<DelayInit<impl rand::Rng + rand::SeedableRng + Trace>, math_rand::Full>
    {
        MathRand::builder().with_xoshiro().full()
    }
}

impl<R, P> MathRand<R, P> {
    /// Include API in this module.
    ///
    /// Requires [`T: TableEntryEx<Ty, R>`](TableEntryEx) bound.
    /// It is not spelled out because it leaks `Ty` into signature and in some situations compiler requires type hints to figure this type.
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
        let MathRand {
            mut rng_state,
            builder,
        } = self;
        Table::with_name("math")
            .include(builder)
            .build(table, core, &mut rng_state);
    }
}

impl<Ty, R, P> TableEntry<Ty> for MathRand<DelayInit<R>, P>
where
    Ty: Types,
    P: TableEntryEx<Ty, RootCell<R>>,
    R: Trace + rand::SeedableRng,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let MathRand {
            rng_state: _,
            builder,
        } = self;
        let mut rng_state = core.gc.alloc_cell(R::from_entropy());
        Table::with_name("math")
            .include(builder)
            .build(table, core, &mut rng_state);
    }
}

/// Typestate indicating delayed RNG construction in `MathRand`.
pub struct DelayInit<R>(PhantomData<R>);

mod math_rand_builder {
    #[doc(hidden)]
    pub struct Start(pub(crate) ());

    #[doc(hidden)]
    pub struct WithRng<R>(pub(crate) R);
}

/// Builder for [`MathRand`] module.
///
/// This is a simple 2-step builder.
/// First you may specify RNG and the way it is constructed, and second choose library configuration (`empty` or `full`).
///
/// See [`MathRand` documentation](MathRand#custom-configuration) for usage examples.
pub struct MathRandBuilder<S>(S);

impl MathRandBuilder<math_rand_builder::Start> {
    /// Construct new builder.
    #[expect(clippy::new_without_default)]
    pub fn new() -> Self {
        use math_rand_builder::Start;

        MathRandBuilder(Start(()))
    }

    /// Configure module to use provided RNG state.
    pub fn with<R>(
        self,
        rng_state: RootCell<R>,
    ) -> MathRandBuilder<math_rand_builder::WithRng<RootCell<R>>> {
        use math_rand_builder::WithRng;

        MathRandBuilder(WithRng(rng_state))
    }

    /// Configure module to construct new RNG state from system entropy.
    pub fn from_entropy<R>(self) -> MathRandBuilder<math_rand_builder::WithRng<DelayInit<R>>> {
        use math_rand_builder::WithRng;

        MathRandBuilder(WithRng(DelayInit(PhantomData)))
    }

    /// Configure module to construct [`xoshiro256**` RNG](rand_xoshiro::Xoshiro256StarStar) from system entropy.
    pub fn with_xoshiro(
        self,
    ) -> MathRandBuilder<
        math_rand_builder::WithRng<DelayInit<UntraceRng<rand_xoshiro::Xoshiro256StarStar>>>,
    > {
        self.from_entropy()
    }

    /// Configure module to construct [`chach12` RNG](rand_chacha::ChaCha12Rng) from system entropy.
    pub fn with_chacha(
        self,
    ) -> MathRandBuilder<math_rand_builder::WithRng<DelayInit<UntraceRng<rand_chacha::ChaCha12Rng>>>>
    {
        self.from_entropy()
    }

    /// Configure module to construct [default RNG](MathRand#default-configuration) from system entropy.
    pub fn with_default(
        self,
    ) -> MathRandBuilder<
        math_rand_builder::WithRng<DelayInit<impl rand::Rng + rand::SeedableRng + Trace>>,
    > {
        self.with_xoshiro()
    }
}

impl<R> MathRandBuilder<math_rand_builder::WithRng<R>> {
    /// Finish construction of empty module.
    pub fn empty(self) -> MathRand<R, empty::Empty> {
        use empty::Empty;
        use math_rand_builder::WithRng;

        let MathRandBuilder(WithRng(rng_state)) = self;

        MathRand {
            rng_state,
            builder: Empty(()),
        }
    }
}

impl<R> MathRandBuilder<math_rand_builder::WithRng<RootCell<R>>> {
    /// Finish construction by introducing [full set of APIs](MathRand#provided-apis) into module.
    pub fn full<Ty>(self) -> MathRand<RootCell<R>, math_rand::Full> {
        use math_rand::Full;
        use math_rand_builder::WithRng;

        let MathRandBuilder(WithRng(rng_state)) = self;

        MathRand {
            rng_state,
            builder: Full(()),
        }
    }
}

impl<R> MathRandBuilder<math_rand_builder::WithRng<DelayInit<R>>> {
    /// Wrap RNG type in [`UntraceRng`].
    pub fn untrace(self) -> MathRandBuilder<math_rand_builder::WithRng<DelayInit<UntraceRng<R>>>> {
        use math_rand_builder::WithRng;

        MathRandBuilder(WithRng(DelayInit(PhantomData)))
    }

    /// Finish construction by introducing [full set of APIs](MathRand#provided-apis) into module.
    pub fn full(self) -> MathRand<DelayInit<R>, math_rand::Full> {
        use math_rand::Full;
        use math_rand_builder::WithRng;

        let MathRandBuilder(WithRng(rng_state)) = self;

        MathRand {
            rng_state,
            builder: Full(()),
        }
    }
}

/// Sequence manipulation library residing in `table` table.
///
/// This library module will use table under `table` key in parent table or construct a new one otherwise.
/// All included items will be put into the table, potentially overriding existing entries.
///
/// [`TableManip::full`] will construct module introducing all sequence manipulation APIs included into [Lua std's table library][lua#6.6].
/// Read below for full list of provided APIs.
///
/// Alternatively, you can start with [`TableManip::empty`] and manually fill in functions from [`std::table`](crate::std::table) or elsewhere:
///
/// ```
/// # use lua_std::lib::{TableManip, Std};
/// use lua_std::std;
///
/// let global_env = Std::empty()
///     .include(
///         TableManip::empty()
///             .include(std::table::insert)
///             .include(std::table::sort)
///     );
/// ```
///
/// # From Lua documentation
///
/// This library provides generic functions for table manipulation.
/// It provides all its functions inside the table `table`.
///
/// Remember that, whenever an operation needs the length of a table, all caveats about the length operator apply (see §3.4.7).
/// All functions ignore non-numeric keys in the tables given as arguments.
///
/// # Provided APIs
///
/// Stringification:
///
/// * [`concat`](crate::std::table::concat)
///
/// Element manipulation:
///
/// * [`insert`](crate::std::table::insert)
/// * [`remove`](crate::std::table::remove)
/// * [`move`](crate::std::table::move_)
///
/// Packing/unpacking:
///
/// * [`pack`](crate::std::table::pack)
/// * [`unpack`](crate::std::table::unpack)
///
/// Sorting:
///
/// * [`sort`](crate::std::table::sort)
///
/// [lua#6.6]: https://www.lua.org/manual/5.4/manual.html#6.6
pub struct TableManip<P>(P);

impl TableManip<empty::Empty> {
    /// Construct empty module.
    ///
    /// This library module will use table under `math` key in parent table or construct a new one otherwise.
    /// All included items will be put into the table, potentially overriding existing entries.
    ///
    /// Table entries can included using [`include`](Math::include) method.
    pub fn empty() -> TableManip<empty::Empty> {
        use empty::Empty;

        TableManip(Empty(()))
    }

    /// Construct module introducing all sequence manipulation APIs included into [Lua std's table library][lua#6.6].
    ///
    /// This library module will use table under `table` key in parent table or construct a new one otherwise.
    /// All included items will be put into the table, potentially overriding existing entries.
    ///
    /// See [provided APIs](TableManip#provided-apis) for full list.
    ///
    /// [lua#6.6]: https://www.lua.org/manual/5.4/manual.html#6.6
    pub fn full() -> TableManip<table::Full> {
        use table::Full;

        TableManip(Full(()))
    }
}

impl<P> TableManip<P> {
    /// Include API in the module.
    ///
    /// Requires [`T: TableEntry<Ty>`](TableEntry) bound.
    /// It is not spelled out because it leaks `Ty` into signature and in some situations compiler requires type hints to figure this type.
    pub fn include<T>(self, part: T) -> TableManip<(P, T)> {
        let TableManip(p) = self;
        TableManip((p, part))
    }
}

impl<Ty, P> TableEntry<Ty> for TableManip<P>
where
    Ty: Types,
    P: TableEntry<Ty>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let TableManip(builder) = self;
        Table::with_name("table")
            .include(builder)
            .build(table, core);
    }
}

/// Attempt to find a command invocation for default OS shell.
///
/// The purpose of this function is to produce *some* shell to execute commands passed to [`os.execute`](crate::std::os::execute) function.
/// It is provided on best-effort basis and may not work as you intend.
///
/// It is best if you avoid both this function and `os.execute`.
///
/// # Implementation details
///
/// Currently, it will construct the following command:
///
/// * `sh` on Linux which should invoke bash
/// * `cmd` on Windows which should invoke command prompt
/// * `sh` on MacOS which should invoke bash
/// * not configured on other platforms
///
/// It will also inherit host process' working directory.
///
/// However, **it is by no means guaranteed that intended shell will be invoked!**
///
/// Rust's [`Command`] attempts to find an executable file with specified name in platform-specific way.
/// It is entirely possible for it to get resolved to some arbitrary file
/// which have nothing to do with actual shell due to misconfigured system or malicious setup.
///
/// You should not rely on this behavior on untrusted systems.
pub fn default_os_shell() -> Option<Command> {
    #[allow(unused_mut, unused_assignments)]
    let mut result = None;

    #[cfg(target_os = "linux")]
    {
        result = Some(Command::new("sh"));
    }

    #[cfg(target_os = "windows")]
    {
        result = Some(Command::new("cmd"));
    }

    #[cfg(target_os = "macos")]
    {
        result = Some(Command::new("sh"));
    }

    result
}

/// Place `LuaFfi` function into table under a name.
///
/// This is a convenience function which let you easily put functional entries into tables.
pub fn set_func<Ty, F>(name: &str, fn_body: F) -> impl TableEntry<Ty> + use<'_, Ty, F>
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    F: LuaFfi<Ty>,
{
    use rt::ffi::boxed;
    use rt::gc::LuaPtr;
    use rt::runtime::Core;
    use rt::value::{Callable, KeyValue as Key, TableIndex, Value};

    move |table: &RootTable<Ty>, core: &mut Core<Ty>| {
        let key = core.alloc_string(name.into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            Key::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Place string into table under a name.
///
/// This is a convenience function which let you easily put string entries into tables.
pub fn set_str<'k, 'v, Ty>(name: &'k str, value: &'v str) -> impl TableEntry<Ty> + use<'k, 'v, Ty>
where
    Ty: Types,
{
    use rt::gc::LuaPtr;
    use rt::runtime::Core;
    use rt::value::{KeyValue as Key, TableIndex, Value};

    move |table: &RootTable<Ty>, core: &mut Core<Ty>| {
        let key = core.alloc_string(name.into());
        let value = core.alloc_string(value.into());

        core.gc[table].set(
            Key::String(LuaPtr(key.downgrade())),
            Value::String(LuaPtr(value.downgrade())),
        );
    }
}

/// The untrace newtype for random number generators.
///
/// Objects are required to implement [`Trace`] trait in order to be put into our gc heap.
/// This type provides a trivial implementation of `Trace` (so it is analogous to [`Untrace`](gc::Untrace))
/// but additionally implements [`Rng`](rand::Rng) and [`SeedableRng`](rand::SeedableRng) by forwarding implementation to wrapped value.
///
/// This newtype is useful when you want to allocate a PRNG into heap without obscuring traits it is expected to implement.
#[derive(Debug, Clone)]
pub struct UntraceRng<Rng>(pub Rng)
where
    Rng: ?Sized;

impl<Rng> Trace for UntraceRng<Rng>
where
    Rng: ?Sized + 'static,
{
    fn trace(&self, _collector: &mut gc::Collector) {}
}

impl<Rng> rand::RngCore for UntraceRng<Rng>
where
    Rng: rand::RngCore + ?Sized,
{
    fn next_u32(&mut self) -> u32 {
        self.0.next_u32()
    }

    fn next_u64(&mut self) -> u64 {
        self.0.next_u64()
    }

    fn fill_bytes(&mut self, dest: &mut [u8]) {
        self.0.fill_bytes(dest);
    }

    fn try_fill_bytes(&mut self, dest: &mut [u8]) -> Result<(), rand::Error> {
        self.0.try_fill_bytes(dest)
    }
}

impl<Rng> rand::SeedableRng for UntraceRng<Rng>
where
    Rng: rand::SeedableRng,
{
    type Seed = <Rng as rand::SeedableRng>::Seed;

    fn from_seed(seed: Self::Seed) -> Self {
        UntraceRng(Rng::from_seed(seed))
    }

    fn seed_from_u64(state: u64) -> Self {
        UntraceRng(Rng::seed_from_u64(state))
    }

    fn from_rng<R: rand::RngCore>(rng: R) -> Result<Self, rand::Error> {
        Ok(UntraceRng(Rng::from_rng(rng)?))
    }

    fn from_entropy() -> Self {
        UntraceRng(Rng::from_entropy())
    }
}

impl<Rng> rand::CryptoRng for UntraceRng<Rng> where Rng: rand::CryptoRng + ?Sized {}

// Below are typestates for empty/full config for all library modules.
// We use explicit types because if is just too silly to expose unholy incantations generated by `.include`s,
// and type erasing them brings extra type parameters which in some configurations require type annotations to resolve.
// All types below are public but unconstructible, uninteractable and hidden from docs to not pollute doc space.
// We also put `Full`s into separate modules because rustdoc strips private module names leaving only struct name
// (without linking it to anything since it is hidden).
// This results in nice textual representation in generated docs.

mod empty {
    use crate::traits::{TableEntry, TableEntryEx};
    use rt::value::Types;

    #[doc(hidden)]
    pub struct Empty(pub(crate) ());

    impl<Ty> TableEntry<Ty> for Empty
    where
        Ty: Types,
    {
        fn build(self, _table: &crate::traits::RootTable<Ty>, _core: &mut rt::runtime::Core<Ty>) {}
    }

    impl<Ty, Ex> TableEntryEx<Ty, Ex> for Empty
    where
        Ty: Types,
    {
        fn build(
            self,
            _table: &crate::traits::RootTable<Ty>,
            _core: &mut rt::runtime::Core<Ty>,
            _: &mut Ex,
        ) {
        }
    }
}

mod math {
    use crate::traits::TableEntry;
    use rt::ffi::DLuaFfi;
    use rt::value::Types;

    #[doc(hidden)]
    pub struct Full(pub(crate) ());

    impl<Ty> TableEntry<Ty> for Full
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    {
        fn build(self, table: &crate::traits::RootTable<Ty>, core: &mut rt::runtime::Core<Ty>) {
            use crate::std::math;

            math::mininteger.build(table, core);
            math::maxinteger.build(table, core);
            math::huge.build(table, core);
            math::pi.build(table, core);

            math::floor.build(table, core);
            math::ceil.build(table, core);
            math::modf.build(table, core);
            math::tointeger.build(table, core);

            math::type_.build(table, core);
            math::min.build(table, core);
            math::max.build(table, core);
            math::fmod.build(table, core);
            math::ult.build(table, core);

            math::sin.build(table, core);
            math::cos.build(table, core);
            math::tan.build(table, core);
            math::asin.build(table, core);
            math::acos.build(table, core);
            math::atan.build(table, core);
            math::deg.build(table, core);
            math::rad.build(table, core);

            math::abs.build(table, core);
            math::sqrt.build(table, core);
            math::exp.build(table, core);
            math::log.build(table, core);
        }
    }
}

mod math_rand {
    use crate::traits::TableEntryEx;
    use gc::RootCell;
    use rt::ffi::DLuaFfi;
    use rt::value::Types;

    #[doc(hidden)]
    pub struct Full(pub(crate) ());

    impl<Ty, R> TableEntryEx<Ty, RootCell<R>> for Full
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
        R: rand::Rng + rand::SeedableRng + 'static,
    {
        fn build(
            self,
            table: &crate::traits::RootTable<Ty>,
            core: &mut rt::runtime::Core<Ty>,
            rng_state: &mut RootCell<R>,
        ) {
            use crate::std::math;

            math::random.build(table, core, rng_state);
            math::randomseed.build(table, core, rng_state);
        }
    }
}

mod table {
    use crate::traits::TableEntry;
    use rt::ffi::DLuaFfi;
    use rt::value::Types;

    #[doc(hidden)]
    pub struct Full(pub(crate) ());

    impl<Ty> TableEntry<Ty> for Full
    where
        Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
        Ty::String: Unpin,
    {
        fn build(self, table: &crate::traits::RootTable<Ty>, core: &mut rt::runtime::Core<Ty>) {
            use crate::std::table;

            table::concat.build(table, core);

            table::insert.build(table, core);
            table::remove.build(table, core);
            table::move_.build(table, core);

            table::pack.build(table, core);
            table::unpack.build(table, core);

            table::sort.build(table, core);
        }
    }
}
