//! Convert Lua [`Value`] stack from/to Rust arguments.
//!
//! The purpose of traits in this module to provide a way to automatically
//! generate conversion layer between a pile of Lua values and strongly typed Rust.
//!
//! # Parsing arguments
//!
//! Parsing can be done with the help of [`ParseArgs`] trait.
//! The choice of words here is not arbitrary:
//! it is best to think of such conversion as *parsing* list of Lua values left-to-right.
//! The type argument `Args` provided to the trait is used to generate a *parser*
//! which when invoked will try to produce a value of type `Args` or fail trying.
//! Additionally parsing will fail if some of the input is left unconsumed.
//!
//! Parser definitions are provided for the following types.
//!
//! Atom parsers:
//! * Any `T` where `T: ParseAtom<Value, Heap>`.
//!
//!     The parser will attempt to match on single `Value`, failing when conversion fails.
//!     This is your primary way to extend the parser.
//!
//! Combinator parsers:
//! * Tuples `()`, `(T0,)`, ..., `(T0, T1, ..., T11)` of up to 12 elements.
//!
//!     The parser will match on all elements in order, failing if any of them fails.
//!
//! * Arrays of any size `[T; N]`.
//!     
//!     The parser will match `T` exactly `N` times, failing otherwise.
//!
//! * [`Vec<T>`]
//!
//!     The parser will greedily match `T` 0 or more times.
//!     It never fails.
//!
//! * [`Maybe<T>`]
//!
//!     The parser will match `T` 0 or 1 times.
//!     It never fails.
//!
//! Combinator parsers accept any parsers as input including combinator parsers.
//!
//! ## Atom parsers
//!
//! This module includes a number of atom types for common situations.
//!
//! Lua values can be extracted verbatim using
//!
//! * [`Nil`]
//! * [`Boolean`]
//! * [`Int`]
//! * [`Float`]
//! * [`LuaString`]
//! * [`LuaTable`]
//!
//! Those wrappers perform no additional conversions, their purpose is to ensure that you are provided with correct `Value` enum item.
//!
//! Additionally, there is
//!
//! * [`NilOr`] to help represent optional values,
//!     although refer to section about [handling optional arguments](#handling-optional-arguments) for caveats.
//! * [`FromLuaString`] to help construct string-y Rust types.
//!     
//!     `FromLuaString` will attempt to convert underlying value to desired representation.
//!     It is most useful when you intend to pass Lua string to Rust code which expects to find a "proper" string type
//!     such as `String`, `PathBuf` or `OsString`.
//!
//!     Implementation is generic: it can be used to extract any type with suitable `TryInto` impl.
//!     For example you can also attempt to convert to `Vec<u8>` when string is expected to contain binary data.
//!
//!     Unfortunately, generic parsing API is basically incompatible with borrowing so only owned types can be acquired in such way.
//!
//!     You should note that Lua claims to be [encoding-agnostic][lua#2.1] so such conversion may implicate string reencoding.
//!     See discussion about encodings (link - TODO) in documentation of `value::string` module.
//!
//! [lua#2.1]: https://www.lua.org/manual/5.4/manual.html#2.1
//!
//! ## Common parsing situations
//!
//! ### Dealing with function arguments
//!
//! You can use [`Signature`](super::signature::Signature) trait in order
//! to extract function arguments as a tuple in order parse it.
//!
//! ### `Option<T>` cannot be used
//!
//! There are a few reasons why `Option` cannot be used.
//!
//! The biggest issue is an unfortunate interaction within type system.
//!
//! * Canonic way to represent fallible conversion is via `TryFrom` trait.
//!     Since converting from Lua values is fallible (most of the time)
//!     we provide a blanket parser impl for all `T: TryFrom<Value>`.
//! * `TryFrom<T>` have blanket impl for all `From<T>` types, which makes a lot of sense.
//! * `Option<T>` provides `From<T>` impl which also makes a lot of sense: it simply wraps the value in `Some`.
//!
//! However combined we get something that we don't want:
//! `Option<Value>` implements `TryFrom<Value>` and therefore already have a parser.
//! This is highly problematic, an attempt to provide parser impl for `Option<T>`
//! immediately fails due to `Option<Value>` now having multiple impls.
//! All workarounds in this case are rather ugly, but there is more.
//!
//! Another issue is ambiguity in meaning.
//!
//! Lua typically represents optional values as either `nil` or absence of value.
//! This is possible because runtime can stealthily adjust its own stack via spawning
//! extra `nil` values from thin air when needed (and ignore them otherwise).
//!
//! So, what does `None` represent?
//! Absence of value, `nil` or either of the two?
//!
//! Additionally, on Rust side distinction is important.
//! On the one hand runtime specifically promises *not* to adjust stack when passing
//! calls to Rust (since potentially it can be handled more efficiently).
//! On the other hand this argument parser is highly generic and the two cases have *very* different handling.
//!
//! As the result, you must explicitly specify desired representation:
//!
//! * [`NilOr<T>`] holds `T` or `Nil`.
//!
//!     In terms of our parser it matches on a single value.
//!     `nil` will be greedily matched, other values will be matched against `T`.
//! * [`Maybe<T>`] holds `T` or no value.
//!
//!     In terms of our parser it either matches on `T` or, if that fails, on nothing.
//!
//! That said, both types are `Option`-esque and provide API to be converted to more familiar Rust type.
//!
//! ### Handling `nil`
//!
//! Nullable types can be represented with [`NilOr<T>`].
//!
//! Also you can match on `nil` directly via [`Nil`] Rust type.
//! Note that this is different from `()`!
//! `()` is an empty parser that matches on 0 values.
//!
//! ### Handling optional arguments
//!
//! In Lua missing **trailing** optional arguments can be represented as either `nil` or absence of value.
//! On Rust side this matches on `Maybe<NilOr<T>>`.
//! However for multiple optionals it gets a bit more tricky.
//!
//! Naive attempt at matching two optional args
//! `(Maybe<NilOr<A>>, Maybe<NilOr<B>>)` is actually not correct:
//! it can match on nothing, `a`, `b` or `a, b`.
//! Among those `b` is odd one out:
//! normally subsequent args can match only when all preceding args are present.
//!
//! Correct way to write 2-optional parser is `Maybe<(NilOr<A>, Maybe<NilOr<B>>)>`,
//! this way we can proceed to match `B` only if there is value matching `A` or `nil` in its place.
//! As you imagine this gets progressively annoying to type and read as number of args increases,
//! so there exists a convenient shorthand [`Opts`].
//!
//! `Opts` take single tuple as argument and desugars it into corect chain of `Maybe` and `NilOr`.
//! For example `Opts<(A, B, C)>` will desugar into `Maybe<(NilOr<A>, Maybe<(NilOr<B>, Maybe<NilOr<<C>>)>)>`.
//! Additionally, there is [`Split`] trait to conveniently convert this monstrosity
//! into tuple of options.
//!
//! Note that **non-trailing** optional arguments generally don't match on `nil`!
//! You still need to nest `Maybe`s to get correct result,
//! for example correct 2-optional parser is `Maybe<(A, Maybe<B>)>`.
//! There is no shorthand for this: it is an odd case by any standard
//! even if [Lua std makes use of it][lua_ref#6.10].
//!
//! You should be careful when dealing with non-trailing optionals:
//! parsing happens sequentially left-to-right so optional may match on values
//! expected by following required arguments resulting in a failure.
//!
//! [lua_ref#6.10]: https://www.lua.org/manual/5.4/manual.html#6.10
//!
//! # Formatting returns
//!
//! Return values can be formatted using [`FormatReturns`] trait.
//! `FormatReturns::format` converts the value into iterator of `Value`s.
//! Conversion is expected to be infallible:
//! you will need to deal with any conversion errors inside you function.
//!
//! Otherwise it behaves similarly to [`ParseArgs`] but in reverse.
//! The following formatters are provided.
//!
//! Terminal formatters:
//! * For `T` where `T: Into<Value<C>>`
//!
//! Combinator formatters:
//! * Tuples `()`, `(T0,)`, ... `(T0, ..., T11)` up to 12 elements
//! * Arrays of any size `[T; N]`
//! * [`Vec<T>`]
//! * [`Maybe<T>`]
//!
//! Combinators may contain any other parsers including combinators.

use std::error::Error;
use std::fmt::{Debug, Display};

use super::tuple::Tuple;
// use crate::gc::{ConvertInto, Heap, TryConvertInto, TryConvertFrom};
use crate::error::AlreadyDroppedError;
use crate::gc::Heap;
use crate::value::{Refs, Strong, Type, Types, Value, Weak};
use sealed::{BubbleUp, Sealed};

pub use crate::value::{Boolean, Callable, Float, Int, Nil};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Maybe<T> {
    #[default]
    None,
    Some(T),
}

impl<T> Maybe<T> {
    pub fn into_option(self) -> Option<T> {
        self.into()
    }

    pub fn take(&mut self) -> Self {
        std::mem::take(self)
    }
}

impl<T> Maybe<NilOr<T>> {
    pub fn flatten_into_option(self) -> Option<T> {
        self.into_option().and_then(NilOr::into_option)
    }
}

impl<T> From<Option<T>> for Maybe<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => Maybe::Some(t),
            None => Maybe::None,
        }
    }
}

impl<T> From<Maybe<T>> for Option<T> {
    fn from(value: Maybe<T>) -> Self {
        match value {
            Maybe::None => None,
            Maybe::Some(t) => Some(t),
        }
    }
}

/// Attempt to parse `Args` out of list of values.
///
/// See module level [documentation](self#parsing-arguments) for usage explanation.
pub trait ParseArgs<Values, Gc>: Sealed {
    type Error;

    fn parse(self, gc: &mut Gc) -> Result<Values, ParseError<Self::Error>>;
}

impl<Value, Values, Gc> ParseArgs<Values, Gc> for &[Value]
where
    Self: ExtractArgs<Values, Gc>,
    // <<Self as ExtractArgs<Values, Gc>>::Error as BubbleUp>::Output: Error,
{
    type Error = <<Self as ExtractArgs<Values, Gc>>::Error as BubbleUp>::Output;

    fn parse(self, gc: &mut Gc) -> Result<Values, ParseError<Self::Error>> {
        match self.extract(gc) {
            Ok(([], args)) => Ok(args),
            Ok((view, _)) => {
                let expected = self.len() - view.len();
                let found = self.len();
                let err = ParseError::TooManyArgs { found, expected };

                Err(err)
            }
            Err(err) => match err.bubble_up() {
                MissingArg::Missing => {
                    let found = self.len();
                    let err = ParseError::TooFewArgs { found };

                    Err(err)
                }
                MissingArg::Other { leftover_args, err } => {
                    let index = self.len() - leftover_args;
                    let err = ParseError::ConversionFailure { index, err };

                    Err(err)
                }
            },
        }
    }
}

/// The error type for argument parser.
#[derive(Debug)]
pub enum ParseError<E> {
    /// Not enough values are provided to satisfy the parser.
    TooFewArgs { found: usize },

    /// Parser completed, but some values were left unconsumed.
    TooManyArgs { found: usize, expected: usize },

    /// A subparser encountered unexpected value.
    ConversionFailure {
        /// Index of the value which cause the error.
        index: usize,
        err: E,
    },
}

impl<E> Display for ParseError<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseError::*;

        match self {
            TooFewArgs { found } => {
                write!(
                    f,
                    "function recieved {found} args, but not all required args were provided"
                )
            }
            TooManyArgs { found, expected } => {
                write!(
                    f,
                    "function expects {expected} args, but {found} was provided"
                )
            }
            ConversionFailure { index, err } => {
                write!(f, "failed to convert arg[{index}]: {err}")
            }
        }
    }
}

impl<E> Error for ParseError<E> where Self: Debug + Display {}

#[doc(hidden)]
pub trait ExtractArgs<Values, Gc>: Sealed + Sized {
    type Error: BubbleUp;

    fn extract(self, gc: &mut Gc) -> Result<(Self, Values), Self::Error>;
}

impl<Value, Gc> ExtractArgs<(), Gc> for &[Value] {
    type Error = std::convert::Infallible;

    fn extract(self, _gc: &mut Gc) -> Result<(Self, ()), Self::Error> {
        Ok((self, ()))
    }
}

impl<Value, Gc, A> ExtractArgs<(A,), Gc> for &[Value]
where
    Self: ExtractArgs<A, Gc>,
{
    type Error = <Self as ExtractArgs<A, Gc>>::Error;

    fn extract(self, gc: &mut Gc) -> Result<(Self, (A,)), Self::Error> {
        let (view, a) = self.extract(gc)?;
        Ok((view, (a,)))
    }
}

macro_rules! extract_tuple {
    ($error:ident, $($t:ident),*) => {
        impl<'a, Value, Gc, $($t),*> ExtractArgs<($($t),*), Gc> for &'a [Value]
        where
            Self: $(ExtractArgs<$t, Gc>+)*,
        {
            type Error = $error<$(<Self as ExtractArgs<$t, Gc>>::Error),*>;

            fn extract(self, gc: &mut Gc) -> Result<(Self, ($($t),*)), Self::Error> {
                let view = self;
                $(
                    #[allow(non_snake_case)]
                    let (view, $t) = view.extract(gc).map_err($error::$t)?;
                )*
                Ok((view, ($($t),*)))
            }
        }
    };
}

extract_tuple!(Error2, A, B);
extract_tuple!(Error3, A, B, C);
extract_tuple!(Error4, A, B, C, D);
extract_tuple!(Error5, A, B, C, D, E);
extract_tuple!(Error6, A, B, C, D, E, F);
extract_tuple!(Error7, A, B, C, D, E, F, G);
extract_tuple!(Error8, A, B, C, D, E, F, G, H);
extract_tuple!(Error9, A, B, C, D, E, F, G, H, I);
extract_tuple!(Error10, A, B, C, D, E, F, G, H, I, J);
extract_tuple!(Error11, A, B, C, D, E, F, G, H, I, J, K);
extract_tuple!(Error12, A, B, C, D, E, F, G, H, I, J, K, L);

impl<Value, Gc, T> ExtractArgs<Maybe<T>, Gc> for &[Value]
where
    Self: ExtractArgs<T, Gc>,
{
    type Error = std::convert::Infallible;

    fn extract(self, gc: &mut Gc) -> Result<(Self, Maybe<T>), Self::Error> {
        match self.extract(gc).ok() {
            Some((view, value)) => Ok((view, Maybe::Some(value))),
            None => Ok((self, Maybe::None)),
        }
    }
}

impl<Value, Gc, T, const N: usize> ExtractArgs<[T; N], Gc> for &[Value]
where
    Self: ExtractArgs<T, Gc>,
{
    type Error = <Self as ExtractArgs<T, Gc>>::Error;

    fn extract(mut self, gc: &mut Gc) -> Result<(Self, [T; N]), Self::Error> {
        // Unfortunately std::array::try_from_fn is still unstable.
        // The following is ugly... but best we can do for now without unsafe.
        let r = std::array::from_fn(|_| {
            self.extract(gc).map(|(view, value)| {
                self = view;
                value
            })
        });

        if r.iter().any(|t| t.is_err()) {
            let Err(err) = self.extract(gc) else {
                unreachable!()
            };
            return Err(err);
        }

        let r = r.map(|t| {
            let Ok(t) = t else { unreachable!() };

            t
        });

        Ok((self, r))
    }
}

impl<Value, Gc, T> ExtractArgs<Vec<T>, Gc> for &[Value]
where
    Self: ExtractArgs<T, Gc>,
{
    type Error = std::convert::Infallible;

    fn extract(mut self, gc: &mut Gc) -> Result<(Self, Vec<T>), Self::Error> {
        let r = std::iter::from_fn(|| {
            self.extract(gc).ok().map(|(view, value)| {
                self = view;
                value
            })
        })
        .collect();

        Ok((self, r))
    }
}

impl<Rf, Ty, T> ExtractArgs<T, Heap<Ty>> for &[Value<Rf, Ty>]
where
    Rf: Refs,
    Ty: Types,
    Value<Rf, Ty>: Clone,
    T: ParseAtom<Value<Rf, Ty>, Heap<Ty>>,
{
    type Error = MissingArg<<T as ParseAtom<Value<Rf, Ty>, Heap<Ty>>>::Error>;

    fn extract(self, gc: &mut Heap<Ty>) -> Result<(Self, T), Self::Error> {
        let (value, view) = self.split_first().ok_or(MissingArg::Missing)?;
        let value = ParseAtom::parse_atom(value.clone(), gc).map_err(|err| MissingArg::Other {
            leftover_args: view.len() + 1,
            err,
        })?;

        Ok((view, value))
    }
}

/// Convert single value.
pub trait ParseAtom<T, Ex>: Sized {
    type Error;

    fn parse_atom(value: T, extra: &mut Ex) -> Result<Self, Self::Error>;
}

impl<T, Ex> ParseAtom<T, Ex> for T {
    type Error = std::convert::Infallible;

    fn parse_atom(value: T, _: &mut Ex) -> Result<Self, Self::Error> {
        Ok(value)
    }
}

impl<Rf, Ty, Ex> ParseAtom<Value<Rf, Ty>, Ex> for Nil
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn parse_atom(value: Value<Rf, Ty>, _: &mut Ex) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl<Rf, Ty, Ex> ParseAtom<Value<Rf, Ty>, Ex> for Boolean
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn parse_atom(value: Value<Rf, Ty>, _: &mut Ex) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl<Rf, Ty, Ex> ParseAtom<Value<Rf, Ty>, Ex> for Int
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn parse_atom(value: Value<Rf, Ty>, _: &mut Ex) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl<Rf, Ty, Ex> ParseAtom<Value<Rf, Ty>, Ex> for Float
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn parse_atom(value: Value<Rf, Ty>, _: &mut Ex) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl<Rf, Ty, Ex> ParseAtom<Value<Rf, Ty>, Ex> for LuaString<Rf::String<Ty::String>>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn parse_atom(value: Value<Rf, Ty>, _: &mut Ex) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl<Rf, Ty, Ex> ParseAtom<Value<Rf, Ty>, Ex> for LuaTable<Rf::Table<Ty::Table>>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn parse_atom(value: Value<Rf, Ty>, _: &mut Ex) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl<Rf, Ty, Ex> ParseAtom<Value<Rf, Ty>, Ex> for Callable<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn parse_atom(value: Value<Rf, Ty>, _: &mut Ex) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

macro_rules! error_type {
    ($name:ident, $($t:ident),*) => {
        #[derive(Debug)]
        #[doc(hidden)]
        pub enum $name<$($t),*> {
            $($t($t),)*
        }

        impl<$($t),*> Display for $name<$($t),*>
        where
            $($t: Display,)*
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$t(err) => write!(f, "{err}"),)*
                }
            }
        }

        impl<$($t),*> Error for $name<$($t),*>
        where
            Self: Debug + Display
        {}

        impl<$($t),*> BubbleUp for $name<$($t),*>
        where
            $($t: BubbleUp,)*
        {
            type Output = $name<$(<$t as BubbleUp>::Output),*>;

            fn bubble_up(self) -> MissingArg<Self::Output> {
                match self {
                    $(Self::$t(err) => err.bubble_up().map($name::$t),)*
                }
            }
        }
    };
}

error_type!(Error2, A, B);
error_type!(Error3, A, B, C);
error_type!(Error4, A, B, C, D);
error_type!(Error5, A, B, C, D, E);
error_type!(Error6, A, B, C, D, E, F);
error_type!(Error7, A, B, C, D, E, F, G);
error_type!(Error8, A, B, C, D, E, F, G, H);
error_type!(Error9, A, B, C, D, E, F, G, H, I);
error_type!(Error10, A, B, C, D, E, F, G, H, I, J);
error_type!(Error11, A, B, C, D, E, F, G, H, I, J, K);
error_type!(Error12, A, B, C, D, E, F, G, H, I, J, K, L);

#[derive(Debug)]
#[doc(hidden)]
pub enum MissingArg<E> {
    Missing,
    Other { leftover_args: usize, err: E },
}

impl<E> MissingArg<E> {
    fn map<T>(self, f: impl FnOnce(E) -> T) -> MissingArg<T> {
        match self {
            MissingArg::Missing => MissingArg::Missing,
            MissingArg::Other { leftover_args, err } => MissingArg::Other {
                leftover_args,
                err: f(err),
            },
        }
    }
}

/// Render type into iterator of Lua [`Value`]s.
///
/// See module-level [documentation](self#formatting-returns) for usage explanation.
pub trait FormatReturns<Ty, R> {
    fn format(&mut self, value: R);
}

impl<Ty, T> FormatReturns<Ty, ()> for T {
    fn format(&mut self, (): ()) {}
}

impl<Ty, T, A> FormatReturns<Ty, (A,)> for T
where
    T: FormatReturns<Ty, A>,
{
    fn format(&mut self, value: (A,)) {
        let (a,) = value;
        self.format(a);
    }
}

macro_rules! format_tuple {
    ($first:ident, $($t:ident),*) => {
        impl<Ty, T, $first, $($t),*> FormatReturns<Ty, ($first, $($t),*)> for T
        where
            T: FormatReturns<Ty, $first>,
            $(T: FormatReturns<Ty, $t>,)*
        {
            fn format(&mut self, value: ($first, $($t),*)) {
                #[allow(non_snake_case)]
                let ($first, $($t,)*) = value;
                self.format($first);
                $(self.format($t);)*
            }
        }
    };
}

format_tuple!(A, B);
format_tuple!(A, B, C);
format_tuple!(A, B, C, D);
format_tuple!(A, B, C, D, E);
format_tuple!(A, B, C, D, E, F);
format_tuple!(A, B, C, D, E, F, G);
format_tuple!(A, B, C, D, E, F, G, H);
format_tuple!(A, B, C, D, E, F, G, H, I);
format_tuple!(A, B, C, D, E, F, G, H, I, J);
format_tuple!(A, B, C, D, E, F, G, H, I, J, K);
format_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);

impl<Ty, T, R> FormatReturns<Ty, Maybe<R>> for T
where
    T: FormatReturns<Ty, R>,
{
    fn format(&mut self, value: Maybe<R>) {
        if let Some(value) = value.into_option() {
            self.format(value);
        }
    }
}

impl<Ty, T, R, const N: usize> FormatReturns<Ty, [R; N]> for T
where
    T: FormatReturns<Ty, R>,
{
    fn format(&mut self, values: [R; N]) {
        for value in values {
            self.format(value);
        }
    }
}

impl<Ty, T, R> FormatReturns<Ty, Vec<R>> for T
where
    T: FormatReturns<Ty, R>,
{
    fn format(&mut self, values: Vec<R>) {
        for value in values {
            self.format(value);
        }
    }
}

impl<Ty, T, R> FormatReturns<Ty, R> for T
where
    Ty: Types,
    T: Extend<Value<Weak, Ty>>,
    R: Into<Value<Weak, Ty>>,
{
    fn format(&mut self, value: R) {
        let value = value.into();
        self.extend([value]);
    }
}

mod sealed {
    use super::MissingArg;

    pub trait BubbleUp {
        type Output;

        fn bubble_up(self) -> MissingArg<Self::Output>;
    }

    impl BubbleUp for std::convert::Infallible {
        type Output = Self;

        fn bubble_up(self) -> MissingArg<Self::Output> {
            match self {}
        }
    }

    impl<E> BubbleUp for MissingArg<E> {
        type Output = E;

        fn bubble_up(self) -> MissingArg<Self::Output> {
            self
        }
    }

    pub trait Sealed {}

    impl<T> Sealed for &[T] {}
}

/// Split [`Opts<T>`] into tuple of options.
///
/// Trait provides a convenience method to convert Lua optional arguments
/// `Opts<(T0, T1,...)>` into more convenient Rustic form of
/// `(Option<T0>, Option<T1>,...)`.
pub trait Split: sealed_opts::Sealed {
    type Output;

    /// Convert [`Opts<(T0, T1,...)>`](Opts) into `(Option<T0>, Option<T1>,...)`.
    fn split(self) -> Self::Output;
}

impl Split for Opts<()> {
    type Output = ();

    fn split(self) -> Self::Output {}
}

impl<A> Split for Maybe<NilOr<A>> {
    type Output = (Option<A>,);

    fn split(self) -> Self::Output {
        (self.flatten_into_option(),)
    }
}

impl<A, B> Split for Maybe<(NilOr<A>, Maybe<NilOr<B>>)> {
    type Output = (Option<A>, Option<B>);

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b,) = rest.split();
                (a.into_option(), b)
            }
        }
    }
}

impl<A, B, C> Split for Maybe<(NilOr<A>, Maybe<(NilOr<B>, Maybe<NilOr<C>>)>)> {
    type Output = (Option<A>, Option<B>, Option<C>);

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c) = rest.split();
                (a.into_option(), b, c)
            }
        }
    }
}

impl<A, B, C, D> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(NilOr<B>, Maybe<(NilOr<C>, Maybe<NilOr<D>>)>)>,
    )>
{
    type Output = (Option<A>, Option<B>, Option<C>, Option<D>);

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d) = rest.split();
                (a.into_option(), b, c, d)
            }
        }
    }
}

impl<A, B, C, D, E> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(NilOr<C>, Maybe<(NilOr<D>, Maybe<NilOr<E>>)>)>,
        )>,
    )>
{
    type Output = (Option<A>, Option<B>, Option<C>, Option<D>, Option<E>);

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e) = rest.split();
                (a.into_option(), b, c, d, e)
            }
        }
    }
}

impl<A, B, C, D, E, F> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(NilOr<D>, Maybe<(NilOr<E>, Maybe<NilOr<F>>)>)>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f) = rest.split();
                (a.into_option(), b, c, d, e, f)
            }
        }
    }
}

impl<A, B, C, D, E, F, G> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(NilOr<E>, Maybe<(NilOr<F>, Maybe<NilOr<G>>)>)>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g) = rest.split();
                (a.into_option(), b, c, d, e, f, g)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(NilOr<F>, Maybe<(NilOr<G>, Maybe<NilOr<H>>)>)>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H, I> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(
                            NilOr<F>,
                            Maybe<(NilOr<G>, Maybe<(NilOr<H>, Maybe<NilOr<I>>)>)>,
                        )>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
        Option<I>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h, i) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h, i)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H, I, J> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(
                            NilOr<F>,
                            Maybe<(
                                NilOr<G>,
                                Maybe<(NilOr<H>, Maybe<(NilOr<I>, Maybe<NilOr<J>>)>)>,
                            )>,
                        )>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
        Option<I>,
        Option<J>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h, i, j) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h, i, j)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(
                            NilOr<F>,
                            Maybe<(
                                NilOr<G>,
                                Maybe<(
                                    NilOr<H>,
                                    Maybe<(NilOr<I>, Maybe<(NilOr<J>, Maybe<NilOr<K>>)>)>,
                                )>,
                            )>,
                        )>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
        Option<I>,
        Option<J>,
        Option<K>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h, i, j, k) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h, i, j, k)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K, L> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(
                            NilOr<F>,
                            Maybe<(
                                NilOr<G>,
                                Maybe<(
                                    NilOr<H>,
                                    Maybe<(
                                        NilOr<I>,
                                        Maybe<(NilOr<J>, Maybe<(NilOr<K>, Maybe<NilOr<L>>)>)>,
                                    )>,
                                )>,
                            )>,
                        )>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
        Option<I>,
        Option<J>,
        Option<K>,
        Option<L>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h, i, j, k, l) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h, i, j, k, l)
            }
        }
    }
}

mod sealed_opts {
    use super::{Maybe, NilOr};

    pub trait Sealed {}

    impl Sealed for () {}

    impl<T> Sealed for Maybe<NilOr<T>> {}

    impl<T, U> Sealed for Maybe<(NilOr<T>, U)> where U: Sealed {}
}

/// Expand tuple into optional Lua arguments.
///
/// This typedef provides a shorthand for constructing
/// [correct parser](self#handling-optional-arguments) for a sequence of optional Lua arguments.
///
/// For example, the following expands into
/// * `Opts<()>` -> `()`
/// * `Opts<(A,)>` -> `Maybe<NilOr<A>>`
/// * `Opts<(A, B)>` -> `Maybe<(NilOr<A>, Maybe<NilOr<B>>)>`
/// * `Opts<(A, B, C)>` -> `Maybe<(NilOr<A>, Maybe<(NilOr<B>, Maybe<NilOr<C>>)>)>`
/// * etc.
///
/// See [`Split`] trait to quickly expand those into tuple of `Option`s.
pub type Opts<T> = <T as OptsImpl>::Output;

#[doc(hidden)]
pub trait OptsImpl: Tuple {
    type Output;
}

impl OptsImpl for () {
    type Output = ();
}

impl<A> OptsImpl for (A,) {
    type Output = Maybe<NilOr<A>>;
}

impl<A, B> OptsImpl for (A, B) {
    type Output = Maybe<(NilOr<A>, Opts<(B,)>)>;
}

impl<A, B, C> OptsImpl for (A, B, C) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C)>)>;
}

impl<A, B, C, D> OptsImpl for (A, B, C, D) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D)>)>;
}

impl<A, B, C, D, E> OptsImpl for (A, B, C, D, E) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E)>)>;
}

impl<A, B, C, D, E, F> OptsImpl for (A, B, C, D, E, F) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F)>)>;
}

impl<A, B, C, D, E, F, G> OptsImpl for (A, B, C, D, E, F, G) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G)>)>;
}

impl<A, B, C, D, E, F, G, H> OptsImpl for (A, B, C, D, E, F, G, H) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H)>)>;
}

impl<A, B, C, D, E, F, G, H, I> OptsImpl for (A, B, C, D, E, F, G, H, I) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H, I)>)>;
}

impl<A, B, C, D, E, F, G, H, I, J> OptsImpl for (A, B, C, D, E, F, G, H, I, J) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H, I, J)>)>;
}

impl<A, B, C, D, E, F, G, H, I, J, K> OptsImpl for (A, B, C, D, E, F, G, H, I, J, K) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H, I, J, K)>)>;
}

impl<A, B, C, D, E, F, G, H, I, J, K, L> OptsImpl for (A, B, C, D, E, F, G, H, I, J, K, L) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H, I, J, K, L)>)>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum NilOr<T> {
    #[default]
    Nil,
    Some(T),
}

impl<T> NilOr<T> {
    pub fn into_option(self) -> Option<T> {
        self.into()
    }
}

impl<T> From<Option<T>> for NilOr<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => NilOr::Some(t),
            None => NilOr::Nil,
        }
    }
}

impl<T> From<NilOr<T>> for Option<T> {
    fn from(value: NilOr<T>) -> Self {
        match value {
            NilOr::Nil => None,
            NilOr::Some(t) => Some(t),
        }
    }
}

impl<T> From<Nil> for NilOr<T> {
    fn from(Nil: Nil) -> Self {
        NilOr::Nil
    }
}

impl<T, Rf, Ty> From<NilOr<T>> for Value<Rf, Ty>
where
    T: Into<Value<Rf, Ty>>,
    Rf: Refs,
    Ty: Types,
{
    fn from(value: NilOr<T>) -> Self {
        match value {
            NilOr::Nil => Value::Nil,
            NilOr::Some(value) => value.into(),
        }
    }
}

impl<T, Rf, Ty> TryFrom<Value<Rf, Ty>> for NilOr<T>
where
    Rf: Refs,
    Ty: Types,
    Value<Rf, Ty>: TryInto<T>,
{
    type Error = <Value<Rf, Ty> as TryInto<T>>::Error;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
        match value {
            Value::Nil => Ok(NilOr::Nil),
            value => value.try_into().map(NilOr::Some),
        }
    }
}

impl<Rf, Ty, Ex, T> ParseAtom<Value<Rf, Ty>, Ex> for NilOr<T>
where
    Rf: Refs,
    Ty: Types,
    T: ParseAtom<Value<Rf, Ty>, Ex>,
{
    type Error = <T as ParseAtom<Value<Rf, Ty>, Ex>>::Error;

    fn parse_atom(value: Value<Rf, Ty>, extra: &mut Ex) -> Result<Self, Self::Error> {
        match value {
            Value::Nil => Ok(NilOr::Nil),
            value => ParseAtom::parse_atom(value, extra).map(NilOr::Some),
        }
    }
}

pub struct FromLuaString<T>(pub T);

impl<Ty, T> ParseAtom<Value<Strong, Ty>, Heap<Ty>> for FromLuaString<T>
where
    Ty: Types,
    Ty::String: TryInto<T>,
{
    type Error = StrongConvertError<<Ty::String as TryInto<T>>::Error>;

    fn parse_atom(value: Value<Strong, Ty>, extra: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        use crate::gc::LuaPtr;

        let Value::String(LuaPtr(ptr)) = value else {
            let err = TypeMismatchError {
                expected: Type::String,
                found: value.type_(),
            };

            return Err(err.into());
        };

        extra
            .get_root(ptr)
            .as_ref()
            .clone()
            .try_into()
            .map(FromLuaString)
            .map_err(StrongConvertError::Other)
    }
}

impl<Ty, T> ParseAtom<Value<Weak, Ty>, Heap<Ty>> for FromLuaString<T>
where
    Ty: Types,
    Ty::String: TryInto<T>,
{
    type Error = WeakConvertError<<Ty::String as TryInto<T>>::Error>;

    fn parse_atom(value: Value<Weak, Ty>, extra: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        use crate::gc::LuaPtr;

        let Value::String(LuaPtr(ptr)) = value else {
            let err = TypeMismatchError {
                expected: Type::String,
                found: value.type_(),
            };

            return Err(err.into());
        };

        extra
            .get(ptr)
            .ok_or(AlreadyDroppedError)?
            .as_ref()
            .clone()
            .try_into()
            .map(FromLuaString)
            .map_err(WeakConvertError::Other)
    }
}

pub struct LuaString<T>(pub T);

impl<T, Rf, Ty> From<LuaString<T>> for Value<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    T: Into<Rf::String<Ty::String>>,
{
    fn from(value: LuaString<T>) -> Self {
        let LuaString(value) = value;

        Value::String(value.into())
    }
}

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for LuaString<Rf::String<Ty::String>>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
        match value {
            Value::String(value) => Ok(LuaString(value)),
            value => {
                let err = TypeMismatchError {
                    expected: Type::String,
                    found: value.type_(),
                };

                Err(err)
            }
        }
    }
}

pub struct LuaTable<T>(pub T);

impl<Rf, Ty, T> From<LuaTable<T>> for Value<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::Table<Ty::Table>: From<T>,
{
    fn from(value: LuaTable<T>) -> Self {
        let LuaTable(value) = value;
        Value::Table(value.into())
    }
}

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for LuaTable<Rf::Table<Ty::Table>>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
        match value {
            Value::Table(t) => Ok(LuaTable(t)),
            value => {
                let err = TypeMismatchError {
                    found: value.type_(),
                    expected: Type::Table,
                };

                Err(err)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeMismatchError {
    pub found: Type,
    pub expected: Type,
}

impl Display for TypeMismatchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TypeMismatchError { found, expected } = self;

        write!(f, "expected value of type `{expected}`, found `{found}`")
    }
}

impl Error for TypeMismatchError {}

#[derive(Debug, Clone, Copy)]
pub enum StrongConvertError<E> {
    TypeMismatch(TypeMismatchError),
    Other(E),
}

impl<E> From<TypeMismatchError> for StrongConvertError<E> {
    fn from(value: TypeMismatchError) -> Self {
        StrongConvertError::TypeMismatch(value)
    }
}

impl<E> Display for StrongConvertError<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StrongConvertError::TypeMismatch(err) => write!(f, "{err}"),
            StrongConvertError::Other(err) => write!(f, "{err}"),
        }
    }
}

impl<E> Error for StrongConvertError<E> where Self: Debug + Display {}

#[derive(Debug, Clone, Copy)]
pub enum WeakConvertError<E> {
    TypeMismatch(TypeMismatchError),
    AlreadyDropped(AlreadyDroppedError),
    Other(E),
}

impl<E> From<TypeMismatchError> for WeakConvertError<E> {
    fn from(value: TypeMismatchError) -> Self {
        WeakConvertError::TypeMismatch(value)
    }
}

impl<E> From<AlreadyDroppedError> for WeakConvertError<E> {
    fn from(value: AlreadyDroppedError) -> Self {
        WeakConvertError::AlreadyDropped(value)
    }
}

impl<E> Display for WeakConvertError<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WeakConvertError::TypeMismatch(err) => write!(f, "{err}"),
            WeakConvertError::AlreadyDropped(err) => write!(f, "{err}"),
            WeakConvertError::Other(err) => write!(f, "{err}"),
        }
    }
}

impl<E> Error for WeakConvertError<E> where Self: Debug + Display {}
