//! Convert Lua [`Value`] stack from/to Rust arguments.
//!
//! The purpose of traits in this module to provide a way to automatically
//! generate conversion layer between Lua ABI and some Rust functions.
//!
//! Not all Rust functions are can benefit from the mechanism.
//! If you are interested in constructing such functions, read below.
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
//! Terminal parsers:
//! * Any `T` where `T: TryFrom<Value>`.
//!
//!     The parser will attempt to match on single `Value`, failing when conversion fails.
//!
//!     This is your primary way to extend the parser.
//!     You only need to provide [`From`]/[`TryFrom`] implementation in order for your type to be parseable.
//!     Remember to implement [`Error`] for the error type!
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
//! This is possible because runtime can stelthily adjust its own stack via spawning
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
//! * [`NilOr<T>`](super::NilOr) holds `T` or `Nil`.
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
//! Nullable types can be represented with [`NilOr<T>`](super::NilOr).
//!
//! Also you can match on `nil` directly via [`Nil`](crate::value::Nil) Rust type.
//! Note that this is not `()`!
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
//! so there exists a convenient shorthand [`Opts`](super::Opts).
//!
//! `Opts` take single tuple as argument and desugars it into corect chain of `Maybe` and `NilOr`.
//! For example `Opts<(A, B, C)>` will desugar into `Maybe<(NilOr<A>, Maybe<(NilOr<B>, Maybe<NilOr<<C>>)>)>`.
//! Additionally, there is [`Split`](super::Split) trait to conveniently convert this monstrosity
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

use super::Maybe;
use crate::value::{TypeProvider, Value};
use sealed::{BubbleUp, Sealed};

/// Attempt to parse `Args` out of list of values.
///
/// See module level [documentation](self#parsing-arguments) for usage explanation.
pub trait ParseArgs<Values>: Sealed {
    type Error: Error;

    fn parse(self) -> Result<Values, ParseError<Self::Error>>;
}

impl<'a, Value, Values> ParseArgs<Values> for &'a [Value]
where
    Self: ExtractArgs<Values>,
    <<Self as ExtractArgs<Values>>::Error as BubbleUp>::Output: Error,
{
    type Error = <<Self as ExtractArgs<Values>>::Error as BubbleUp>::Output;

    fn parse(self) -> Result<Values, ParseError<Self::Error>> {
        match self.extract() {
            Ok((view, args)) if view.is_empty() => Ok(args),
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
pub trait ExtractArgs<Values>: Sealed + Sized {
    type Error: BubbleUp;

    fn extract(self) -> Result<(Self, Values), Self::Error>;
}

impl<'a, Value> ExtractArgs<()> for &'a [Value] {
    type Error = std::convert::Infallible;

    fn extract(self) -> Result<(Self, ()), Self::Error> {
        Ok((self, ()))
    }
}

impl<'a, Value, A> ExtractArgs<(A,)> for &'a [Value]
where
    Self: ExtractArgs<A>,
{
    type Error = <Self as ExtractArgs<A>>::Error;

    fn extract(self) -> Result<(Self, (A,)), Self::Error> {
        let (view, a) = self.extract()?;
        Ok((view, (a,)))
    }
}

macro_rules! extract_tuple {
    ($error:ident, $($t:ident),*) => {
        impl<'a, Value, $($t),*> ExtractArgs<($($t),*)> for &'a [Value]
        where
            Self: $(ExtractArgs<$t>+)*,
        {
            type Error = $error<$(<Self as ExtractArgs<$t>>::Error),*>;

            fn extract(self) -> Result<(Self, ($($t),*)), Self::Error> {
                let view = self;
                $(
                    #[allow(non_snake_case)]
                    let (view, $t) = view.extract().map_err($error::$t)?;
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

impl<'a, Value, T> ExtractArgs<Maybe<T>> for &'a [Value]
where
    Self: ExtractArgs<T>,
{
    type Error = std::convert::Infallible;

    fn extract(self) -> Result<(Self, Maybe<T>), Self::Error> {
        match self.extract().ok() {
            Some((view, value)) => Ok((view, Maybe::Some(value))),
            None => Ok((self, Maybe::None)),
        }
    }
}

impl<'a, Value, T, const N: usize> ExtractArgs<[T; N]> for &'a [Value]
where
    Self: ExtractArgs<T>,
{
    type Error = <Self as ExtractArgs<T>>::Error;

    fn extract(mut self) -> Result<(Self, [T; N]), Self::Error> {
        // Unfortunately std::array::try_from_fn is still unstable.
        // The following is ugly... but best we can do for now without unsafe.
        let r = std::array::from_fn(|_| {
            self.extract().map(|(view, value)| {
                self = view;
                value
            })
        });

        if r.iter().any(|t| t.is_err()) {
            let Err(err) = self.extract() else {
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

impl<'a, Value, T> ExtractArgs<Vec<T>> for &'a [Value]
where
    Self: ExtractArgs<T>,
{
    type Error = std::convert::Infallible;

    fn extract(mut self) -> Result<(Self, Vec<T>), Self::Error> {
        let r = std::iter::from_fn(|| {
            self.extract().ok().map(|(view, value)| {
                self = view;
                value
            })
        })
        .collect();

        Ok((self, r))
    }
}

impl<'a, Types, T> ExtractArgs<T> for &'a [Value<Types>]
where
    Types: TypeProvider,
    Value<Types>: TryInto<T>,
    <Value<Types> as TryInto<T>>::Error: Error,
{
    type Error = MissingArg<<Value<Types> as TryInto<T>>::Error>;

    fn extract(self) -> Result<(Self, T), Self::Error> {
        let (value, view) = self.split_first().ok_or(MissingArg::Missing)?;
        let value = value.clone().try_into().map_err(|err| MissingArg::Other {
            leftover_args: view.len() + 1,
            err,
        })?;

        Ok((view, value))
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
pub trait FormatReturns<Types>
where
    Types: TypeProvider,
{
    type Iter: Iterator<Item = Value<Types>>;

    fn format(self) -> Self::Iter;
}

impl<Types> FormatReturns<Types> for ()
where
    Types: TypeProvider,
{
    type Iter = std::iter::Empty<Value<Types>>;

    fn format(self) -> Self::Iter {
        std::iter::empty()
    }
}

impl<Types, A> FormatReturns<Types> for (A,)
where
    Types: TypeProvider,
    A: FormatReturns<Types>,
{
    type Iter = <A as FormatReturns<Types>>::Iter;

    fn format(self) -> Self::Iter {
        let (a,) = self;
        a.format()
    }
}

macro_rules! format_tuple {
    ($first:ident, $($t:ident),*) => {
        impl<Types, $first, $($t),*> FormatReturns<Types> for ($first, $($t),*)
        where
        Types: TypeProvider,
            $first: FormatReturns<Types>,
            $($t: FormatReturns<Types>,)*
        {
            type Iter = std::iter::Chain<<$first as FormatReturns<Types>>::Iter, <($($t,)*) as FormatReturns<Types>>::Iter>;

            fn format(self) -> Self::Iter {
                #[allow(non_snake_case)]
                let ($first, $($t,)*) = self;
                $first.format().chain(($($t,)*).format())
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

impl<Types, T> FormatReturns<Types> for Maybe<T>
where
    Types: TypeProvider,
    T: FormatReturns<Types>,
{
    type Iter = std::iter::Flatten<std::option::IntoIter<<T as FormatReturns<Types>>::Iter>>;

    fn format(self) -> Self::Iter {
        self.into_option()
            .map(FormatReturns::format)
            .into_iter()
            .flatten()
    }
}

impl<Types, T, const N: usize> FormatReturns<Types> for [T; N]
where
    Types: TypeProvider,
    T: FormatReturns<Types>,
{
    type Iter = std::iter::Flatten<std::array::IntoIter<<T as FormatReturns<Types>>::Iter, N>>;

    fn format(self) -> Self::Iter {
        self.map(FormatReturns::format).into_iter().flatten()
    }
}

impl<Types, T> FormatReturns<Types> for Vec<T>
where
    Types: TypeProvider,
    T: FormatReturns<Types>,
{
    type Iter = std::iter::FlatMap<
        std::vec::IntoIter<T>,
        <T as FormatReturns<Types>>::Iter,
        fn(T) -> <T as FormatReturns<Types>>::Iter,
    >;

    fn format(self) -> Self::Iter {
        self.into_iter()
            .flat_map(FormatReturns::format as fn(_) -> _)
    }
}

impl<Types, T> FormatReturns<Types> for T
where
    Types: TypeProvider,
    T: Into<Value<Types>>,
{
    type Iter = std::iter::Once<Value<Types>>;

    fn format(self) -> Self::Iter {
        std::iter::once(self.into())
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

    impl<'a, T> Sealed for &'a [T] {}
}
