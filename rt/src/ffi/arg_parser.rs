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

use std::borrow::Cow;
use std::error::Error;
use std::fmt::{Debug, Display};

use gc::index::{Access, GcPtr, Interned, RefAccess, RootPtr};
use gc::userdata::Params;

use super::tuple::Tuple;
use crate::error::{AlreadyDroppedError, AlreadyDroppedOr, NotTextError, RtError};
use crate::gc::{AsGc, AsRoot, Downgrade, Heap, LuaPtr, Upgrade};
use crate::runtime::thread::{StackGuard, TransientStackGuard};
use crate::value::int::NotExactIntError;
use crate::value::string::IntoEncoding;
use crate::value::{Refs, Strong, Type, Types, Value, Weak, WeakValue};
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

pub trait Adapt<Ty, Args, R>
where
    Ty: Types,
{
    fn adapt<F>(&mut self, heap: &mut Heap<Ty>, f: F) -> Result<(), RtError<Ty>>
    where
        F: FnOnce(&mut Heap<Ty>, Args) -> Result<R, RtError<Ty>>;
}

impl<Ty, Args, R> Adapt<Ty, Args, R> for StackGuard<'_, Ty>
where
    Ty: Types,
    for<'a> &'a [WeakValue<Ty>]: ParseArgs<Args, Heap<Ty>>,
    for<'a> <&'a [WeakValue<Ty>] as ParseArgs<Args, Heap<Ty>>>::Error: Display,
    for<'a> TransientStackGuard<'a, Ty>: FormatReturns<Ty, R>,
{
    fn adapt<F>(&mut self, heap: &mut Heap<Ty>, f: F) -> Result<(), RtError<Ty>>
    where
        F: FnOnce(&mut Heap<Ty>, Args) -> Result<R, RtError<Ty>>,
    {
        self.transient_in(heap, |mut stack, heap| {
            let args = stack.parse(heap)?;
            stack.clear();

            let ret = f(heap, args)?;

            stack.format(ret);
            Ok(())
        })
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

pub trait ParseFrom<T: ?Sized>: Sized {
    type Error;

    fn parse(value: &T) -> Result<Self, Self::Error>;
}

impl<Ty, T> ParseAtom<Value<Strong, Ty>, Heap<Ty>> for FromLuaString<T>
where
    Ty: Types,
    T: ParseFrom<Ty::String>,
{
    type Error = StrongConvertError<<T as ParseFrom<Ty::String>>::Error>;

    fn parse_atom(value: Value<Strong, Ty>, extra: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        use crate::gc::LuaPtr;

        let Value::String(LuaPtr(ptr)) = value else {
            let err = TypeMismatchError {
                expected: Type::String,
                found: value.type_(),
            };

            return Err(err.into());
        };

        let value = extra.get_root(&ptr).as_ref();

        ParseFrom::parse(value)
            .map(FromLuaString)
            .map_err(StrongConvertError::Other)
    }
}

impl<Ty, T> ParseAtom<Value<Weak, Ty>, Heap<Ty>> for FromLuaString<T>
where
    Ty: Types,
    T: ParseFrom<Ty::String>,
{
    type Error = WeakConvertError<<T as ParseFrom<Ty::String>>::Error>;

    fn parse_atom(value: Value<Weak, Ty>, extra: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        use crate::gc::LuaPtr;

        let Value::String(LuaPtr(ptr)) = value else {
            let err = TypeMismatchError {
                expected: Type::String,
                found: value.type_(),
            };

            return Err(err.into());
        };

        let value = extra.get(ptr).ok_or(AlreadyDroppedError)?.as_ref();

        ParseFrom::parse(value)
            .map(FromLuaString)
            .map_err(WeakConvertError::Other)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LuaString<T>(pub T);

impl<Ptr> LuaString<Ptr>
where
    Ptr: AsGc,
{
    pub fn fmt_stringless(&self) -> impl Debug + Display + use<Ptr> {
        FmtStringless(self.as_gc())
    }
}

impl<Ptr> LuaString<Ptr>
where
    Ptr: AsGc,
    <Ptr as AsGc>::Access: RefAccess,
    <Ptr as AsGc>::Output: IntoEncoding + Sized + 'static,
{
    pub fn fmt_with<'h, M, P>(
        &self,
        heap: &'h gc::Heap<M, P>,
    ) -> Result<impl Debug + Display + use<'h, Ptr, M, P>, AlreadyDroppedError>
    where
        P: Params,
    {
        use crate::gc::TryGet;

        let string = heap.try_get(self.as_gc())?;
        Ok(FmtWith(string))
    }

    pub fn to_bytes<M, P>(self, heap: &gc::Heap<M, P>) -> Result<Cow<'_, [u8]>, AlreadyDroppedError>
    where
        P: Params,
    {
        use crate::gc::TryGet;

        let value = heap.try_get(self.as_gc())?;
        Ok(value.to_bytes())
    }
}

impl<Ptr> LuaString<Ptr>
where
    Ptr: AsGc<Access = gc::index::Ref>,
    <Ptr as AsGc>::Output: IntoEncoding + Sized + 'static,
{
    pub fn to_str<M, P>(
        self,
        heap: &gc::Heap<M, P>,
    ) -> Result<Cow<'_, str>, AlreadyDroppedOr<NotTextError<<Ptr as AsGc>::Output>>>
    where
        P: Params,
    {
        use crate::gc::TryGet;

        let ptr = self.as_gc();
        let value = heap.try_get(ptr)?;
        value.to_str().ok_or_else(|| match heap.try_upgrade(ptr) {
            Ok(ptr) => AlreadyDroppedOr::Other(NotTextError(ptr)),
            Err(err) => err.into(),
        })
    }
}

impl<Ptr> LuaString<Ptr>
where
    Ptr: AsGc,
    <Ptr as AsGc>::Access: RefAccess,
{
    pub fn as_native<'h, Ty>(
        &self,
        heap: &'h Heap<Ty>,
    ) -> Result<&'h Ty::String, AlreadyDroppedError>
    where
        Ty: Types,
        Ptr: AsGc<Output = Interned<Ty::String>>,
    {
        use crate::gc::TryGet;

        heap.try_get(self.as_gc()).map(|t| t.as_inner())
    }
}

impl<Ptr> AsGc for LuaString<Ptr>
where
    Ptr: AsGc,
{
    type Access = <Ptr as AsGc>::Access;
    type Output = <Ptr as AsGc>::Output;

    fn as_gc(&self) -> GcPtr<Self::Output, Self::Access> {
        self.0.as_gc()
    }
}

impl<Ptr> AsRoot for LuaString<Ptr>
where
    Ptr: AsRoot,
{
    fn as_root(&self) -> &RootPtr<Self::Output, Self::Access> {
        self.0.as_root()
    }
}

impl<T, H> Upgrade<H> for LuaString<T>
where
    T: Upgrade<H>,
{
    type Output = LuaString<<T as Upgrade<H>>::Output>;

    fn try_upgrade(&self, heap: &H) -> Result<Self::Output, AlreadyDroppedError> {
        Ok(LuaString(self.0.try_upgrade(heap)?))
    }
}

impl<T> Downgrade for LuaString<T>
where
    T: Downgrade,
{
    type Output = LuaString<<T as Downgrade>::Output>;

    fn downgrade(&self) -> Self::Output {
        LuaString(self.0.downgrade())
    }
}

struct HexBytes<'a>(&'a [u8]);

impl Debug for HexBytes<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for HexBytes<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02X?}", self.0)
    }
}

pub(crate) struct FmtWith<T>(T);

impl<'a, T> FmtWith<&'a T> {
    // Used directly by `Value`'s  formatting machinery.
    pub(crate) fn new(value: &'a T) -> Self {
        FmtWith(value)
    }
}

impl<T> Debug for FmtWith<&T>
where
    T: IntoEncoding,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("LuaString");

        if let Some(s) = self.0.to_str() {
            f.field("type", &"text");
            f.field("value", &s.as_ref());
        } else {
            let bytes = self.0.to_bytes();
            f.field("type", &"binary");
            f.field("value", &HexBytes(bytes.as_ref()));
        }

        f.finish()
    }
}

impl<T> Display for FmtWith<&T>
where
    T: IntoEncoding,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.0.to_str() {
            if f.alternate() {
                write!(f, "{:?}", s)
            } else {
                write!(f, "{}", s)
            }
        } else {
            let bytes = self.0.to_bytes();
            write!(f, "{}", HexBytes(bytes.as_ref()))
        }
    }
}

struct FmtStringless<T>(T);

impl<T, A> Debug for FmtStringless<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Pointer;

        struct AsPointer<T>(T);

        impl<T> Debug for AsPointer<T>
        where
            T: Pointer,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Pointer::fmt(&self.0, f)
            }
        }

        f.debug_struct("LuaString")
            .field("addr", &AsPointer(self.0))
            .finish()
    }
}

impl<T, A> Display for FmtStringless<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{string <{:p}>}}", self.0)
    }
}

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

#[derive(Debug, Clone, Copy)]
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

impl<T, A> Display for LuaTable<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{table <{:p}>}}", self.0)
    }
}

impl<T, A> Display for LuaTable<LuaPtr<GcPtr<T, A>>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", LuaTable(self.0 .0))
    }
}

impl<T, A> Display for LuaTable<RootPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", LuaTable(self.0.downgrade()))
    }
}

impl<T, A> Display for LuaTable<LuaPtr<RootPtr<T, A>>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", LuaTable(self.0 .0.downgrade()))
    }
}

#[derive(Debug)]
pub struct LuaUserdata<T>(pub T);

impl<Rf, Ty, T> From<LuaUserdata<T>> for Value<Rf, Ty>
where
    Rf: Refs,
    Ty: Types,
    Rf::FullUserdata<Ty::FullUserdata>: From<T>,
{
    fn from(value: LuaUserdata<T>) -> Self {
        let LuaUserdata(value) = value;
        Value::Userdata(value.into())
    }
}

impl<Rf, Ty> TryFrom<Value<Rf, Ty>> for LuaUserdata<Rf::FullUserdata<Ty::FullUserdata>>
where
    Rf: Refs,
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn try_from(value: Value<Rf, Ty>) -> Result<Self, Self::Error> {
        match value {
            Value::Userdata(t) => Ok(LuaUserdata(t)),
            value => {
                let err = TypeMismatchError {
                    found: value.type_(),
                    expected: Type::Userdata,
                };

                Err(err)
            }
        }
    }
}

impl<T, A> Display for LuaUserdata<GcPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{userdata <{:p}>}}", self.0)
    }
}

impl<T, A> Display for LuaUserdata<LuaPtr<GcPtr<T, A>>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", LuaUserdata(self.0 .0))
    }
}

impl<T, A> Display for LuaUserdata<RootPtr<T, A>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", LuaUserdata(self.0.downgrade()))
    }
}

impl<T, A> Display for LuaUserdata<LuaPtr<RootPtr<T, A>>>
where
    T: ?Sized,
    A: Access,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", LuaUserdata(self.0 .0.downgrade()))
    }
}

/// Lua number, which can be either integer or float.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Number {
    pub fn to_float(self) -> Float {
        match self {
            Number::Int(v) => Int(v).into(),
            Number::Float(v) => Float(v),
        }
    }

    pub fn to_int(self) -> Result<Int, NotExactIntError> {
        match self {
            Number::Int(v) => Ok(Int(v)),
            Number::Float(v) => Float(v).try_into(),
        }
    }
}

impl From<Int> for Number {
    fn from(value: Int) -> Self {
        Number::Int(value.0)
    }
}

impl From<Float> for Number {
    fn from(value: Float) -> Self {
        Number::Float(value.0)
    }
}

impl<Ty> TryFrom<WeakValue<Ty>> for Number
where
    Ty: Types,
{
    type Error = NotNumberError;

    fn try_from(value: WeakValue<Ty>) -> Result<Self, Self::Error> {
        match value {
            Value::Int(v) => Ok(Number::Int(v)),
            Value::Float(v) => Ok(Number::Float(v)),
            value => Err(NotNumberError(value.type_())),
        }
    }
}

impl<Ty> From<Number> for WeakValue<Ty>
where
    Ty: Types,
{
    fn from(value: Number) -> Self {
        match value {
            Number::Int(v) => Value::Int(v),
            Number::Float(v) => Value::Float(v),
        }
    }
}

impl<Ty> ParseAtom<WeakValue<Ty>, Heap<Ty>> for Number
where
    Ty: Types,
{
    type Error = NotNumberError;

    fn parse_atom(value: WeakValue<Ty>, _: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NotNumberError(pub Type);

impl Display for NotNumberError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected integer or float, found `{}`", self.0)
    }
}

impl Error for NotNumberError {}

/// Lua integer interpreted as array index.
///
/// Purpose of this type is to assist in transformation of numbers which represent *indices* into some array between Lua and Rust.
///
/// Confusingly for Rust users, Lua indexing start with 1 and not 0.
/// Additionally, Lua permits indexing from the end by using negative indices.
/// This can be a source of errors in FFI code.
///
/// See helper methods on this type to handle transformations.
///
/// Otherwise `Index` is just a special variant of [`Int`]:
/// you can parse it from arguments and convert back to [`Value`]s when necessary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index(pub i64);

impl Index {
    /// Convert from Rust index to Lua index.
    ///
    /// Confusingly for Rust users, Lua indexing start with 1 and not 0.
    /// This can be a source of errors in FFI code.
    ///
    /// This function will convert `usize` (which is what vast majority of Rust indices are) to Lua integer
    /// and will properly offset the value to account for difference between languages.
    ///
    /// # Returns
    ///
    /// `None` will be produced if final index is not representable.
    ///
    /// Under most circumstances this should be impossible.
    /// Rust's std types do not allocate more than `isize::MAX` bytes,
    /// so unless address size on target is wider than 64 bits it should be representable as `i64`.
    ///
    /// The only edge case is passing `isize::MAX` itself as you need to add 1 to it which causes overflow.
    /// This function handles it by returning `None`.
    /// However, if you actually run into this edge case it means
    /// you operate on arrays containing at least 2^63 elements and at this point you have bigger problems.
    pub fn from_index(index: usize) -> Option<Self> {
        index
            .try_into()
            .ok()
            .and_then(|n: i64| n.checked_add(1))
            .map(Index)
    }

    /// Convert from Lua index to Rust index.
    ///
    /// Confusingly for Rust users, Lua indexing start with 1 and not 0.
    /// Additionally, Lua permits indexing from the end by using negative indices.
    /// Both can be a source of errors in FFI code.
    ///
    /// This function will convert Lua integer to `usize` (which is what vast majority of Rust indices are)
    /// and will properly offset the value to account for difference between languages.
    /// It only permits indices from the beginning of sequence, so negative indices will be rejected.
    ///
    /// If you wish to handle indexing from the end (negative indices) or
    /// ensure that resulting index stays in bounds consider using [`to_index`](Self::to_index) or [`to_offset`](Self::to_offset) instead.
    ///
    /// # Returns
    ///
    /// `None` will be produced if
    /// * input is 0, which is an invalid Lua index
    /// * input is negative, which represents indexing from the end of the sequence
    /// * final index is not representable as `usize`
    ///
    /// The last point should only be possible if `i64` is smaller than `usize` on target machine,
    /// e.g. if it has 32-bit addresses.
    pub fn to_index_from_start(self) -> Option<usize> {
        let Index(n) = self;
        match n {
            1.. => {
                let n = n - 1;
                n.try_into().ok()
            }
            _ => None,
        }
    }

    /// Convert from Lua index to Rust index.
    ///
    /// Confusingly for Rust users, Lua indexing start with 1 and not 0.
    /// Additionally, Lua permits indexing from the end by using negative indices.
    /// Both can be a source of errors in FFI code.
    ///
    /// This function will convert Lua integer to `usize` (which is what vast majority of Rust indices are)
    /// and will properly offset the value to account for difference between languages.
    ///
    /// This function is similar to [`to_offset`](Self::to_offset), however it does not permit indices equal to `len`.
    /// This ensures that resulting value is in bounds and safe to use for indexing.
    ///
    /// # Returns
    ///
    /// `None` will be produced if
    /// * input is 0, which is an invalid Lua index
    /// * final index falls outside of `0..len` range
    /// * final index is not representable as `usize`
    ///
    /// The last point should only be possible if `i64` is smaller than `usize` on target machine,
    /// e.g. if it has 32-bit addresses.
    pub fn to_index(self, len: usize) -> Option<usize> {
        self.try_to_index(len).ok()
    }

    /// Convert from Lua index to Rust index clamping it to `0..len` range.
    ///
    /// Confusingly for Rust users, Lua indexing start with 1 and not 0.
    /// Additionally, Lua permits indexing from the end by using negative indices.
    /// Both can be a source of errors in FFI code.
    ///
    /// This function will convert Lua integer to `usize` (which is what vast majority of Rust indices are)
    /// and will properly offset the value to account for difference between languages.
    /// Output will be clamped to `0..len` range.
    ///
    /// This function is similar to [`to_offset_clamp`](Self::to_offset_clamp), however it does not permit indices equal to `len`.
    /// This ensures that resulting value is in bounds and safe to use for indexing.
    ///
    /// # Returns
    ///
    /// `None` will be produced if
    /// * input is 0, which is an invalid Lua index
    /// * `len` is 0, since empty range contains no valid indicies
    /// * final index is not representable as `usize`
    ///
    /// The last point should only be possible if `i64` is smaller than `usize` on target machine,
    /// e.g. if it has 32-bit addresses.
    pub fn to_index_clamp(self, len: usize) -> Option<usize> {
        match self.try_to_index(len) {
            Ok(n) => Some(n),
            Err(IndexError::OutOfBoundsBelow) if !(0..len).is_empty() => Some(0),
            Err(IndexError::OutOfBoundsBelow) => None,
            Err(IndexError::OutOfBoundsAbove) => len.checked_sub(1),
            Err(IndexError::Zero | IndexError::NumericOverflow) => None,
        }
    }

    /// Convert from Lua index to Rust offset.
    ///
    /// Confusingly for Rust users, Lua indexing start with 1 and not 0.
    /// Additionally, Lua permits indexing from the end by using negative indices.
    /// Both can be a source of errors in FFI code.
    ///
    /// This function will convert Lua integer to `usize` (which is what vast majority of Rust indices are)
    /// and will properly offset the value to account for difference between languages.
    ///
    /// This function is similar to [`to_index`](Self::to_index), however it permits indices equal to `len`.
    /// Such indices behave as offsets into range of values and `len` serve as sentinel value denoting end of the range.
    /// This is a common practice including in Rust.
    ///
    /// # Returns
    ///
    /// `None` will be produced if
    /// * input is 0, which is an invalid Lua index
    /// * final index falls outside of `0..=len` range
    /// * final index is not representable as `usize`
    ///
    /// The last point should only be possible if `i64` is smaller than `usize` on target machine,
    /// e.g. if it has 32-bit addresses.
    pub fn to_offset(self, len: usize) -> Option<usize> {
        self.try_to_offset(len).ok()
    }

    /// Convert from Lua index to Rust offset clamping it to `0..=len` range.
    ///
    /// Confusingly for Rust users, Lua indexing start with 1 and not 0.
    /// Additionally, Lua permits indexing from the end by using negative indices.
    /// Both can be a source of errors in FFI code.
    ///
    /// This function will convert Lua integer to `usize` (which is what vast majority of Rust indices are)
    /// and will properly offset the value to account for difference between languages.
    /// Output will be clamped to `0..=len` range.
    ///
    /// This function is similar to [`to_index_clamp`](Self::to_index_clamp), however it permits indices equal to `len`.
    /// Such indices behave as offsets into range of values and `len` serve as sentinel value denoting end of the range.
    /// This is a common practice including in Rust.
    ///
    /// # Returns
    ///
    /// `None` will be produced if
    /// * input is 0, which is an invalid Lua index
    /// * final index is not representable as `usize`
    ///
    /// The last point should only be possible if `i64` is smaller than `usize` on target machine,
    /// e.g. if it has 32-bit addresses.
    pub fn to_offset_clamp(self, len: usize) -> Option<usize> {
        match self.try_to_offset(len) {
            Ok(n) => Some(n),
            Err(IndexError::OutOfBoundsBelow) => Some(0),
            Err(IndexError::OutOfBoundsAbove) => Some(len),
            Err(IndexError::Zero | IndexError::NumericOverflow) => None,
        }
    }

    fn try_map(self, len: usize) -> Result<usize, IndexError> {
        let Index(n) = self;
        match n {
            0 => Err(IndexError::Zero),
            1.. => {
                let n = n - 1;
                let n = usize::try_from(n).map_err(|_| IndexError::NumericOverflow)?;

                Ok(n)
            }
            ..0 => {
                let offset =
                    usize::try_from(n.unsigned_abs()).map_err(|_| IndexError::NumericOverflow)?;

                len.checked_sub(offset).ok_or(IndexError::OutOfBoundsBelow)
            }
        }
    }

    fn try_to_offset(self, len: usize) -> Result<usize, IndexError> {
        match self.try_map(len) {
            Ok(n) => {
                if (0..=len).contains(&n) {
                    Ok(n)
                } else {
                    Err(IndexError::OutOfBoundsAbove)
                }
            }
            t => t,
        }
    }

    fn try_to_index(self, len: usize) -> Result<usize, IndexError> {
        match self.try_map(len) {
            Ok(n) => {
                if (0..len).contains(&n) {
                    Ok(n)
                } else {
                    Err(IndexError::OutOfBoundsAbove)
                }
            }
            t => t,
        }
    }
}

enum IndexError {
    Zero,
    OutOfBoundsBelow,
    OutOfBoundsAbove,
    NumericOverflow,
}

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{}_i64", self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl From<Int> for Index {
    fn from(value: Int) -> Self {
        let Int(value) = value;
        Index(value)
    }
}

impl From<Index> for Int {
    fn from(value: Index) -> Self {
        let Index(value) = value;
        Int(value)
    }
}

impl<Ty> From<Index> for WeakValue<Ty>
where
    Ty: Types,
{
    fn from(value: Index) -> Self {
        let Index(n) = value;
        Value::Int(n)
    }
}

impl<Ty> TryFrom<WeakValue<Ty>> for Index
where
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn try_from(value: WeakValue<Ty>) -> Result<Self, Self::Error> {
        let value: Int = value.try_into()?;
        Ok(value.into())
    }
}

impl<Ty> ParseAtom<WeakValue<Ty>, Heap<Ty>> for Index
where
    Ty: Types,
{
    type Error = TypeMismatchError;

    fn parse_atom(value: WeakValue<Ty>, _: &mut Heap<Ty>) -> Result<Self, Self::Error> {
        let value: Int = value.try_into()?;
        Ok(value.into())
    }
}

use std::ops::{Range, RangeBounds};

/// Convert range of Lua indices to range of Rust indicies.
///
/// Confusingly for Rust users, Lua indexing start with 1 and not 0.
/// Additionally, Lua permits indexing from the end by using negative indices.
/// Both can be a source of errors in FFI code.
///
/// The situation made even worse when index ranges are involved.
/// Lua std is *incredibly* inconsistent in processing index ranges, making emulating correct behavior a challenge.
/// Lua ranges are typically inclusive and have complex rules on resolving edge cases such as when a range should be empty or illegal.
/// This function makes an attempt to accommodate common Lua expectations.
///
/// It is best to avoid this method unless interoperability with Lua std is required.
///
/// Input range can be any Rust's slice expression, `a..b`, `a..=b`, `a..`, `..` etc. containing [`Index`].
///
/// # Transformation process
///
/// 1.  The lower bound is resolved to offset `s` transforming it into inclusive bound.
///     
///     * If `s` is outside of `0..=len`, range is **illegal**.
///
/// 2.  The upper bound is resolved to a *virtual* offset `e` transforming it into exclusive bound.
///     Virtual offset may be outside of `0..=len` range, including being negative.
///
///     * If `s..e` is empty, range is **empty** regardless of value of `e`.
///     * If original end is 0, range is **empty**.
///     * If `e` is outside of `0..=len`, range is **illegal**.
///
/// 3. `s..e` is normalized, so that `s <= e`.
/// 4. `s..e` is the result.
///
/// Remember that 0 is not a valid index!
/// You cannot use it in lower bound position (even if excluded).
/// However, Lua commonly permits it in upper bound position resulting in empty range.
/// This is likely due to the fact that Lua ranges are *inclusive* (e.g. Rust's `start..=end`),
/// which makes it difficult to express empty ranges and index into empty arrays.
///
/// # Returns
///
/// `None` will be produced if
/// * lower bound is 0
/// * transformed lower bound falls outside of `0..=len` range
/// * transformed upper bound is both bigger than transformed lower bounds and `len`
/// * any index is not representable as `usize`
///
/// The last point should only be possible if `i64` is smaller than `usize` on target machine,
/// e.g. if it has 32-bit addresses.
pub fn range_from_lua(range: impl RangeBounds<Index>, len: usize) -> Option<Range<usize>> {
    use std::ops::Bound::*;

    let start = range.start_bound();
    let end = range.end_bound();

    let start = match start {
        Included(i) => i.to_offset(len)?,
        Excluded(i) => i.to_index(len)? + 1,
        Unbounded => 0,
    };

    let end = match end {
        Included(i) => match i.try_to_index(len) {
            Ok(i) => i + 1,
            Err(IndexError::Zero | IndexError::OutOfBoundsBelow) => start,
            Err(IndexError::OutOfBoundsAbove | IndexError::NumericOverflow) => return None,
        },
        Excluded(i) => match i.try_to_offset(len) {
            Ok(i) => i,
            Err(IndexError::Zero | IndexError::OutOfBoundsBelow) => start,
            Err(IndexError::OutOfBoundsAbove | IndexError::NumericOverflow) => return None,
        },
        Unbounded => len,
    };

    // Bring empty range to valid shape.
    let end = end.max(start);

    debug_assert!((0..=len).contains(&start));
    debug_assert!((0..=len).contains(&end));

    Some(start..end)
}

/// Convert range of Lua indices to range of Rust indicies, clamping the bounds.
///
/// Confusingly for Rust users, Lua indexing start with 1 and not 0.
/// Additionally, Lua permits indexing from the end by using negative indices.
/// Both can be a source of errors in FFI code.
///
/// The situation made even worse when index ranges are involved.
/// Lua std is *incredibly* inconsistent in processing index ranges, making emulating correct behavior a challenge.
/// Lua ranges are typically inclusive and have complex rules on resolving edge cases such as when a range should be empty or illegal.
/// This function makes an attempt to accommodate common Lua expectations.
///
/// It is best to avoid this method unless interoperability with Lua std is required.
///
/// Input range can be any Rust's slice expression, `a..b`, `a..=b`, `a..`, `..` etc. containing [`Index`].
///
/// # Transformation process
///
/// 1.  The lower bound is resolved to offset `s` transforming it into inclusive bound.
///     
///     * If `s` is outside of `0..=len`, value is clamped.
///
/// 2.  The upper bound is resolved to a *virtual* offset `e` transforming it into exclusive bound.
///     Virtual offset may be outside of `0..=len` range, including being negative.
///
///     * If `s..e` is empty, range is **empty** regardless of value of `e`.
///     * If original end is 0, range is **empty**.
///     * If `e` is outside of `0..=len`, value is clamped.
///
/// 3. `s..e` is normalized, so that `s <= e`.
/// 4. `s..e` is the result.
///
/// Remember that 0 is not a valid index!
/// You cannot use it in lower bound position (even if excluded).
/// However, Lua commonly permits it in upper bound position resulting in empty range.
/// This is likely due to the fact that Lua ranges are *inclusive* (e.g. Rust's `start..=end`),
/// which makes it difficult to express empty ranges and index into empty arrays.
///
/// # Returns
///
/// `None` will be produced if
/// * starting bound is 0
/// * any index is not representable as `usize`
///
/// The last point should only be possible if `i64` is smaller than `usize` on target machine,
/// e.g. if it has 32-bit addresses.
pub fn range_from_lua_clamp(range: impl RangeBounds<Index>, len: usize) -> Option<Range<usize>> {
    use std::ops::Bound::*;

    let start = range.start_bound();
    let end = range.end_bound();

    let start = match start {
        Included(i) => i.to_offset_clamp(len)?,
        Excluded(i) => i.to_index_clamp(len)? + 1,
        Unbounded => 0,
    };

    let end = match end {
        Included(i) => match i.try_to_index(len) {
            Ok(i) => i + 1,
            Err(IndexError::Zero | IndexError::OutOfBoundsBelow) => start,
            Err(IndexError::OutOfBoundsAbove) => len,
            Err(IndexError::NumericOverflow) => return None,
        },
        Excluded(i) => match i.try_to_offset(len) {
            Ok(i) => i,
            Err(IndexError::Zero | IndexError::OutOfBoundsBelow) => start,
            Err(IndexError::OutOfBoundsAbove) => len,
            Err(IndexError::NumericOverflow) => return None,
        },
        Unbounded => len,
    };

    // Bring empty range to valid shape.
    let end = end.max(start);

    debug_assert!((0..=len).contains(&start));
    debug_assert!((0..=len).contains(&end));

    Some(start..end)
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
