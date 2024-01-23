use std::error::Error;
use std::fmt::{Debug, Display};

use super::{Maybe, NilOr};
use crate::value::Value;

pub(super) trait ExtractArgs<Values> {
    type Error: Error;

    fn extract(self) -> Result<Values, ExtractError<Self::Error>>;
}

impl<'a, Value, Values> ExtractArgs<Values> for &'a [Value]
where
    Self: ParseArgs<Values>,
    <<Self as ParseArgs<Values>>::Error as BubbleUp>::Output: Error,
{
    type Error = <<Self as ParseArgs<Values>>::Error as BubbleUp>::Output;

    fn extract(self) -> Result<Values, ExtractError<Self::Error>> {
        match self.parse() {
            Ok((view, args)) if view.is_empty() => Ok(args),
            Ok((view, _)) => {
                let expected = self.len() - view.len();
                let found = self.len();
                let err = ExtractError::TooManyArgs { found, expected };

                Err(err)
            }
            Err(err) => match err.bubble_up() {
                MissingArg::Missing => {
                    let found = self.len();
                    let err = ExtractError::TooFewArgs { found };

                    Err(err)
                }
                MissingArg::Other { leftover_args, err } => {
                    let index = self.len() - leftover_args;
                    let err = ExtractError::ConversionFailure { index, err };

                    Err(err)
                }
            },
        }
    }
}

#[derive(Debug)]
pub(super) enum ExtractError<E> {
    TooFewArgs { found: usize },
    TooManyArgs { found: usize, expected: usize },
    ConversionFailure { index: usize, err: E },
}

impl<E> Display for ExtractError<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ExtractError::*;

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

impl<E> Error for ExtractError<E> where Self: Debug + Display {}

pub(super) trait ParseArgs<Values>: Sized {
    type Error: BubbleUp;

    fn parse(self) -> Result<(Self, Values), Self::Error>;
}

impl<'a, Value> ParseArgs<()> for &'a [Value] {
    type Error = std::convert::Infallible;

    fn parse(self) -> Result<(Self, ()), Self::Error> {
        Ok((self, ()))
    }
}

impl<'a, Value, A> ParseArgs<(A,)> for &'a [Value]
where
    Self: ParseArgs<A>,
{
    type Error = <Self as ParseArgs<A>>::Error;

    fn parse(self) -> Result<(Self, (A,)), Self::Error> {
        let (view, a) = self.parse()?;
        Ok((view, (a,)))
    }
}

impl<'a, Value, A, B> ParseArgs<(A, B)> for &'a [Value]
where
    Self: ParseArgs<A> + ParseArgs<B>,
{
    type Error = Error2<<Self as ParseArgs<A>>::Error, <Self as ParseArgs<B>>::Error>;

    fn parse(self) -> Result<(Self, (A, B)), Self::Error> {
        let (view, a) = self.parse().map_err(Error2::A)?;
        let (view, b) = view.parse().map_err(Error2::B)?;
        Ok((view, (a, b)))
    }
}

impl<'a, Value, T> ParseArgs<Maybe<T>> for &'a [Value]
where
    Self: ParseArgs<T>,
{
    type Error = std::convert::Infallible;

    fn parse(self) -> Result<(Self, Maybe<T>), Self::Error> {
        match self.parse().ok() {
            Some((view, value)) => Ok((view, Maybe::Some(value))),
            None => Ok((self, Maybe::None)),
        }
    }
}

impl<'a, Value, T, const N: usize> ParseArgs<[T; N]> for &'a [Value]
where
    Self: ParseArgs<T>,
{
    type Error = <Self as ParseArgs<T>>::Error;

    fn parse(mut self) -> Result<(Self, [T; N]), Self::Error> {
        // Unfortunately std::array::try_from_fn is still unstable.
        // The following is ugly... but best we can do for now without unsafe.
        let r = std::array::from_fn(|_| {
            self.parse().map(|(view, value)| {
                self = view;
                value
            })
        });

        if r.iter().any(|t| t.is_err()) {
            let Err(err) = self.parse() else {
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

impl<'a, Value, T> ParseArgs<Vec<T>> for &'a [Value]
where
    Self: ParseArgs<T>,
{
    type Error = std::convert::Infallible;

    fn parse(mut self) -> Result<(Self, Vec<T>), Self::Error> {
        let r = std::iter::from_fn(|| {
            self.parse().ok().map(|(view, value)| {
                self = view;
                value
            })
        })
        .collect();

        Ok((self, r))
    }
}

impl<'a, C, T> ParseArgs<T> for &'a [Value<C>]
where
    T: TryFrom<Value<C>>,
    <T as TryFrom<Value<C>>>::Error: Error,
{
    type Error = MissingArg<<T as TryFrom<Value<C>>>::Error>;

    fn parse(self) -> Result<(Self, T), Self::Error> {
        let (value, view) = self.split_first().ok_or(MissingArg::Missing)?;
        let value = value.clone().try_into().map_err(|err| MissingArg::Other {
            leftover_args: view.len() + 1,
            err,
        })?;

        Ok((view, value))
    }
}

#[derive(Debug)]
pub(super) enum Error2<A, B> {
    A(A),
    B(B),
}

impl<A, B> Display for Error2<A, B>
where
    A: Display,
    B: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error2::A(err) => write!(f, "{err}"),
            Error2::B(err) => write!(f, "{err}"),
        }
    }
}

impl<A, B> Error for Error2<A, B> where Self: Debug + Display {}

#[derive(Debug)]
pub(super) enum MissingArg<E> {
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

pub(super) trait BubbleUp {
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

impl<A, B> BubbleUp for Error2<A, B>
where
    A: BubbleUp,
    B: BubbleUp,
{
    type Output = Error2<<A as BubbleUp>::Output, <B as BubbleUp>::Output>;

    fn bubble_up(self) -> MissingArg<Self::Output> {
        match self {
            Error2::A(err) => err.bubble_up().map(Error2::A),
            Error2::B(err) => err.bubble_up().map(Error2::B),
        }
    }
}

pub(super) trait FormatReturns<C> {
    type Iter: Iterator<Item = Value<C>>;

    fn format(self) -> Self::Iter;
}

impl<Cache> FormatReturns<Cache> for () {
    type Iter = std::iter::Empty<Value<Cache>>;

    fn format(self) -> Self::Iter {
        std::iter::empty()
    }
}

impl<Cache, A> FormatReturns<Cache> for (A,)
where
    A: FormatReturns<Cache>,
{
    type Iter = <A as FormatReturns<Cache>>::Iter;

    fn format(self) -> Self::Iter {
        let (a,) = self;
        a.format()
    }
}

impl<Cache, A, B> FormatReturns<Cache> for (A, B)
where
    A: FormatReturns<Cache>,
    B: FormatReturns<Cache>,
{
    type Iter =
        std::iter::Chain<<A as FormatReturns<Cache>>::Iter, <B as FormatReturns<Cache>>::Iter>;

    fn format(self) -> Self::Iter {
        let (a, b) = self;
        a.format().chain(b.format())
    }
}

impl<Cache, T> FormatReturns<Cache> for Maybe<T>
where
    T: FormatReturns<Cache>,
{
    type Iter = std::iter::Flatten<std::option::IntoIter<<T as FormatReturns<Cache>>::Iter>>;

    fn format(self) -> Self::Iter {
        self.into_option()
            .map(FormatReturns::format)
            .into_iter()
            .flatten()
    }
}

impl<Cache, T, const N: usize> FormatReturns<Cache> for [T; N]
where
    T: FormatReturns<Cache>,
{
    type Iter = std::iter::Flatten<std::array::IntoIter<<T as FormatReturns<Cache>>::Iter, N>>;

    fn format(self) -> Self::Iter {
        self.map(FormatReturns::format).into_iter().flatten()
    }
}

impl<Cache, T> FormatReturns<Cache> for Vec<T>
where
    T: FormatReturns<Cache>,
{
    type Iter = std::iter::FlatMap<
        std::vec::IntoIter<T>,
        <T as FormatReturns<Cache>>::Iter,
        fn(T) -> <T as FormatReturns<Cache>>::Iter,
    >;

    fn format(self) -> Self::Iter {
        self.into_iter()
            .flat_map(FormatReturns::format as fn(_) -> _)
    }
}

impl<Cache, T> FormatReturns<Cache> for T
where
    T: Into<Value<Cache>>,
{
    type Iter = std::iter::Once<Value<Cache>>;

    fn format(self) -> Self::Iter {
        std::iter::once(self.into())
    }
}

pub type Opts<T> = <T as OptsImpl>::Output;

pub(super) trait OptsImpl {
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
