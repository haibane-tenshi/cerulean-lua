use std::error::Error;
use std::fmt::{Debug, Display};

use super::{Maybe, NilOr};
use crate::value::Value;

pub(super) trait ParseArgs<Values>: Sized {
    type Error: Error;

    fn extract(self) -> Result<(Self, Values), Self::Error>;
}

impl<'a, Value> ParseArgs<()> for &'a [Value] {
    type Error = std::convert::Infallible;

    fn extract(self) -> Result<(Self, ()), Self::Error> {
        Ok((self, ()))
    }
}

impl<'a, Value, A> ParseArgs<(A,)> for &'a [Value]
where
    Self: ParseArgs<A>,
{
    type Error = <Self as ParseArgs<A>>::Error;

    fn extract(self) -> Result<(Self, (A,)), Self::Error> {
        let (view, a) = self.extract()?;
        Ok((view, (a,)))
    }
}

impl<'a, Value, A, B> ParseArgs<(A, B)> for &'a [Value]
where
    Self: ParseArgs<A> + ParseArgs<B>,
{
    type Error = Error2<<Self as ParseArgs<A>>::Error, <Self as ParseArgs<B>>::Error>;

    fn extract(self) -> Result<(Self, (A, B)), Self::Error> {
        let (view, a) = self.extract().map_err(Error2::A)?;
        let (view, b) = view.extract().map_err(Error2::B)?;
        Ok((view, (a, b)))
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
            Error2::A(err) => write!(f, "{}", err),
            Error2::B(err) => write!(f, "{}", err),
        }
    }
}

impl<A, B> Error for Error2<A, B> where Self: Debug + Display {}

impl<'a, Value, T> ParseArgs<Maybe<T>> for &'a [Value]
where
    Self: ParseArgs<T>,
{
    type Error = std::convert::Infallible;

    fn extract(self) -> Result<(Self, Maybe<T>), Self::Error> {
        match self.extract().ok() {
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

        let r = r.map(|t| t.unwrap());

        Ok((self, r))
    }
}

impl<'a, Value, T> ParseArgs<Vec<T>> for &'a [Value]
where
    Self: ParseArgs<T>,
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

impl<'a, C, T> ParseArgs<T> for &'a [Value<C>]
where
    T: TryFrom<Value<C>>,
    <T as TryFrom<Value<C>>>::Error: Error,
{
    type Error = MissingArg<<T as TryFrom<Value<C>>>::Error>;

    fn extract(self) -> Result<(Self, T), Self::Error> {
        let (value, view) = self.split_first().ok_or(MissingArg::Missing)?;
        let value = value.clone().try_into().map_err(MissingArg::Other)?;

        Ok((view, value))
    }
}

#[derive(Debug)]
pub(super) enum MissingArg<E> {
    Missing,
    Other(E),
}

impl<E> Display for MissingArg<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MissingArg::Missing => write!(f, "argument is missing"),
            MissingArg::Other(err) => write!(f, "{}", err),
        }
    }
}

impl<E> Error for MissingArg<E> where Self: Debug + Display {}

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
