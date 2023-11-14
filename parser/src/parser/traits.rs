#![allow(dead_code)]

use std::ops::Range;

use super::error::{Arrow, FailureMode, WithMode};

type ArrowT<T, U> = <T as Arrow<U>>::Output;

pub trait ParseOnce<Source>: Sized {
    type Output;
    type Success;
    type Failure;
    type FailFast;

    fn parse_once(
        self,
        _: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>;

    fn as_ref(&self) -> AsRef<&Self> {
        AsRef { parser: self }
    }

    fn as_mut(&mut self) -> AsRef<&mut Self> {
        AsRef { parser: self }
    }

    fn optional(self) -> Optional<Self> {
        Optional { parser: self }
    }

    fn map_output<F>(self, f: F) -> MapOutput<Self, F> {
        MapOutput { parser: self, f }
    }

    fn map_success<F>(self, f: F) -> MapSuccess<Self, F> {
        MapSuccess { parser: self, f }
    }

    fn map_failure<F>(self, f: F) -> MapFailure<Self, F> {
        MapFailure { parser: self, f }
    }
}

impl<F, Source, Output, Success, Failure, FailFast> ParseOnce<Source> for F
where
    F: FnOnce(Source) -> Result<ParsingState<Source, (), Output, Success, Failure>, FailFast>,
{
    type Output = Output;
    type Success = Success;
    type Failure = Failure;
    type FailFast = FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        (self)(s)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AsRef<P> {
    parser: P,
}

impl<P, Source> ParseOnce<Source> for AsRef<&P>
where
    P: Parse<Source>,
{
    type Output = P::Output;
    type Success = P::Success;
    type Failure = P::Failure;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parser.parse(s)
    }
}

impl<P, Source> ParseMut<Source> for AsRef<&P>
where
    P: Parse<Source>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parser.parse(s)
    }
}

impl<P, Source> Parse<Source> for AsRef<&P>
where
    P: Parse<Source>,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parser.parse(s)
    }
}

impl<P, Source> ParseOnce<Source> for AsRef<&mut P>
where
    P: ParseMut<Source>,
{
    type Output = P::Output;
    type Success = P::Success;
    type Failure = P::Failure;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parser.parse_mut(s)
    }
}

impl<P, Source> ParseMut<Source> for AsRef<&mut P>
where
    P: ParseMut<Source>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parser.parse_mut(s)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Optional<P> {
    parser: P,
}

impl<Source, P> ParseOnce<Source> for Optional<P>
where
    P: ParseOnce<Source>,
    Source: Clone,
    P::Failure: Into<P::Success>,
{
    type Output = Option<P::Output>;
    type Success = P::Success;
    type Failure = std::convert::Infallible;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        let r = match self.parser.parse_once(s.clone())? {
            ParsingState::Success(s, output, reason) => {
                ParsingState::Success(s, Some(output), reason)
            }
            ParsingState::Failure(_, failure) => ParsingState::Success(s, None, failure.into()),
        };

        Ok(r)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MapOutput<P, F> {
    parser: P,
    f: F,
}

impl<Source, P, F, T> ParseOnce<Source> for MapOutput<P, F>
where
    P: ParseOnce<Source>,
    F: FnOnce(P::Output) -> T,
{
    type Output = T;
    type Success = P::Success;
    type Failure = P::Failure;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        Ok(self.parser.parse_once(s)?.map_output(self.f))
    }
}

impl<Source, P, F, T> ParseMut<Source> for MapOutput<P, F>
where
    P: ParseMut<Source>,
    F: FnMut(P::Output) -> T,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        Ok(self.parser.parse_mut(s)?.map_output(&mut self.f))
    }
}

impl<Source, P, F, T> Parse<Source> for MapOutput<P, F>
where
    P: Parse<Source>,
    F: Fn(P::Output) -> T,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        Ok(self.parser.parse(s)?.map_output(&self.f))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MapSuccess<P, F> {
    parser: P,
    f: F,
}

impl<Source, P, F, T> ParseOnce<Source> for MapSuccess<P, F>
where
    P: ParseOnce<Source>,
    F: FnOnce(P::Success) -> T,
{
    type Output = P::Output;
    type Success = T;
    type Failure = P::Failure;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        Ok(self.parser.parse_once(s)?.map_success(self.f))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MapFailure<P, F> {
    parser: P,
    f: F,
}

impl<Source, P, F, T> ParseOnce<Source> for MapFailure<P, F>
where
    P: ParseOnce<Source>,
    F: FnOnce(P::Failure) -> T,
{
    type Output = P::Output;
    type Success = P::Success;
    type Failure = T;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        Ok(self.parser.parse_once(s)?.map_failure(self.f))
    }
}

impl<Source, P, F, T> ParseMut<Source> for MapFailure<P, F>
where
    P: ParseMut<Source>,
    F: FnMut(P::Failure) -> T,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        Ok(self.parser.parse_mut(s)?.map_failure(&mut self.f))
    }
}

impl<Source, P, F, T> Parse<Source> for MapFailure<P, F>
where
    P: Parse<Source>,
    F: Fn(P::Failure) -> T,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        Ok(self.parser.parse(s)?.map_failure(&self.f))
    }
}

pub trait ParseMut<Source>: ParseOnce<Source> {
    fn parse_mut(
        &mut self,
        _: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>;

    fn repeat_with<F>(self, fold: F) -> RepeatWith<Self, F> {
        RepeatWith { parser: self, fold }
    }

    fn repeat(self) -> Repeat<Self> {
        Repeat { parser: self }
    }
}

impl<F, Source, Output, Success, Failure, FailFast> ParseMut<Source> for F
where
    F: FnMut(Source) -> Result<ParsingState<Source, (), Output, Success, Failure>, FailFast>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        (self)(s)
    }
}

pub struct RepeatWith<P, F> {
    parser: P,
    fold: F,
}

impl<P, F, Source> ParseOnce<Source> for RepeatWith<P, F>
where
    Source: Clone,
    P: ParseMut<Source>,
    P::Success: Arrow<P::Success, Output = P::Success> + Arrow<P::Failure>,
    F: FnMut(P::Output, P::Output) -> P::Output,
{
    type Output = P::Output;
    type Success = ArrowT<P::Success, P::Failure>;
    type Failure = P::Failure;
    type FailFast = P::FailFast;

    fn parse_once(
        mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parse_mut(s)
    }
}

impl<P, F, Source> ParseMut<Source> for RepeatWith<P, F>
where
    Source: Clone,
    P: ParseMut<Source>,
    P::Success: Arrow<P::Success, Output = P::Success> + Arrow<P::Failure>,
    F: FnMut(P::Output, P::Output) -> P::Output,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        let (mut s, mut output, mut success) = match self.parser.parse_mut(s.clone())? {
            ParsingState::Success(s, output, reason) => (s, output, reason),
            ParsingState::Failure(s, failure) => return Ok(ParsingState::Failure(s, failure)),
        };

        let reason = loop {
            match self.parser.parse_mut(s.clone())? {
                ParsingState::Success(ns, noutput, nreason) => {
                    s = ns;
                    success = success.arrow(nreason);
                    output = (self.fold)(output, noutput);
                }
                ParsingState::Failure(_, failure) => break success.arrow(failure),
            }
        };

        Ok(ParsingState::Success(s, output, reason))
    }
}

pub struct Repeat<P> {
    parser: P,
}

impl<P, Source> ParseOnce<Source> for Repeat<P>
where
    Source: Clone,
    P: ParseMut<Source>,
    P::Success: Arrow<P::Success, Output = P::Success> + Arrow<P::Failure>,
{
    type Output = ();
    type Success = ArrowT<P::Success, P::Failure>;
    type Failure = P::Failure;
    type FailFast = P::FailFast;

    fn parse_once(
        mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parse_mut(s)
    }
}

impl<P, Source> ParseMut<Source> for Repeat<P>
where
    Source: Clone,
    P: ParseMut<Source>,
    P::Success: Arrow<P::Success, Output = P::Success> + Arrow<P::Failure>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parser
            .as_mut()
            .map_output(|_| ())
            .repeat_with(|_, _| ())
            .parse_mut(s)
    }
}

pub trait Parse<Source>: ParseMut<Source> {
    fn parse(
        &self,
        _: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>;
}

impl<F, Source, Output, Success, Failure, FailFast> Parse<Source> for F
where
    F: Fn(Source) -> Result<ParsingState<Source, (), Output, Success, Failure>, FailFast>,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<ParsingState<Source, (), Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        (self)(s)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Source<S>(pub S);

impl<S> Source<S> {
    pub fn and<P>(
        self,
        p: P,
    ) -> Result<
        ParsingState<
            S,
            (),
            <P as ParseOnce<S>>::Output,
            <P as ParseOnce<S>>::Success,
            <P as ParseOnce<S>>::Failure,
        >,
        <P as ParseOnce<S>>::FailFast,
    >
    where
        P: ParseOnce<S>,
    {
        let state = p.parse_once(self.0)?.map_fsource(|_| ());
        Ok(state)
    }

    pub fn or<P>(
        self,
        p: P,
    ) -> Result<ParsingState<S, S, P::Output, P::Success, P::Failure>, P::FailFast>
    where
        S: Clone,
        P: ParseOnce<S>,
    {
        let state = p.parse_once(self.0.clone())?.map_fsource(|_| self.0);
        Ok(state)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParsingState<Source, FSource, Output, Success, Failure> {
    Success(Source, Output, Success),
    Failure(FSource, Failure),
}

impl<Source, FSource, Output, Success, Failure>
    ParsingState<Source, FSource, Output, Success, Failure>
{
    pub fn with_mode(
        self,
        mode: FailureMode,
    ) -> ParsingStateWithMode<Source, FSource, Output, Success, Failure> {
        ParsingStateWithMode { mode, state: self }
    }

    pub fn is_success(&self) -> bool {
        matches!(self, ParsingState::Success(..))
    }

    pub fn map_output<T>(
        self,
        f: impl FnOnce(Output) -> T,
    ) -> ParsingState<Source, FSource, T, Success, Failure> {
        match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, f(output), success)
            }
            ParsingState::Failure(source, failure) => ParsingState::Failure(source, failure),
        }
    }

    pub fn try_map_output<T, FailFast>(
        self,
        f: impl FnOnce(Output) -> Result<T, FailFast>,
    ) -> Result<ParsingState<Source, FSource, T, Success, Failure>, FailFast> {
        let r = match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, f(output)?, success)
            }
            ParsingState::Failure(source, failure) => ParsingState::Failure(source, failure),
        };

        Ok(r)
    }

    pub fn map_success<T>(
        self,
        f: impl FnOnce(Success) -> T,
    ) -> ParsingState<Source, FSource, Output, T, Failure> {
        match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, output, f(success))
            }
            ParsingState::Failure(source, failure) => ParsingState::Failure(source, failure),
        }
    }

    pub fn map_failure<T>(
        self,
        f: impl FnOnce(Failure) -> T,
    ) -> ParsingState<Source, FSource, Output, Success, T> {
        match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, output, success)
            }
            ParsingState::Failure(source, failure) => ParsingState::Failure(source, f(failure)),
        }
    }

    pub fn map_fsource<T>(
        self,
        f: impl FnOnce(FSource) -> T,
    ) -> ParsingState<Source, T, Output, Success, Failure> {
        match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, output, success)
            }
            ParsingState::Failure(source, failure) => ParsingState::Failure(f(source), failure),
        }
    }

    pub fn transform<Out>(
        self,
        f: impl FnOnce(Output) -> Result<Out, Failure>,
    ) -> ParsingState<Source, (), Out, Success, Failure> {
        match self {
            ParsingState::Success(s, output, success) => match f(output) {
                Ok(output) => ParsingState::Success(s, output, success),
                Err(failure) => ParsingState::Failure((), failure),
            },
            ParsingState::Failure(_, failure) => ParsingState::Failure((), failure),
        }
    }

    pub fn else_failure<Fail>(
        self,
        f: impl FnOnce(FSource, Failure) -> Fail,
    ) -> ParsingState<Source, (), Output, Success, Fail> {
        match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, output, success)
            }
            ParsingState::Failure(source, failure) => ParsingState::Failure((), f(source, failure)),
        }
    }

    pub fn inspect(self, f: impl FnOnce(&Output)) -> Self {
        if let ParsingState::Success(_, output, _) = &self {
            f(output);
        }

        self
    }

    pub fn and<P, Out>(
        self,
        p: P,
        f: impl FnOnce(Output, P::Output) -> Out,
    ) -> Result<
        ParsingState<Source, (), Out, <Success as Arrow<P::Success>>::Output, Failure>,
        P::FailFast,
    >
    where
        P: ParseOnce<Source>,
        Success: Arrow<P::Success> + Arrow<P::Failure>,
        <Success as Arrow<P::Failure>>::Output: Into<Failure>,
    {
        self.then(|output| p.map_output(|output2| f(output, output2)))
    }

    pub fn then<P>(
        self,
        f: impl FnOnce(Output) -> P,
    ) -> Result<
        ParsingState<Source, (), P::Output, <Success as Arrow<P::Success>>::Output, Failure>,
        P::FailFast,
    >
    where
        P: ParseOnce<Source>,
        Success: Arrow<P::Success> + Arrow<P::Failure>,
        <Success as Arrow<P::Failure>>::Output: Into<Failure>,
    {
        let r = match self {
            ParsingState::Success(s, output, success) => match f(output).parse_once(s)? {
                ParsingState::Success(s, output, success2) => {
                    ParsingState::Success(s, output, success.arrow(success2))
                }
                ParsingState::Failure(_, failure) => {
                    ParsingState::Failure((), success.arrow(failure).into())
                }
            },
            ParsingState::Failure(_, failure) => ParsingState::Failure((), failure),
        };

        Ok(r)
    }
}

impl<Source, Output, Success, Failure> ParsingState<Source, Source, Output, Success, Failure> {
    pub fn or<P>(
        self,
        p: P,
    ) -> Result<
        ParsingState<Source, Source, Output, Success, <Failure as Arrow<P::Failure>>::Output>,
        P::FailFast,
    >
    where
        Source: Clone,
        P: ParseOnce<Source>,
        P::Output: Into<Output>,
        Failure: Arrow<P::Success> + Arrow<P::Failure>,
        <Failure as Arrow<P::Success>>::Output: Into<Success>,
    {
        let r = match self {
            ParsingState::Success(s, output, success) => ParsingState::Success(s, output, success),
            ParsingState::Failure(s, failure) => match p.parse_once(s.clone())? {
                ParsingState::Success(s, output, success) => {
                    ParsingState::Success(s, output.into(), failure.arrow(success).into())
                }
                ParsingState::Failure(_, failure2) => {
                    ParsingState::Failure(s, failure.arrow(failure2))
                }
            },
        };

        Ok(r)
    }
}

#[derive(Debug)]
pub struct ParsingStateWithMode<Source, FSource, Output, Success, Failure> {
    mode: FailureMode,
    state: ParsingState<Source, FSource, Output, Success, Failure>,
}

impl<Source, FSource, Output, Success, Failure>
    ParsingStateWithMode<Source, FSource, Output, Success, Failure>
{
    pub fn with_mode(self, mode: FailureMode) -> Self {
        ParsingStateWithMode { mode, ..self }
    }

    pub fn collapse(self) -> ParsingState<Source, FSource, Output, Success, Failure> {
        self.state
    }

    pub fn map_output<T>(
        self,
        f: impl FnOnce(Output) -> T,
    ) -> ParsingStateWithMode<Source, FSource, T, Success, Failure> {
        ParsingStateWithMode {
            mode: self.mode,
            state: self.state.map_output(f),
        }
    }

    pub fn try_map_output<T, FailFast>(
        self,
        f: impl FnOnce(Output) -> Result<T, FailFast>,
    ) -> Result<ParsingStateWithMode<Source, FSource, T, Success, Failure>, FailFast> {
        let r = ParsingStateWithMode {
            mode: self.mode,
            state: self.state.try_map_output(f)?,
        };

        Ok(r)
    }

    pub fn map_success<T>(
        self,
        f: impl FnOnce(Success) -> T,
    ) -> ParsingStateWithMode<Source, FSource, Output, T, Failure> {
        ParsingStateWithMode {
            mode: self.mode,
            state: self.state.map_success(f),
        }
    }

    pub fn map_failure<T>(
        self,
        f: impl FnOnce(Failure) -> T,
    ) -> ParsingStateWithMode<Source, FSource, Output, Success, T> {
        ParsingStateWithMode {
            mode: self.mode,
            state: self.state.map_failure(f),
        }
    }

    pub fn transform<Out>(
        self,
        f: impl FnOnce(Output) -> Result<Out, Failure>,
    ) -> ParsingStateWithMode<Source, (), Out, Success, Failure> {
        ParsingStateWithMode {
            mode: self.mode,
            state: self.state.transform(f),
        }
    }

    pub fn inspect(self, f: impl FnOnce(&Output)) -> Self {
        ParsingStateWithMode {
            mode: self.mode,
            state: self.state.inspect(f),
        }
    }

    pub fn and<P, Out>(
        self,
        p: P,
        f: impl FnOnce(Output, P::Output) -> Out,
    ) -> Result<
        ParsingStateWithMode<Source, (), Out, <Success as Arrow<P::Success>>::Output, Failure>,
        P::FailFast,
    >
    where
        P: ParseOnce<Source>,
        Success: Arrow<P::Success> + Arrow<P::Failure>,
        <Success as Arrow<P::Failure>>::Output: Into<Failure>,
        Failure: WithMode,
    {
        self.then(|out| p.map_output(|out2| f(out, out2)))
    }

    pub fn then<P>(
        self,
        f: impl FnOnce(Output) -> P,
    ) -> Result<
        ParsingStateWithMode<
            Source,
            (),
            P::Output,
            <Success as Arrow<P::Success>>::Output,
            Failure,
        >,
        P::FailFast,
    >
    where
        P: ParseOnce<Source>,
        Success: Arrow<P::Success> + Arrow<P::Failure>,
        <Success as Arrow<P::Failure>>::Output: Into<Failure>,
        Failure: WithMode,
    {
        let ParsingStateWithMode { mode, state } = self;

        let is_success = state.is_success();

        let state = state
            .then(f)?
            .map_failure(|f| if is_success { f.with_mode(mode) } else { f });

        let r = ParsingStateWithMode { mode, state };

        Ok(r)
    }
}

impl<Source, Output, Success, Failure>
    ParsingStateWithMode<Source, Source, Output, Success, Failure>
{
    pub fn or<P>(
        self,
        p: P,
    ) -> Result<
        ParsingStateWithMode<
            Source,
            Source,
            Output,
            Success,
            <Failure as Arrow<P::Failure>>::Output,
        >,
        P::FailFast,
    >
    where
        Source: Clone,
        P: ParseOnce<Source>,
        P::Output: Into<Output>,
        Failure: Arrow<P::Success> + Arrow<P::Failure>,
        <Failure as Arrow<P::Success>>::Output: Into<Success>,
    {
        let r = ParsingStateWithMode {
            mode: self.mode,
            state: self.state.or(p)?,
        };

        Ok(r)
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn replace<U>(self, new_value: U) -> (T, Spanned<U>) {
        let Spanned { value, span } = self;

        let r = Spanned {
            value: new_value,
            span,
        };

        (value, r)
    }

    pub fn take(self) -> (T, Spanned<()>) {
        self.replace(())
    }

    pub fn put<U>(self, u: U) -> Spanned<U> {
        self.replace(u).1
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        let Spanned { value, span } = self;

        Spanned {
            value: f(value),
            span,
        }
    }
}

pub fn keep<T, U>(t: Spanned<T>, u: Spanned<U>) -> Spanned<(T, U)> {
    let Spanned {
        value: t,
        span: span_t,
    } = t;

    let Spanned {
        value: u,
        span: span_u,
    } = u;

    Spanned {
        value: (t, u),
        span: span_t.start..span_u.end,
    }
}

pub fn replace<T, U>(t: Spanned<T>, u: Spanned<U>) -> Spanned<U> {
    let Spanned { span, value } = u;

    Spanned {
        value,
        span: t.span.start..span.end,
    }
}

pub fn discard<T, U>(t: Spanned<T>, u: Spanned<U>) -> Spanned<T> {
    let Spanned { span, value } = t;

    Spanned {
        value,
        span: span.start..u.span.end,
    }
}

pub fn opt_keep<T, U>(t: Spanned<T>, u: Option<Spanned<U>>) -> Spanned<(T, Option<U>)> {
    match u {
        Some(u) => keep(t, u.map(Some)),
        None => t.map(|t| (t, None)),
    }
}

pub fn opt_discard<T, U>(t: Spanned<T>, u: Option<Spanned<U>>) -> Spanned<T> {
    match u {
        Some(u) => discard(t, u),
        None => t,
    }
}

pub fn opt_replace<T, U>(t: Spanned<T>, u: Option<Spanned<U>>) -> Spanned<Option<U>> {
    match u {
        Some(u) => replace(t, u.map(Some)),
        None => t.map(|_| None),
    }
}
