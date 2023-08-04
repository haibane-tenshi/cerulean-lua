use super::error::Combine;
use super::error::{FailureMode, WithMode};

pub trait ParseOnce<Source>: Sized {
    type Output;
    type Success;
    type Failure;
    type FailFast;

    fn parse_once(
        self,
        _: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>;

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
    F: FnOnce(Source) -> Result<ParsingState<Source, Output, Success, Failure>, FailFast>,
{
    type Output = Output;
    type Success = Success;
    type Failure = Failure;
    type FailFast = FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        let r = match self.parser.parse_once(s.clone())? {
            ParsingState::Success(s, output, reason) => {
                ParsingState::Success(s, Some(output), reason)
            }
            ParsingState::Failure(failure) => ParsingState::Success(s, None, failure.into()),
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        Ok(self.parser.parse(s)?.map_failure(&self.f))
    }
}

pub trait ParseMut<Source>: ParseOnce<Source> {
    fn parse_mut(
        &mut self,
        _: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>;

    fn repeat_with<F>(self, fold: F) -> RepeatWith<Self, F> {
        RepeatWith { parser: self, fold }
    }

    fn repeat(self) -> Repeat<Self> {
        Repeat { parser: self }
    }
}

impl<F, Source, Output, Success, Failure, FailFast> ParseMut<Source> for F
where
    F: FnMut(Source) -> Result<ParsingState<Source, Output, Success, Failure>, FailFast>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    P::Output: Default,
    P::Success: Combine<P::Failure>,
    P::Failure: Into<<P::Success as Combine<P::Failure>>::Output>,
    F: FnMut(P::Output, P::Output) -> P::Output,
{
    type Output = P::Output;
    type Success = <P::Success as Combine<P::Failure>>::Output;
    type Failure = std::convert::Infallible;
    type FailFast = P::FailFast;

    fn parse_once(
        mut self,
        s: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parse_mut(s)
    }
}

impl<P, F, Source> ParseMut<Source> for RepeatWith<P, F>
where
    Source: Clone,
    P: ParseMut<Source>,
    P::Output: Default,
    P::Success: Combine<P::Failure>,
    P::Failure: Into<<P::Success as Combine<P::Failure>>::Output>,
    F: FnMut(P::Output, P::Output) -> P::Output,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        let (mut s, mut output, mut reason) = match self.parser.parse_mut(s.clone())? {
            ParsingState::Success(s, output, reason) => (s, output, reason),
            ParsingState::Failure(failure) => {
                return Ok(ParsingState::Success(s, Default::default(), failure.into()))
            }
        };

        let reason = loop {
            match self.parser.parse_mut(s.clone())? {
                ParsingState::Success(ns, noutput, nreason) => {
                    s = ns;
                    reason = nreason;
                    output = (self.fold)(output, noutput);
                }
                ParsingState::Failure(err) => break reason.combine(err),
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
    P::Success: Combine<P::Failure>,
    P::Failure: Into<<P::Success as Combine<P::Failure>>::Output>,
{
    type Output = ();
    type Success = <P::Success as Combine<P::Failure>>::Output;
    type Failure = std::convert::Infallible;
    type FailFast = P::FailFast;

    fn parse_once(
        mut self,
        s: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        self.parse_mut(s)
    }
}

impl<P, Source> ParseMut<Source> for Repeat<P>
where
    Source: Clone,
    P: ParseMut<Source>,
    P::Success: Combine<P::Failure>,
    P::Failure: Into<<P::Success as Combine<P::Failure>>::Output>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
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
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>;
}

impl<F, Source, Output, Success, Failure, FailFast> Parse<Source> for F
where
    F: Fn(Source) -> Result<ParsingState<Source, Output, Success, Failure>, FailFast>,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<ParsingState<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>
    {
        (self)(s)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParsingState<Source, Output, Success, Failure> {
    Success(Source, Output, Success),
    Failure(Failure),
}

impl<Source, Output, Success, Failure> ParsingState<Source, Output, Success, Failure> {
    pub fn with_mode(
        self,
        mode: FailureMode,
    ) -> ParsingStateWithMode<Source, Output, Success, Failure> {
        ParsingStateWithMode { mode, state: self }
    }

    pub fn is_success(&self) -> bool {
        matches!(self, ParsingState::Success(..))
    }

    pub fn map_output<T>(
        self,
        f: impl FnOnce(Output) -> T,
    ) -> ParsingState<Source, T, Success, Failure> {
        match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, f(output), success)
            }
            ParsingState::Failure(failure) => ParsingState::Failure(failure),
        }
    }

    pub fn try_map_output<T, FailFast>(
        self,
        f: impl FnOnce(Output) -> Result<T, FailFast>,
    ) -> Result<ParsingState<Source, T, Success, Failure>, FailFast> {
        let r = match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, f(output)?, success)
            }
            ParsingState::Failure(failure) => ParsingState::Failure(failure),
        };

        Ok(r)
    }

    pub fn map_success<T>(
        self,
        f: impl FnOnce(Success) -> T,
    ) -> ParsingState<Source, Output, T, Failure> {
        match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, output, f(success))
            }
            ParsingState::Failure(failure) => ParsingState::Failure(failure),
        }
    }

    pub fn map_failure<T>(
        self,
        f: impl FnOnce(Failure) -> T,
    ) -> ParsingState<Source, Output, Success, T> {
        match self {
            ParsingState::Success(source, output, success) => {
                ParsingState::Success(source, output, success)
            }
            ParsingState::Failure(failure) => ParsingState::Failure(f(failure)),
        }
    }

    pub fn transform<Out>(
        self,
        f: impl FnOnce(Output) -> Result<Out, Failure>,
    ) -> ParsingState<Source, Out, Success, Failure> {
        match self {
            ParsingState::Success(s, output, success) => match f(output) {
                Ok(output) => ParsingState::Success(s, output, success),
                Err(failure) => ParsingState::Failure(failure),
            },
            ParsingState::Failure(failure) => ParsingState::Failure(failure),
        }
    }

    pub fn inspect(self, f: impl FnOnce(&Output)) -> Self {
        if let ParsingState::Success(_, output, _) = &self {
            f(output);
        }

        self
    }

    pub fn and_with<P, Out>(
        self,
        p: P,
        f: impl FnOnce(Output, P::Output) -> Out,
    ) -> Result<ParsingState<Source, Out, P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
    {
        self.then(|output| p.map_output(|output2| f(output, output2)))
    }

    pub fn and<P>(
        self,
        p: P,
    ) -> Result<ParsingState<Source, (Output, P::Output), P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
    {
        self.and_with(p, |out, out2| (out, out2))
    }

    pub fn and_discard<P>(
        self,
        p: P,
    ) -> Result<ParsingState<Source, Output, P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
    {
        self.and_with(p, |out, _| out)
    }

    pub fn and_replace<P>(
        self,
        p: P,
    ) -> Result<ParsingState<Source, P::Output, P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
    {
        self.and_with(p, |_, out| out)
    }

    pub fn then<P>(
        self,
        f: impl FnOnce(Output) -> P,
    ) -> Result<ParsingState<Source, P::Output, P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
    {
        let r = match self {
            ParsingState::Success(s, output, reason) => match f(output).parse_once(s)? {
                ParsingState::Success(s, output2, reason) => {
                    ParsingState::Success(s, output2, reason)
                }
                ParsingState::Failure(failure) => {
                    ParsingState::Failure(reason.combine(failure).into())
                }
            },
            ParsingState::Failure(failure) => ParsingState::Failure(failure),
        };

        Ok(r)
    }

    pub fn or<P>(
        self,
        s: Source,
        p: P,
    ) -> Result<
        ParsingState<Source, Output, Success, <Failure as Combine<P::Failure>>::Output>,
        P::FailFast,
    >
    where
        P: ParseOnce<Source>,
        P::Output: Into<Output>,
        P::Success: Into<Success>,
        Failure: Combine<P::Failure>,
    {
        let r = match self {
            ParsingState::Success(s, output, success) => ParsingState::Success(s, output, success),
            ParsingState::Failure(failure) => match p.parse_once(s)? {
                ParsingState::Success(s, output, success) => {
                    ParsingState::Success(s, output.into(), success.into())
                }
                ParsingState::Failure(failure2) => ParsingState::Failure(failure.combine(failure2)),
            },
        };

        Ok(r)
    }
}

pub struct ParsingStateWithMode<Source, Output, Success, Failure> {
    mode: FailureMode,
    state: ParsingState<Source, Output, Success, Failure>,
}

impl<Source, Output, Success, Failure> ParsingStateWithMode<Source, Output, Success, Failure> {
    pub fn with_mode(self, mode: FailureMode) -> Self {
        ParsingStateWithMode { mode, ..self }
    }

    pub fn collapse(self) -> ParsingState<Source, Output, Success, Failure> {
        self.state
    }

    pub fn map_output<T>(
        self,
        f: impl FnOnce(Output) -> T,
    ) -> ParsingStateWithMode<Source, T, Success, Failure> {
        ParsingStateWithMode {
            mode: self.mode,
            state: self.state.map_output(f),
        }
    }

    pub fn try_map_output<T, FailFast>(
        self,
        f: impl FnOnce(Output) -> Result<T, FailFast>,
    ) -> Result<ParsingStateWithMode<Source, T, Success, Failure>, FailFast> {
        let r = ParsingStateWithMode {
            mode: self.mode,
            state: self.state.try_map_output(f)?,
        };

        Ok(r)
    }

    pub fn map_success<T>(
        self,
        f: impl FnOnce(Success) -> T,
    ) -> ParsingStateWithMode<Source, Output, T, Failure> {
        ParsingStateWithMode {
            mode: self.mode,
            state: self.state.map_success(f),
        }
    }

    pub fn map_failure<T>(
        self,
        f: impl FnOnce(Failure) -> T,
    ) -> ParsingStateWithMode<Source, Output, Success, T> {
        ParsingStateWithMode {
            mode: self.mode,
            state: self.state.map_failure(f),
        }
    }

    pub fn transform<Out>(
        self,
        f: impl FnOnce(Output) -> Result<Out, Failure>,
    ) -> ParsingStateWithMode<Source, Out, Success, Failure> {
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

    pub fn and_with<P, Out>(
        self,
        p: P,
        f: impl FnOnce(Output, P::Output) -> Out,
    ) -> Result<ParsingStateWithMode<Source, Out, P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
        Failure: WithMode,
    {
        self.then(|out| p.map_output(|out2| f(out, out2)))
    }

    pub fn and<P>(
        self,
        p: P,
    ) -> Result<ParsingStateWithMode<Source, (Output, P::Output), P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
        Failure: WithMode,
    {
        self.and_with(p, |out, out2| (out, out2))
    }

    pub fn and_discard<P>(
        self,
        p: P,
    ) -> Result<ParsingStateWithMode<Source, Output, P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
        Failure: WithMode,
    {
        self.and_with(p, |out, _| out)
    }

    pub fn and_replace<P>(
        self,
        p: P,
    ) -> Result<ParsingStateWithMode<Source, P::Output, P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
        Failure: WithMode,
    {
        self.and_with(p, |_, out| out)
    }

    pub fn then<P>(
        self,
        f: impl FnOnce(Output) -> P,
    ) -> Result<ParsingStateWithMode<Source, P::Output, P::Success, Failure>, P::FailFast>
    where
        P: ParseOnce<Source>,
        Success: Combine<P::Failure>,
        <Success as Combine<P::Failure>>::Output: Into<Failure>,
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

    pub fn or<P>(
        self,
        s: Source,
        p: P,
    ) -> Result<
        ParsingStateWithMode<Source, Output, Success, <Failure as Combine<P::Failure>>::Output>,
        P::FailFast,
    >
    where
        P: ParseOnce<Source>,
        P::Output: Into<Output>,
        P::Success: Into<Success>,
        Failure: Combine<P::Failure>,
    {
        let r = ParsingStateWithMode {
            mode: self.mode,
            state: self.state.or(s, p)?,
        };

        Ok(r)
    }
}
