#[derive(Debug, Clone, Copy)]
pub enum State<Source, Output, Success, Failure> {
    Success(Source, Output, Success),
    Failure(Failure),
}

impl<Source, Output, Success, Failure> State<Source, Output, Success, Failure> {
    pub fn map_output<T>(self, f: impl FnOnce(Output) -> T) -> State<Source, T, Success, Failure> {
        match self {
            State::Success(source, output, success) => State::Success(source, f(output), success),
            State::Failure(failure) => State::Failure(failure),
        }
    }

    pub fn map_success<T>(self, f: impl FnOnce(Success) -> T) -> State<Source, Output, T, Failure> {
        match self {
            State::Success(source, output, success) => State::Success(source, output, f(success)),
            State::Failure(failure) => State::Failure(failure),
        }
    }

    pub fn map_failure<T>(self, f: impl FnOnce(Failure) -> T) -> State<Source, Output, Success, T> {
        match self {
            State::Success(source, output, success) => State::Success(source, output, success),
            State::Failure(failure) => State::Failure(f(failure)),
        }
    }
}

pub trait ParseOnce<Source>: Sized {
    type Output;
    type Success;
    type Failure;
    type FailFast;

    #[expect(clippy::type_complexity)]
    fn parse_once(
        self,
        _: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>;

    fn as_ref(&self) -> AsRef<'_, Self> {
        AsRef { parser: self }
    }

    fn as_mut(&mut self) -> AsMut<'_, Self> {
        AsMut { parser: self }
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

pub trait ParseMut<Source>: ParseOnce<Source> {
    #[expect(clippy::type_complexity)]
    fn parse_mut(
        &mut self,
        _: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>;
}

pub trait Parse<Source>: ParseMut<Source> {
    #[expect(clippy::type_complexity)]
    fn parse(
        &self,
        _: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast>;
}

impl<F, Source, Output, Success, Failure, FailFast> ParseOnce<Source> for F
where
    F: FnOnce(Source) -> Result<State<Source, Output, Success, Failure>, FailFast>,
{
    type Output = Output;
    type Success = Success;
    type Failure = Failure;
    type FailFast = FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        (self)(s)
    }
}

impl<F, Source, Output, Success, Failure, FailFast> ParseMut<Source> for F
where
    F: FnMut(Source) -> Result<State<Source, Output, Success, Failure>, FailFast>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        (self)(s)
    }
}

impl<F, Source, Output, Success, Failure, FailFast> Parse<Source> for F
where
    F: Fn(Source) -> Result<State<Source, Output, Success, Failure>, FailFast>,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        (self)(s)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AsRef<'a, P> {
    parser: &'a P,
}

impl<P, Source> ParseOnce<Source> for AsRef<'_, P>
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        self.parser.parse(s)
    }
}

impl<P, Source> ParseMut<Source> for AsRef<'_, P>
where
    P: Parse<Source>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        self.parser.parse(s)
    }
}

impl<P, Source> Parse<Source> for AsRef<'_, P>
where
    P: Parse<Source>,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        self.parser.parse(s)
    }
}

#[derive(Debug)]
pub struct AsMut<'a, P> {
    parser: &'a mut P,
}

impl<P, Source> ParseOnce<Source> for AsMut<'_, P>
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        self.parser.parse_mut(s)
    }
}

impl<P, Source> ParseMut<Source> for AsMut<'_, P>
where
    P: ParseMut<Source>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        self.parser.parse_mut(s)
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        Ok(self.parser.parse_once(s)?.map_success(self.f))
    }
}

impl<Source, P, F, T> ParseMut<Source> for MapSuccess<P, F>
where
    P: ParseMut<Source>,
    F: FnMut(P::Success) -> T,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        Ok(self.parser.parse_mut(s)?.map_success(&mut self.f))
    }
}

impl<Source, P, F, T> Parse<Source> for MapSuccess<P, F>
where
    P: Parse<Source>,
    F: Fn(P::Success) -> T,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        Ok(self.parser.parse(s)?.map_success(&self.f))
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
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
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        Ok(self.parser.parse(s)?.map_failure(&self.f))
    }
}
