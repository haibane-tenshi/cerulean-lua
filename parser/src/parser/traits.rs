use std::marker::PhantomData;

pub trait ParseOnce<Source>: Sized {
    type Output;
    type Success;
    type Failure;
    type FailFast;

    fn parse_once(
        self,
        _: Source,
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast>;

    fn optional<SuccessOut>(self) -> Optional<Self, SuccessOut> {
        Optional {
            parser: self,
            _marker: PhantomData,
        }
    }
}

impl<F, Source, Output, Success, Failure, FailFast> ParseOnce<Source> for F
where
    F: FnOnce(Source) -> Result<Result<(Source, Output, Success), Failure>, FailFast>,
{
    type Output = Output;
    type Success = Success;
    type Failure = Failure;
    type FailFast = FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast> {
        (self)(s)
    }
}

pub struct Optional<P, SuccessOut> {
    parser: P,
    _marker: PhantomData<fn() -> SuccessOut>,
}

impl<Source, P, SuccessOut> ParseOnce<Source> for Optional<P, SuccessOut>
where
    P: ParseOnce<Source>,
    Source: Clone,
    P::Success: Into<SuccessOut>,
    P::Failure: Into<SuccessOut>,
{
    type Output = Option<P::Output>;
    type Success = SuccessOut;
    type Failure = std::convert::Infallible;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast> {
        let r = match self.parser.parse_once(s.clone())? {
            Ok((s, output, reason)) => Ok((s, Some(output), reason.into())),
            Err(failure) => Ok((s, None, failure.into())),
        };

        Ok(r)
    }
}

pub trait ParseMut<Source>: ParseOnce<Source> {
    fn parse_mut(
        &mut self,
        _: Source,
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast>;

    fn repeat(self) -> Repeat<Self> {
        Repeat { parser: self }
    }
}

impl<F, Source, Output, Success, Failure, FailFast> ParseMut<Source> for F
where
    F: FnMut(Source) -> Result<Result<(Source, Output, Success), Failure>, FailFast>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast> {
        (self)(s)
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
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast> {
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
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast> {
        let (mut s, _, mut reason) = match self.parser.parse_mut(s.clone())? {
            Ok(r) => r,
            Err(failure) => return Ok(Ok((s, (), failure.into()))),
        };

        let reason = loop {
            match self.parser.parse_mut(s.clone())? {
                Ok((ns, _, nreason)) => {
                    s = ns;
                    reason = nreason;
                }
                Err(err) => break reason.combine(err),
            }
        };

        Ok(Ok((s, (), reason)))
    }
}

pub trait Parse<Source>: ParseMut<Source> {
    fn parse(
        self,
        _: Source,
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast>;
}

impl<F, Source, Output, Success, Failure, FailFast> Parse<Source> for F
where
    F: Fn(Source) -> Result<Result<(Source, Output, Success), Failure>, FailFast>,
{
    fn parse(
        self,
        s: Source,
    ) -> Result<Result<(Source, Self::Output, Self::Success), Self::Failure>, Self::FailFast> {
        (self)(s)
    }
}

pub trait Combine<Other> {
    type Output;

    fn combine(self, other: Other) -> Self::Output;
}

pub trait Pipeline<F> {
    type Output;

    fn map_output(self, f: F) -> Self::Output;
}

impl<Source, Output, Success, Failure, F, Output2> Pipeline<F>
    for Result<(Source, Output, Success), Failure>
where
    F: FnOnce(Output) -> Output2,
{
    type Output = Result<(Source, Output2, Success), Failure>;

    fn map_output(self, f: F) -> Self::Output {
        self.map(move |(s, output, reason)| (s, f(output), reason))
    }
}

pub trait PipelineThen<P> {
    type Output;

    fn then(self, parser: P) -> Self::Output;
}

impl<Source, Output, Success, Failure, P> PipelineThen<P>
    for Result<(Source, Output, Success), Failure>
where
    P: ParseOnce<Source>,
    Success: Combine<P::Failure>,
    Failure: Into<<Success as Combine<P::Failure>>::Output>,
{
    type Output = Result<
        Result<(Source, (Output, P::Output), P::Success), <Success as Combine<P::Failure>>::Output>,
        P::FailFast,
    >;

    fn then(self, p: P) -> Self::Output {
        let r = match self {
            Ok((s, output, reason)) => match p.parse_once(s)? {
                Ok((s, output2, reason)) => Ok((s, (output, output2), reason)),
                Err(failure) => Err(reason.combine(failure)),
            },
            Err(failure) => Err(failure.into()),
        };

        Ok(r)
    }
}

// pub trait PipelineOptional<P> {
//     type Output;

//     fn optional(self, p: P) -> Self::Output;
// }

// impl<Source, Output, Success, Failure, P> PipelineOptional<P> for Result<(Source, Output, Success), Failure>
// where
//     P: ParseOnce<Source>,
//     Source: Clone,
//     Success: Combine<P::Failure>,
//     P::Success: Into<<Success as Combine<P::Failure>>::Output>,
// {
//     type Output = Result<Result<(Source, (Output, Option<P::Output>), <Success as Combine<P::Failure>>::Output), Failure>, P::FailFast>;

//     fn optional(self, p: P) -> Self::Output {
//         let r = match self {
//             Ok((s, output, reason)) => match p.parse_once(s.clone())? {
//                 Ok((s, output2, reason)) => Ok((s, (output, Some(output2)), reason.into())),
//                 Err(failure) => Ok((s, (output, None), reason.combine(failure)))
//             }
//             Err(failure) => Err(failure),
//         };

//         Ok(r)
//     }
// }
