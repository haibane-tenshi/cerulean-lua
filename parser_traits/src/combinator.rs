use crate::reason::{Compress, Compressed, Never};
use crate::traits::{Parse, ParseMut, ParseOnce, State};

pub fn optional<P>(p: P) -> Optional<P> {
    Optional { parser: p }
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
    type Failure = Never;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        let r = match self.parser.parse_once(s.clone())? {
            State::Success(s, output, reason) => State::Success(s, Some(output), reason),
            State::Failure(failure) => State::Success(s, None, failure.into()),
        };

        Ok(r)
    }
}

impl<Source, P> ParseMut<Source> for Optional<P>
where
    P: ParseMut<Source>,
    Source: Clone,
    P::Failure: Into<P::Success>,
{
    fn parse_mut(
        &mut self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        let r = match self.parser.parse_mut(s.clone())? {
            State::Success(s, output, reason) => State::Success(s, Some(output), reason),
            State::Failure(failure) => State::Success(s, None, failure.into()),
        };

        Ok(r)
    }
}

impl<Source, P> Parse<Source> for Optional<P>
where
    P: Parse<Source>,
    Source: Clone,
    P::Failure: Into<P::Success>,
{
    fn parse(
        &self,
        s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        let r = match self.parser.parse(s.clone())? {
            State::Success(s, output, reason) => State::Success(s, Some(output), reason),
            State::Failure(failure) => State::Success(s, None, failure.into()),
        };

        Ok(r)
    }
}

pub fn repeat_fold<P, R, F>(p: P, init: R, fold: F) -> RepeatFold<P, R, F> {
    RepeatFold {
        parser: p,
        value: init,
        fold,
    }
}

pub struct RepeatFold<P, R, F> {
    parser: P,
    value: R,
    fold: F,
}

impl<P, R, F, Source> ParseOnce<Source> for RepeatFold<P, R, F>
where
    Source: Clone,
    P: ParseMut<Source>,
    P::Success: Compress<P::Success, Output = P::Success> + Compress<P::Failure>,
    P::Failure: Into<Compressed<P::Success, P::Failure>>,
    F: FnMut(R, P::Output) -> R,
{
    type Output = R;
    type Success = Compressed<P::Success, P::Failure>;
    type Failure = P::Failure;
    type FailFast = P::FailFast;

    fn parse_once(
        self,
        mut s: Source,
    ) -> Result<State<Source, Self::Output, Self::Success, Self::Failure>, Self::FailFast> {
        let RepeatFold {
            mut parser,
            mut value,
            mut fold,
        } = self;

        let mut success = match parser.parse_mut(s.clone())? {
            State::Success(new_s, out, reason) => {
                s = new_s;
                value = fold(value, out);
                reason
            }
            State::Failure(reason) => return Ok(State::Success(s, value, reason.into())),
        };

        let reason = loop {
            match parser.parse_mut(s.clone())? {
                State::Success(new_s, out, reason) => {
                    s = new_s;
                    value = fold(value, out);
                    success = success.compress(reason);
                }
                State::Failure(reason) => break success.compress(reason),
            }
        };

        Ok(State::Success(s, value, reason))
    }
}
