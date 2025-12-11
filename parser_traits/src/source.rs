use crate::reason::{Complete, Compress, Compressed, Never};
use crate::traits::{ParseOnce, State};

#[derive(Debug, Clone, Copy)]
enum InnerState<Output, Success, Failure> {
    Success(Output, Success),
    Failure(Failure),
}

impl<Output, Success, Failure> InnerState<Output, Success, Failure> {
    fn map_output<T>(self, f: impl FnOnce(Output) -> T) -> InnerState<T, Success, Failure> {
        match self {
            InnerState::Success(output, success) => InnerState::Success(f(output), success),
            InnerState::Failure(failure) => InnerState::Failure(failure),
        }
    }

    fn map_success<T>(self, f: impl FnOnce(Success) -> T) -> InnerState<Output, T, Failure> {
        match self {
            InnerState::Success(output, success) => InnerState::Success(output, f(success)),
            InnerState::Failure(failure) => InnerState::Failure(failure),
        }
    }

    fn map_failure<T>(self, f: impl FnOnce(Failure) -> T) -> InnerState<Output, Success, T> {
        match self {
            InnerState::Success(output, success) => InnerState::Success(output, success),
            InnerState::Failure(failure) => InnerState::Failure(f(failure)),
        }
    }

    fn try_map_output<T, E>(
        self,
        f: impl FnOnce(Output) -> Result<T, E>,
    ) -> Result<InnerState<T, Success, Failure>, E> {
        let r = match self {
            InnerState::Success(output, success) => InnerState::Success(f(output)?, success),
            InnerState::Failure(failure) => InnerState::Failure(failure),
        };
        Ok(r)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Source<S, Output, Success, Failure> {
    source: S,
    state: InnerState<Output, Success, Failure>,
}

impl<S> Source<S, (), Complete, Never> {
    pub fn new(source: S) -> Self {
        Source {
            source,
            state: InnerState::Success((), Complete),
        }
    }
}

impl<S, Output, Success, Failure> Source<S, Output, Success, Failure> {
    pub fn is_success(&self) -> bool {
        matches!(self.state, InnerState::Success(..))
    }

    pub fn is_failure(&self) -> bool {
        matches!(self.state, InnerState::Failure(..))
    }

    pub fn map_output<T>(self, f: impl FnOnce(Output) -> T) -> Source<S, T, Success, Failure> {
        let Source { source, state } = self;

        let state = state.map_output(f);

        Source { source, state }
    }

    pub fn try_map_output<T, FailFast>(
        self,
        f: impl FnOnce(Output) -> Result<T, FailFast>,
    ) -> Result<Source<S, T, Success, Failure>, FailFast> {
        let Source { source, state } = self;

        let state = state.try_map_output(f)?;

        let r = Source { source, state };
        Ok(r)
    }

    pub fn map_success<T>(self, f: impl FnOnce(Success) -> T) -> Source<S, Output, T, Failure> {
        let Source { source, state } = self;

        let state = state.map_success(f);

        Source { source, state }
    }

    pub fn map_failure<T>(self, f: impl FnOnce(Failure) -> T) -> Source<S, Output, Success, T> {
        let Source { source, state } = self;

        let state = state.map_failure(f);

        Source { source, state }
    }

    pub fn inspect(self, f: impl FnOnce(&Output)) -> Self {
        if let InnerState::Success(output, _) = &self.state {
            f(output);
        }

        self
    }

    #[expect(clippy::type_complexity)]
    pub fn and<P>(
        self,
        p: P,
    ) -> Result<Source<S, (Output, P::Output), Compressed<Success, P::Success>, Failure>, P::FailFast>
    where
        S: Clone,
        P: ParseOnce<S>,
        Success: Compress<P::Success> + Compress<P::Failure>,
        Compressed<Success, P::Failure>: Into<Failure>,
    {
        self.and_with(p, |out0, out1| (out0, out1))
    }

    #[expect(clippy::type_complexity)]
    pub fn and_with<P, T>(
        self,
        p: P,
        f: impl FnOnce(Output, P::Output) -> T,
    ) -> Result<Source<S, T, Compressed<Success, P::Success>, Failure>, P::FailFast>
    where
        S: Clone,
        P: ParseOnce<S>,
        Success: Compress<P::Success> + Compress<P::Failure>,
        Compressed<Success, P::Failure>: Into<Failure>,
    {
        self.and_then(|output| p.map_output(|output2| f(output, output2)))
    }

    #[expect(clippy::type_complexity)]
    pub fn and_then<P>(
        self,
        f: impl FnOnce(Output) -> P,
    ) -> Result<Source<S, P::Output, Compressed<Success, P::Success>, Failure>, P::FailFast>
    where
        S: Clone,
        P: ParseOnce<S>,
        Success: Compress<P::Success> + Compress<P::Failure>,
        Compressed<Success, P::Failure>: Into<Failure>,
    {
        let Source { source, state } = self;

        let r = match state {
            InnerState::Success(output, success) => match f(output).parse_once(source.clone())? {
                State::Success(s, output, success2) => Source {
                    source: s,
                    state: InnerState::Success(output, success.compress(success2)),
                },
                State::Failure(failure) => Source {
                    source,
                    state: InnerState::Failure(success.compress(failure).into()),
                },
            },
            InnerState::Failure(failure) => Source {
                source,
                state: InnerState::Failure(failure),
            },
        };

        Ok(r)
    }
    #[expect(clippy::type_complexity)]
    pub fn or<P>(
        self,
        p: P,
    ) -> Result<Source<S, Output, Success, Compressed<Failure, P::Failure>>, P::FailFast>
    where
        S: Clone,
        P: ParseOnce<S, Output = Output>,
        Failure: Compress<P::Success> + Compress<P::Failure>,
        Compressed<Failure, P::Success>: Into<Success>,
    {
        self.or_with(p, |t| t)
    }

    #[expect(clippy::type_complexity)]
    pub fn or_with<P>(
        self,
        p: P,
        f: impl FnOnce(P::Output) -> Output,
    ) -> Result<Source<S, Output, Success, Compressed<Failure, P::Failure>>, P::FailFast>
    where
        S: Clone,
        P: ParseOnce<S>,
        Failure: Compress<P::Success> + Compress<P::Failure>,
        Compressed<Failure, P::Success>: Into<Success>,
    {
        let Source { source, state } = self;

        let r = match state {
            InnerState::Success(output, success) => Source {
                source,
                state: InnerState::Success(output, success),
            },
            InnerState::Failure(failure) => match p.parse_once(source.clone())? {
                State::Success(s, output, success) => Source {
                    source: s,
                    state: InnerState::Success(f(output), failure.compress(success).into()),
                },
                State::Failure(failure2) => Source {
                    source,
                    state: InnerState::Failure(failure.compress(failure2)),
                },
            },
        };

        Ok(r)
    }

    pub fn finish(self) -> State<S, Output, Success, Failure> {
        let Source { source, state } = self;

        match state {
            InnerState::Success(output, success) => State::Success(source, output, success),
            InnerState::Failure(failure) => State::Failure(failure),
        }
    }
}
