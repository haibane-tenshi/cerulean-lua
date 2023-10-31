use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn do_end<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;

        let mut frag = core.scope();

        let token_do =
            match_token(Token::Do).map_failure(|f| ParseFailure::from(DoEndFailure::Do(f)));
        let token_end =
            match_token(Token::End).map_failure(|f| ParseFailure::from(DoEndFailure::End(f)));

        let state = token_do
            .parse_once(s)?
            .with_mode(FailureMode::Malformed)
            .and(block(frag.new_core()), opt_discard)?
            .and(token_end, discard)?
            .inspect(move |_| {
                frag.commit();
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum DoEndFailure {
    #[error("missing `do` keyword")]
    Do(#[source] TokenMismatch),
    #[error("missing `end` keyword")]
    End(#[source] TokenMismatch),
}
