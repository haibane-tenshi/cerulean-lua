use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn break_<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let token_break =
            match_token(Token::Break).map_failure(|f| ParseFailure::from(BreakFailure::Break(f)));

        let mut frag = core.decl();

        let state = token_break
            .parse(s)?
            .try_map_output(|_| -> Result<_, CodegenError> {
                frag.emit_break()?;
                frag.commit();

                Ok(())
            })?;

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum BreakFailure {
    #[error("missing `break` keyword")]
    Break(TokenMismatch),
}
