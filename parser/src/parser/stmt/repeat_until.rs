use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn repeat_until<'s, 'origin>(
    mut outer_frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::inner_block;
        use crate::parser::expr::expr_adjusted_to_1;

        let token_repeat = match_token(Token::Repeat)
            .map_failure(|f| ParseFailure::from(RepeatUntilFailure::Repeat(f)));
        let token_until = match_token(Token::Until)
            .map_failure(|f| ParseFailure::from(RepeatUntilFailure::Until(f)));

        let state = token_repeat
            .parse(s)?
            .with_mode(FailureMode::Malformed)
            .map_output(|_| outer_frag.new_fragment())
            .then(|mut frag| {
                |s| -> Result<_, FailFast> {
                    let state = inner_block(frag.new_fragment())
                        .parse_once(s)?
                        .map_output(|_| frag);
                    Ok(state)
                }
            })?
            .and_discard(token_until)?
            .then(|mut frag| {
                |s| -> Result<_, FailFast> {
                    let state = expr_adjusted_to_1(frag.new_fragment())
                        .parse_once(s)?
                        .map_output(|_| frag);
                    Ok(state)
                }
            })?
            .try_map_output(|mut frag| -> Result<_, CodegenError> {
                frag.emit_jump_to(frag.id(), Some(true))?;
                frag.emit_loop_to()?;
                frag.commit_scope();
                Ok(())
            })?
            .collapse();

        let state = state.map_output(|_| outer_frag.commit());

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum RepeatUntilFailure {
    #[error("missing `repeat` keyword")]
    Repeat(#[source] TokenMismatch),
    #[error("missing `until` keyword")]
    Until(#[source] TokenMismatch),
}
