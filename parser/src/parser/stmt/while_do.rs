use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn while_do<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;
        use crate::parser::expr::expr_adjusted_to_1;

        let token_while =
            match_token(Token::While).map_failure(|f| ParseFailure::from(WhileDoFailure::While(f)));
        let token_do =
            match_token(Token::Do).map_failure(|f| ParseFailure::from(WhileDoFailure::Do(f)));
        let token_end =
            match_token(Token::End).map_failure(|f| ParseFailure::from(WhileDoFailure::End(f)));

        let mut outer_frag = core.scope();

        let state = token_while
            .parse(s)?
            .with_mode(FailureMode::Malformed)
            .map_output(|_| {
                let mut frag = outer_frag.new_scope();
                frag.mark_as_loop();

                frag
            })
            .then(|mut frag| {
                move |s| -> Result<_, FailFast> {
                    Ok(expr_adjusted_to_1(frag.new_core())
                        .parse_once(s)?
                        .map_output(|_| frag))
                }
            })?
            .and_discard(token_do)?
            .map_output(|mut frag| {
                frag.emit_jump_to(frag.id(), Some(false));
                frag
            })
            .then(|mut frag| {
                move |s| -> Result<_, FailFast> {
                    Ok(block(frag.new_core()).parse_once(s)?.map_output(|_| frag))
                }
            })?
            .and_discard(token_end)?
            .map_output(|mut frag| {
                frag.emit_loop_to();
                frag.commit();
            })
            .collapse();

        let state = state.map_output(|_| outer_frag.commit());

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum WhileDoFailure {
    #[error("missing `while` keyword")]
    While(#[source] TokenMismatch),
    #[error("missing `do` keyword")]
    Do(#[source] TokenMismatch),
    #[error("missing `end` keyword")]
    End(#[source] TokenMismatch),
}
