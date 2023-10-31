use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn while_do<'s, 'origin>(
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
            .map_output(|span| {
                let mut frag = outer_frag.new_scope();
                frag.mark_as_loop();

                (frag, span)
            })
            .then(|(mut frag, span)| {
                move |s| -> Result<_, FailFast> {
                    Ok(expr_adjusted_to_1(frag.new_core())
                        .parse_once(s)?
                        .map_output(|output| discard(span, output).replace(frag).1))
                }
            })?
            .and(token_do, discard)?
            .map_output(|mut output| {
                let frag = &mut output.value;
                frag.emit_jump_to(frag.id(), Some(false));
                output
            })
            .then(|output| {
                move |s| -> Result<_, FailFast> {
                    let (mut frag, span) = output.take();

                    Ok(block(frag.new_core())
                        .parse_once(s)?
                        .map_output(|output| opt_discard(span, output).put(frag)))
                }
            })?
            .and(token_end, discard)?
            .map_output(|output| {
                let (mut frag, span) = output.take();
                frag.emit_loop_to();
                frag.commit();

                span
            })
            .collapse();

        let state = state.inspect(|_| outer_frag.commit());

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
