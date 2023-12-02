use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn repeat_until<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
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

        let mut envelope = core.scope();
        let envelope_id = envelope.id();

        let source = s.source();
        let _span = trace_span!("repeat_until").entered();

        let mut loop_body = envelope.new_scope();
        loop_body.mark_as_loop();

        let state = Source(s)
            .and(token_repeat)?
            .with_mode(FailureMode::Malformed)
            .and(inner_block(loop_body.new_core()), opt_discard)?
            .and(token_until, replace_range)?
            .and(expr_adjusted_to_1(loop_body.new_core()), discard)?
            .inspect(|output| {
                let until_span = output.value.clone();
                loop_body.emit_jump_to(envelope_id, Some(true), until_span.clone());
                loop_body.emit_loop_to(until_span.clone());
                loop_body.commit(until_span.clone());
            })
            .map_output(|output| {
                let (until_span, span) = output.take();
                envelope.commit(until_span);

                trace!(span=?span.span(), str=&source[span.span()]);

                span
            })
            .collapse();

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
