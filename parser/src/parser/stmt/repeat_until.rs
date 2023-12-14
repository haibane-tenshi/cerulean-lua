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
            .map_output(Spanned::put_range)
            .and(inner_block(loop_body.new_core()), opt_discard)?
            .and(token_until, keep_range)?
            .and(expr_adjusted_to_1(loop_body.new_core()), discard)?
            .map_output(|output| {
                let ((repeat_span, until_span), output) = output.take();

                let debug_info = DebugInfo::RepeatLoop {
                    repeat: repeat_span.clone(),
                    until: until_span.clone(),
                };

                loop_body.emit_jump_to(envelope_id, Some(true), debug_info.clone());
                loop_body.emit_loop_to(debug_info.clone());
                loop_body.commit(until_span.clone());

                (until_span, output)
            })
            .map_output(|(until_span, output)| {
                envelope.commit(until_span);

                trace!(span=?output.span(), str=&source[output.span()]);

                output
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
