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

        let mut envelope = core.scope();
        let envelope_id = envelope.id();

        let source = s.source();
        let _span = trace_span!("while_do").entered();

        let mut loop_body = envelope.new_scope();
        loop_body.mark_as_loop();

        let state = Source(s)
            .and(token_while)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(loop_body.new_core()), discard)?
            .and(token_do, discard)?
            .inspect(|_| {
                loop_body.emit_jump_to(envelope_id, Some(false));
            })
            .and(block(loop_body.new_core()), opt_discard)?
            .and(token_end, discard)?
            .inspect(|_| {
                loop_body.emit_loop_to();
                loop_body.commit();
            })
            .inspect(|output| {
                envelope.commit();

                trace!(span=?output.span(), str=&source[output.span()]);
            })
            .collapse();

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
