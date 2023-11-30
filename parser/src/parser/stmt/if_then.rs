use crate::codegen::fragment::FragmentId;
use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn if_then<'s, 'origin>(
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

        let token_if =
            match_token(Token::If).map_failure(|f| ParseFailure::from(IfThenFailure::If(f)));
        let token_then =
            match_token(Token::Then).map_failure(|f| ParseFailure::from(IfThenFailure::Then(f)));
        let token_end =
            match_token(Token::End).map_failure(|f| ParseFailure::from(IfThenFailure::End(f)));

        let mut envelope = core.scope();
        let envelope_id = envelope.id();

        let source = s.source();
        let _span = trace_span!("if_then").entered();

        let state = Source(s)
            .and(token_if)?
            .with_mode(FailureMode::Malformed)
            .and(
                |s| -> Result<_, FailFast> {
                    let mut frag = envelope.new_scope();

                    let state = Source(s)
                        .and(expr_adjusted_to_1(frag.new_core()))?
                        .and(token_then, discard)?
                        .inspect(|_| {
                            frag.emit_jump_to_end(Some(false));
                        })
                        .and(block(frag.new_core()), opt_discard)?
                        .inspect(|_| {
                            frag.emit_jump_to(envelope_id, None);
                            frag.commit();
                        });

                    Ok(state)
                },
                discard,
            )?
            .and(
                (|s| else_if_clause(envelope_id, envelope.new_core()).parse_once(s))
                    .repeat_with(discard)
                    .optional(),
                opt_discard,
            )?
            .and(else_clause(envelope.new_core()).optional(), opt_discard)?
            .and(token_end, discard)?
            .inspect(|output| {
                envelope.commit();

                trace!(span=?output.span(), str=&source[output.span()]);
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum IfThenFailure {
    #[error("missing `if` token")]
    If(#[source] TokenMismatch),
    #[error("missing `then` token")]
    Then(#[source] TokenMismatch),
    #[error("missing `elseif` token")]
    ElseIf(#[source] TokenMismatch),
    #[error("missing `else` token")]
    Else(#[source] TokenMismatch),
    #[error("missing `end` token")]
    End(#[source] TokenMismatch),
}

fn else_if_clause<'s, 'origin>(
    envelope: FragmentId,
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = CompleteOr<ParseFailure>,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;
        use crate::parser::expr::expr_adjusted_to_1;

        let token_elseif = match_token(Token::ElseIf)
            .map_failure(|f| ParseFailure::from(IfThenFailure::ElseIf(f)));
        let token_then =
            match_token(Token::Then).map_failure(|f| ParseFailure::from(IfThenFailure::Then(f)));

        let mut frag = core.scope();

        let state = Source(s)
            .and(token_elseif)?
            .with_mode(FailureMode::Malformed)
            .and(
                |s| -> Result<_, FailFast> {
                    let mut frag = frag.new_scope();

                    let state = Source(s)
                        .and(expr_adjusted_to_1(frag.new_core()))?
                        .and(token_then, discard)?
                        .inspect(|_| {
                            frag.emit_jump_to_end(Some(false));
                        })
                        .and(block(frag.new_core()), opt_discard)?
                        .inspect(|_| {
                            frag.emit_jump_to(envelope, None);
                            frag.commit();
                        });

                    Ok(state)
                },
                discard,
            )?
            .collapse()
            .inspect(|_| frag.commit());

        Ok(state)
    }
}

fn else_clause<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = CompleteOr<ParseFailure>,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;

        let token_else =
            match_token(Token::Else).map_failure(|f| ParseFailure::from(IfThenFailure::Else(f)));

        let state = Source(s)
            .and(token_else)?
            .with_mode(FailureMode::Malformed)
            .and(block(core), opt_discard)?
            .collapse();

        Ok(state)
    }
}
