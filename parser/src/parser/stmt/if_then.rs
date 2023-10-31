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

        let mut frag = core.scope();

        let outer = frag.id();

        let state = token_if
            .parse_once(s)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .and(token_then, discard)?
            .and(
                |s| -> Result<_, FailFast> {
                    use crate::codegen::stack::CommitKind;
                    let mut frag = frag.new_fragment_at_boundary(CommitKind::Scope);

                    frag.emit_jump_to(frag.id(), Some(false));

                    let state = block(frag.new_core()).parse_once(s)?;

                    frag.emit_jump_to(outer, None);

                    frag.commit();
                    Ok(state)
                },
                opt_discard,
            )?
            .and(
                (|s| else_if_clause(outer, frag.new_core()).parse_once(s))
                    .repeat_with(discard)
                    .optional(),
                opt_discard,
            )?
            .and(else_clause(frag.new_core()).optional(), opt_discard)?
            .and(token_end, discard)?
            .inspect(|_| {
                frag.commit();
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
    outer: FragmentId,
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

        let state = token_elseif
            .parse_once(s)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(frag.new_core()), discard)?
            .and(token_then, discard)?
            .and(
                |s| -> Result<_, FailFast> {
                    use crate::codegen::stack::CommitKind;
                    let mut frag = frag.new_fragment_at_boundary(CommitKind::Scope);

                    frag.emit_jump_to(frag.id(), Some(false));

                    let state = block(frag.new_core()).parse_once(s)?;

                    frag.emit_jump_to(outer, None);

                    frag.commit();
                    Ok(state)
                },
                opt_discard,
            )?
            .collapse();

        let state = state.inspect(|_| {
            frag.commit();
        });

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

        let state = token_else
            .parse_once(s)?
            .with_mode(FailureMode::Malformed)
            .and(block(core), opt_discard)?
            .collapse();

        Ok(state)
    }
}
