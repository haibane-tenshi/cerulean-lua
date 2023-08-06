use crate::codegen::fragment::FragmentId;
use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn if_then<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
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

        let token_if =
            match_token(Token::If).map_failure(|f| ParseFailure::from(IfThenFailure::If(f)));
        let token_then =
            match_token(Token::Then).map_failure(|f| ParseFailure::from(IfThenFailure::Then(f)));
        let token_end =
            match_token(Token::End).map_failure(|f| ParseFailure::from(IfThenFailure::End(f)));

        let outer = frag.id();

        let state = token_if
            .parse_once(s)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .and(token_then)?
            .try_map_output(|_| -> Result<_, CodegenError> {
                frag.emit_jump_to(outer, Some(false))?;
                Ok(())
            })?
            .and(block(frag.new_fragment()))?
            .and((|s| else_if_clause(outer, frag.new_fragment()).parse_once(s)).repeat())?
            .and(else_clause(outer, frag.new_fragment()).optional())?
            .and(token_end)?
            .map_output(|_| {
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
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
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

        // Emit jump from end of previous block to end of if-then statement since we didn't reach the end yet.
        frag.emit_jump_to(outer, None)?;

        let state = token_elseif
            .parse_once(s)?
            .with_mode(FailureMode::Malformed)
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .and(token_then)?
            .try_map_output(|_| frag.emit_jump_to(frag.id(), Some(false)))?
            .and(block(frag.new_fragment()))?
            .collapse();

        let state = state.map_output(|_| {
            frag.commit();
        });

        Ok(state)
    }
}

fn else_clause<'s, 'origin>(
    outer: FragmentId,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = CompleteOr<ParseFailure>,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;

        let token_else =
            match_token(Token::Else).map_failure(|f| ParseFailure::from(IfThenFailure::Else(f)));

        // Emit jump from end of previous block to end of if-then statement since we didn't reach the end yet.
        frag.emit_jump_to(outer, None)?;

        let state = token_else
            .parse_once(s)?
            .with_mode(FailureMode::Malformed)
            .and(block(frag))?
            .map_output(|_| ())
            .collapse();

        Ok(state)
    }
}
