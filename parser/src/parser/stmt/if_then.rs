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
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .and(token_then)?
            .try_map_output(|_| -> Result<_, CodegenError> {
                frag.emit_jump_to(outer, Some(false))?;
                Ok(())
            })?
            .and(block(frag.new_fragment()))?
            .and(
                (|s| {
                    else_if_clause(outer, frag.new_fragment())
                        .map_success(ElseIfFailure::Expr)
                        .parse_once(s)
                })
                .repeat(),
            )?
            .and(
                else_clause(outer, frag.new_fragment())
                    .map_success(ElseFailure::Expr)
                    .optional(),
            )?
            .and(token_end)?
            .map_output(|_| {
                frag.commit();
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum IfThenFailure {
    #[error("missing `if` token")]
    If(#[source] TokenMismatch),
    #[error("missing `then` token")]
    Then(#[source] TokenMismatch),
    #[error("missing `end` token")]
    End(#[source] TokenMismatch),
}

impl HaveFailureMode for IfThenFailure {
    fn mode(&self) -> FailureMode {
        match self {
            IfThenFailure::If(_) => FailureMode::Mismatch,
            IfThenFailure::Then(_) => FailureMode::Malformed,
            IfThenFailure::End(_) => FailureMode::Malformed,
        }
    }
}

fn else_if_clause<'s, 'origin>(
    outer: FragmentId,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = CompleteOr<ParseFailure>,
    Failure = ElseIfFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;
        use crate::parser::expr::expr_adjusted_to_1;

        let token_elseif = match_token(Token::ElseIf).map_failure(|_| ElseIfFailure::ElseIf);
        let token_then = match_token(Token::Then)
            .map_failure(|f| ElseIfFailure::Expr(CompleteOr::Other(IfThenFailure::Then(f).into())));

        // Emit jump from end of previous block to end of if-then statement since we didn't reach the end yet.
        frag.emit_jump_to(outer, None)?;

        let state = token_elseif
            .parse_once(s)?
            .and(expr_adjusted_to_1(frag.new_fragment()))?
            .and(token_then)?
            .try_map_output(|_| frag.emit_jump_to(frag.id(), Some(false)))?
            .and(block(frag.new_fragment()))?;

        let state = state.map_output(|_| {
            frag.commit();
        });

        Ok(state)
    }
}

enum ElseIfFailure {
    ElseIf,
    Expr(CompleteOr<ParseFailure>),
}

impl From<Never> for ElseIfFailure {
    fn from(value: Never) -> Self {
        match value {}
    }
}

impl From<ParseFailure> for ElseIfFailure {
    fn from(value: ParseFailure) -> Self {
        ElseIfFailure::Expr(CompleteOr::Other(value))
    }
}

impl Combine<Never> for ElseIfFailure {
    type Output = Never;

    fn combine(self, other: Never) -> Self::Output {
        match other {}
    }
}

impl Combine<ElseIfFailure> for ElseIfFailure {
    type Output = Self;

    fn combine(self, other: ElseIfFailure) -> Self::Output {
        match (self, other) {
            (r, ElseIfFailure::ElseIf) => r,
            (ElseIfFailure::Expr(f0), ElseIfFailure::Expr(f1)) => {
                ElseIfFailure::Expr(f0.combine(f1))
            }
            (_, r) => r,
        }
    }
}

impl Combine<ElseIfFailure> for ParseFailure {
    type Output = ParseFailure;

    fn combine(self, other: ElseIfFailure) -> Self::Output {
        match other {
            ElseIfFailure::ElseIf => self,
            ElseIfFailure::Expr(failure) => self.combine(failure),
        }
    }
}

impl Combine<ElseFailure> for ElseIfFailure {
    type Output = ElseFailure;

    fn combine(self, other: ElseFailure) -> Self::Output {
        match other {
            ElseFailure::Else => ElseFailure::Else,
            ElseFailure::Expr(failure0) => match self {
                ElseIfFailure::ElseIf => ElseFailure::Expr(failure0),
                ElseIfFailure::Expr(failure1) => ElseFailure::Expr(failure0.combine(failure1)),
            },
        }
    }
}

fn else_clause<'s, 'origin>(
    outer: FragmentId,
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = CompleteOr<ParseFailure>,
    Failure = ElseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;

        let token_else = match_token(Token::Else).map_failure(|_| ElseFailure::Else);

        // Emit jump from end of previous block to end of if-then statement since we didn't reach the end yet.
        frag.emit_jump_to(outer, None)?;

        let state = token_else
            .parse_once(s)?
            .and(block(frag))?
            .map_output(|_| ());

        Ok(state)
    }
}

enum ElseFailure {
    Else,
    Expr(CompleteOr<ParseFailure>),
}

impl From<Never> for ElseFailure {
    fn from(value: Never) -> Self {
        match value {}
    }
}

impl Combine<ParseFailure> for ElseFailure {
    type Output = ParseFailure;

    fn combine(self, other: ParseFailure) -> Self::Output {
        match self {
            ElseFailure::Else => other,
            ElseFailure::Expr(f) => f.combine(other),
        }
    }
}
