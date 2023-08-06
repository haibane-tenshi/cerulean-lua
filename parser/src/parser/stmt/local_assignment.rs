use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn local_assignment<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailure,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_list;

        let token_local = match_token(Token::Local)
            .map_failure(|f| ParseFailure::from(LocalAssignmentFailure::Local(f)));
        let token_equals_sign = match_token(Token::EqualsSign)
            .map_failure(|f| ParseFailure::from(LocalAssignmentFailure::EqualsSign(f)));

        let stack_start = frag.stack().top()?;

        let state = token_local
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and_with(
                ident_list.map_failure(|f| ParseFailure::from(LocalAssignmentFailure::Ident(f))),
                |_, idents| idents,
            )?
            .with_mode(FailureMode::Malformed)
            .and_discard(token_equals_sign)?
            .and_discard(expr_list(frag.new_fragment()))?
            .try_map_output(|idents| -> Result<_, CodegenError> {
                let count: u32 = idents.len().try_into().unwrap();
                frag.emit_adjust_to(stack_start + count)?;

                for (ident, slot) in idents.into_iter().zip((stack_start.0..).map(StackSlot)) {
                    frag.stack_mut().give_name(slot, ident)?;
                }

                frag.commit();
                Ok(())
            })?
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub enum LocalAssignmentFailure {
    #[error("missing `local` keyword")]
    Local(#[source] TokenMismatch),
    #[error("assignment list should have at least one identifier")]
    Ident(#[source] IdentMismatch),
    #[error("missing equals sign")]
    EqualsSign(#[source] TokenMismatch),
}

impl HaveFailureMode for LocalAssignmentFailure {
    fn mode(&self) -> FailureMode {
        match self {
            LocalAssignmentFailure::Local(_) => FailureMode::Mismatch,
            LocalAssignmentFailure::Ident(_) => FailureMode::Ambiguous,
            LocalAssignmentFailure::EqualsSign(_) => FailureMode::Ambiguous,
        }
    }
}

fn ident_list(
    s: Lexer,
) -> Result<ParsingState<Lexer, Vec<&str>, IdentListSuccess, IdentMismatch>, LexError> {
    let (ident, state) = match identifier(s)? {
        ParsingState::Success(s, (ident, _), success) => {
            (ident, ParsingState::Success(s, (), success))
        }
        ParsingState::Failure(failure) => return Ok(ParsingState::Failure(failure)),
    };

    let mut output = vec![ident];

    let next = |s| -> Result<_, LexError> {
        let token_comma = match_token(Token::Comma).map_failure(|_| IdentListSuccess::Comma);

        let state = token_comma
            .parse_once(s)?
            .and(identifier.map_failure(IdentListSuccess::Ident))?
            .map_output(|(_, (ident, _))| {
                output.push(ident);
            });

        Ok(state)
    };

    let state = state.and(next.repeat())?.map_output(|_| output);

    Ok(state)
}

enum IdentListSuccess {
    Comma,
    Ident(IdentMismatch),
}

impl Arrow<ParseFailure> for IdentListSuccess {
    type Output = ParseFailure;

    fn arrow(self, other: ParseFailure) -> Self::Output {
        match self {
            IdentListSuccess::Comma => other,
            IdentListSuccess::Ident(f) => {
                ParseFailure::from(LocalAssignmentFailure::Ident(f)).arrow(other)
            }
        }
    }
}

impl Arrow<IdentListSuccess> for Complete {
    type Output = IdentListSuccess;

    fn arrow(self, other: IdentListSuccess) -> Self::Output {
        other
    }
}
