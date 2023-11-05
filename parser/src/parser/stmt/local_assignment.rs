use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn local_assignment<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
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
        let ident_list =
            ident_list.map_failure(|f| ParseFailure::from(LocalAssignmentFailure::Ident(f)));

        let mut frag = core.decl();
        let stack_start = frag.stack().len();

        let state = Source(s)
            .and(token_local)?
            .with_mode(FailureMode::Ambiguous)
            .and(ident_list, replace)?
            .with_mode(FailureMode::Malformed)
            .and(
                (|s| -> Result<_, FailFast> {
                    let state = Source(s)
                        .and(token_equals_sign)?
                        .with_mode(FailureMode::Malformed)
                        .and(expr_list(frag.new_core()), discard)?
                        .collapse();

                    Ok(state)
                })
                .optional(),
                opt_discard,
            )?
            .map_output(|output| {
                let (idents, span) = output.take();

                let count = idents.len();

                frag.emit_adjust_to(stack_start + count);

                // Apply names.
                frag.adjust_stack_to(stack_start);
                for ident in idents {
                    frag.push_temporary(Some(ident));
                }

                frag.commit();

                span
            })
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

fn ident_list(
    s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<Vec<Ident>>, IdentListSuccess, IdentMismatch>, LexError>
{
    let mut result = Vec::new();
    let token_comma = match_token(Token::Comma).map_failure(|_| IdentListSuccess::Comma);
    let mut identifier = identifier.map_output(|output: Spanned<_>| {
        let (ident, span) = output.take();
        result.push(ident);
        span
    });

    let state = Source(s)
        .and(identifier.as_mut())?
        .and(
            (|s| {
                let state = Source(s).and(token_comma)?.and(
                    identifier.as_mut().map_failure(IdentListSuccess::Ident),
                    discard,
                )?;

                Ok(state)
            })
            .repeat_with(discard)
            .optional(),
            opt_discard,
        )?
        .map_output(|span| span.put(result));

    Ok(state)
}

#[derive(Debug)]
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
