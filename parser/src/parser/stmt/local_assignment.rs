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

        let mut frag = core.decl();

        let stack_start = frag.stack().len();

        let state = token_local
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and(
                ident_list.map_failure(|f| ParseFailure::from(LocalAssignmentFailure::Ident(f))),
                replace,
            )?
            .with_mode(FailureMode::Malformed)
            .and(
                (|s| -> Result<_, FailFast> {
                    let state = token_equals_sign
                        .parse(s)?
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

                let count: u32 = idents.len().try_into().unwrap();

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
) -> Result<ParsingState<Lexer, Spanned<Vec<&str>>, IdentListSuccess, IdentMismatch>, LexError> {
    let (ident, state) = match identifier(s)? {
        ParsingState::Success(s, output, success) => {
            let (ident, span) = output.take();

            (ident, ParsingState::Success(s, span, success))
        }
        ParsingState::Failure(failure) => return Ok(ParsingState::Failure(failure)),
    };

    let mut r = vec![ident];

    let next = |s| -> Result<_, LexError> {
        let token_comma = match_token(Token::Comma).map_failure(|_| IdentListSuccess::Comma);

        let state = token_comma
            .parse_once(s)?
            .and(identifier.map_failure(IdentListSuccess::Ident), replace)?
            .map_output(|output| {
                let (ident, span) = output.take();
                r.push(ident);

                span
            });

        Ok(state)
    };

    let state = state
        .and(next.repeat_with(discard).optional(), opt_discard)?
        .map_output(|span| span.replace(r).1);

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
