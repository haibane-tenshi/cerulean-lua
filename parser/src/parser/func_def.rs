use crate::parser::prelude::*;
use repr::chunk::Signature;
use repr::index::FunctionId;
use thiserror::Error;

pub(crate) fn func_body<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = FunctionId,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;
        use FuncBodyFailure::*;

        let token_par_l = match_token(Token::ParL).map_failure(|f| ParseFailure::from(ParL(f)));
        let token_par_r = match_token(Token::ParR).map_failure(|f| ParseFailure::from(ParR(f)));
        let token_end = match_token(Token::End).map_failure(|f| ParseFailure::from(End(f)));

        let mut outer_frag = core.scope();

        let state = token_par_l
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and_replace(parlist().optional())?
            .map_success(|success| success.map(|f| ParseFailure::from(FuncBodyFailure::from(f))))
            .with_mode(FailureMode::Malformed)
            .and_discard(token_par_r)?
            .then(|signature| {
                let outer_frag = &mut outer_frag;
                move |s: Lexer<'s>| -> Result<_, FailFast> {
                    let (idents, mut signature) = signature.unwrap_or_default();
                    // At the moment extra slot is occupied by the function pointer itself.
                    signature.height += 1;

                    let mut frame = outer_frag.new_core().frame(signature);
                    let mut frag = frame.new_core().scope();

                    frag.stack_mut().push(None);

                    for ident in idents {
                        frag.stack_mut().push(Some(ident));
                    }

                    let state = block(frag.new_core())
                        .parse_once(s)?
                        .map_output(|_| frag.commit())
                        .try_map_output(|_| frame.commit())?
                        .map_output(|fun| fun.resolve());

                    Ok(state)
                }
            })?
            .and_discard(token_end)?
            .map_output(|func| {
                let r = outer_frag.func_table_mut().push(func).unwrap();
                outer_frag.commit();

                r
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum FuncBodyFailure {
    #[error("missing opening parenthesis")]
    ParL(#[source] TokenMismatch),
    #[error("missing comma between arguments")]
    ArgListComma(#[source] TokenMismatch),
    #[error("function argument requires to be a valid identifier")]
    ArgListIdent(#[source] IdentMismatch),
    #[error("missing variadic arguments")]
    ArgListVariadic(#[source] TokenMismatch),
    #[error("missing closing parenthesis")]
    ParR(#[source] TokenMismatch),
    #[error("missing `end` token")]
    End(#[source] TokenMismatch),
}

impl From<ParListMismatch> for FuncBodyFailure {
    fn from(value: ParListMismatch) -> Self {
        match value {
            ParListMismatch::Comma(err) => FuncBodyFailure::ArgListComma(err),
            ParListMismatch::Ident(err) => FuncBodyFailure::ArgListIdent(err),
            ParListMismatch::Variadic(err) => FuncBodyFailure::ArgListVariadic(err),
        }
    }
}

fn parlist<'s, 'origin>() -> impl ParseOnce<
    Lexer<'s>,
    Output = (Vec<&'s str>, Signature),
    Success = CompleteOr<ParListMismatch>,
    Failure = ParListMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut idents = Vec::new();

        let token_comma = match_token(Token::Comma).map_failure(ParListMismatch::Comma);
        let token_variadic = match_token(Token::TripleDot).map_failure(ParListMismatch::Variadic);
        let mut ident = identifier
            .map_failure(ParListMismatch::Ident)
            .map_output(|(ident, _)| {
                idents.push(ident);
            });

        let state = ident
            .parse_mut(s.clone())?
            .and(
                (|s: Lexer<'s>| -> Result<_, FailFast> {
                    let state = token_comma.parse(s)?.and(ident.as_mut())?;

                    Ok(state)
                })
                .repeat(),
            )?
            .and_replace(
                (|s: Lexer<'s>| -> Result<_, FailFast> {
                    let state = token_comma
                        .parse(s)?
                        .and(token_variadic)?
                        .map_output(|_| ());

                    Ok(state)
                })
                .map_success(CompleteOr::Complete)
                .optional(),
            )?
            .map_output(|t: Option<_>| t.is_some())
            .or_else(|| (s, token_variadic.map_output(|_| true)))?
            .map_output(|is_variadic| {
                let height = idents.len().try_into().unwrap();

                let signature = Signature {
                    height,
                    is_variadic,
                };

                (idents, signature)
            });

        Ok(state)
    }
}

enum ParListMismatch {
    Comma(TokenMismatch),
    Ident(IdentMismatch),
    Variadic(TokenMismatch),
}

impl From<Never> for ParListMismatch {
    fn from(value: Never) -> Self {
        match value {}
    }
}

impl Arrow<ParListMismatch> for ParListMismatch {
    type Output = Self;

    fn arrow(self, _: ParListMismatch) -> Self::Output {
        self
    }
}

impl Arrow<CompleteOr<ParListMismatch>> for ParListMismatch {
    type Output = CompleteOr<ParListMismatch>;

    fn arrow(self, other: CompleteOr<ParListMismatch>) -> Self::Output {
        other.map(|_| self)
    }
}

impl Arrow<ParListMismatch> for Complete {
    type Output = ParListMismatch;

    fn arrow(self, other: ParListMismatch) -> Self::Output {
        other
    }
}

impl From<ParListMismatch> for CompleteOr<ParListMismatch> {
    fn from(value: ParListMismatch) -> Self {
        CompleteOr::Other(value)
    }
}
