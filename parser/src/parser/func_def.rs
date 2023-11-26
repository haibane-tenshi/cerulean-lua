use crate::codegen::function::Signature;
use crate::parser::prelude::*;
use repr::index::RecipeId;
use thiserror::Error;

pub(crate) fn func_body<'s, 'origin>(
    core: Core<'s, 'origin>,
    self_arg: bool,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<RecipeId>,
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

        let mut envelope = core.scope();

        let source = s.source();
        let _span = trace_span!("fn_body").entered();

        let state = Source(s)
            .and(token_par_l)?
            .with_mode(FailureMode::Ambiguous)
            .and(arg_list().optional(), opt_replace)?
            .map_success(|success| success.map(|f| ParseFailure::from(FuncBodyFailure::from(f))))
            .with_mode(FailureMode::Malformed)
            .and(token_par_r, discard)?
            .then(|output| {
                let envelope = &mut envelope;
                move |s: Lexer<'s>| -> Result<_, FailFast> {
                    let (signature, span) = output.take();
                    let (idents, mut signature) = signature.unwrap_or_default();
                    if self_arg {
                        signature.height += 1;
                    }

                    let mut frame = envelope.new_core().frame(signature);
                    let mut frag = frame.new_core().scope();

                    if self_arg {
                        frag.push_temporary(Some(Ident::self_()));
                    }

                    for ident in idents {
                        frag.push_temporary(Some(ident));
                    }

                    let state = block(frag.new_core())
                        .parse_once(s)?
                        .inspect(|_| frag.commit())
                        .try_map_output(|output| -> Result<_, CodegenError> {
                            let span = opt_discard(span, output);
                            let (func, upvalues) = frame.commit()?;
                            Ok((func, upvalues, span))
                        })?
                        .map_output(|(func, upvalues, span)| {
                            let upvalues = upvalues
                                .resolve()
                                .into_iter()
                                .map(|ident| envelope.capture_variable(ident).unwrap().into())
                                .collect::<repr::tivec::TiVec<_, _>>();
                            let func = func.resolve(upvalues.len());
                            span.put((func, upvalues))
                        });

                    Ok(state)
                }
            })?
            .and(token_end, discard)?
            .map_output(|output| {
                use repr::chunk::ClosureRecipe;

                let ((func, upvalues), span) = output.take();
                let function_id = envelope.func_table_mut().push(func);
                let closure = ClosureRecipe {
                    function_id,
                    upvalues,
                };
                let recipe_id = envelope.recipe_table_mut().push(closure);
                envelope.commit();

                trace!(?function_id, ?recipe_id, "new function");
                trace!(span=?span.span(), str=&source[span.span()]);

                span.put(recipe_id)
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

fn arg_list<'s, 'origin>() -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<(Vec<Ident<'s>>, Signature)>,
    Success = CompleteOr<ParListMismatch>,
    Failure = ParListMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut idents = Vec::new();

        let token_comma = match_token(Token::Comma).map_failure(ParListMismatch::Comma);
        let token_variadic = match_token(Token::TripleDot).map_failure(ParListMismatch::Variadic);
        let mut ident = identifier
            .map_output(|output: Spanned<_>| {
                let (ident, span) = output.take();
                idents.push(ident);
                span
            })
            .map_failure(ParListMismatch::Ident);

        let state = Source(s)
            .or(|s: Lexer<'s>| -> Result<_, FailFast> {
                let state = Source(s)
                    .and(ident.as_mut())?
                    .and(
                        (|s: Lexer<'s>| -> Result<_, FailFast> {
                            let state = Source(s).and(token_comma)?.and(ident.as_mut(), discard)?;

                            Ok(state)
                        })
                        .repeat_with(discard)
                        .optional(),
                        opt_discard,
                    )?
                    .and(
                        (|s: Lexer<'s>| -> Result<_, FailFast> {
                            let state = Source(s).and(token_comma)?.and(token_variadic, discard)?;

                            Ok(state)
                        })
                        .map_success(CompleteOr::Complete)
                        .optional(),
                        opt_replace,
                    )?
                    .map_output(|output| output.map(|t| t.is_some()));

                Ok(state)
            })?
            .or(token_variadic.map_output(|output: Spanned<_>| output.map(|_| true)))?
            .map_fsource(|_| ())
            .map_output(|output| {
                let (is_variadic, span) = output.take();
                let height = idents.len();

                let signature = Signature {
                    height,
                    is_variadic,
                };

                span.put((idents, signature))
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
