use crate::codegen::stack::StackView;
use crate::parser::prelude::*;
use repr::index::FunctionId;
use thiserror::Error;

pub(crate) fn func_body<'s, 'origin>(
    mut outer_frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = FunctionId,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::codegen::function::Function;
        use crate::parser::block::block;
        use FuncBodyFailure::*;

        let token_par_l = match_token(Token::ParL).map_failure(|f| ParseFailure::from(ParL(f)));
        let token_par_r = match_token(Token::ParR).map_failure(|f| ParseFailure::from(ParR(f)));
        let token_end = match_token(Token::End).map_failure(|f| ParseFailure::from(End(f)));

        // Start function
        let mut func = Function::new();
        let mut frag = outer_frag.new_function(func.view());

        // Currently this slot contains pointer to function itself.
        // In the future we will put environment here instead.
        frag.stack_mut().push();

        let state = token_par_l
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and_replace(parlist(frag.stack_mut().new_block()).optional())?
            .map_success(|success| success.map(|f| ParseFailure::from(FuncBodyFailure::from(f))))
            .with_mode(FailureMode::Malformed)
            .and_discard(token_par_r)?
            .and_discard(block(frag.new_fragment()))?
            .and_discard(token_end)?
            .inspect(|_| {
                // Cannot capture both `frag` and `func` in the same closure.
                frag.commit_scope();
            })
            .try_map_output(move |param_count| -> Result<_, CodegenError> {
                let (param_count, is_variadic) = param_count.unwrap_or((0, false));

                // An extra stack slot is taken by function pointer itself.
                let height = param_count + 1;

                // Finish function.
                // frag.commit_scope();
                let func = func.resolve(height, is_variadic);
                let func_id = outer_frag.func_table_mut().push(func)?;

                // Drop outer fragment to make sure we didn't mess up current function.
                drop(outer_frag);

                Ok(func_id)
            })?
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

fn parlist<'s, 'origin>(
    mut stack: StackView<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (u32, bool),
    Success = CompleteOr<ParListMismatch>,
    Failure = ParListMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut count = 0;

        let token_comma = match_token(Token::Comma).map_failure(ParListMismatch::Comma);
        let token_variadic = match_token(Token::TripleDot).map_failure(ParListMismatch::Variadic);
        let mut ident = identifier
            .map_failure(ParListMismatch::Ident)
            .map_output(|(ident, _)| {
                let slot = stack.push();
                stack.give_name(slot, ident);
                count += 1;
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
                stack.commit(crate::codegen::stack::CommitKind::Decl);

                (count, is_variadic)
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
