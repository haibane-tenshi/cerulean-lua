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
        use FuncDefFailure::*;

        let token_par_l = match_token(Token::ParL).map_failure(|f| ParseFailure::from(ParL(f)));
        let token_par_r = match_token(Token::ParR).map_failure(|f| ParseFailure::from(ParR(f)));
        let token_end = match_token(Token::End).map_failure(|f| ParseFailure::from(End(f)));

        // Start function
        let mut func = Function::new();
        let mut frag = outer_frag.new_function(func.view());

        // Currently this slot contains pointer to function itself.
        // In the future we will put environment here instead.
        frag.stack_mut().push()?;

        let state = token_par_l
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and_replace(parlist(frag.stack_mut().new_block()).optional())?
            .map_success(FuncDefFailure::from)
            .map_success(ParseFailure::from)
            .with_mode(FailureMode::Malformed)
            .and_discard(token_par_r)?
            .and_discard(block(frag.new_fragment()))?
            .and_discard(token_end)?
            .inspect(|_| {
                // Cannot capture both `frag` and `func` in the same closure.
                frag.commit_scope();
            })
            .try_map_output(move |param_count| -> Result<_, CodegenError> {
                let param_count = param_count.unwrap_or(0);

                // An extra stack slot is taken by function pointer itself.
                let height = param_count + 1;

                // Finish function.
                // frag.commit_scope();
                let func = func.resolve(height);
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
pub(crate) enum FuncDefFailure {
    #[error("missing opening parenthesis")]
    ParL(#[source] TokenMismatch),
    #[error("missing comma between arguments")]
    ArgListComma(#[source] TokenMismatch),
    #[error("function argument requires to be a valid identifier")]
    ArgListIdent(#[source] IdentMismatch),
    #[error("missing closing parenthesis")]
    ParR(#[source] TokenMismatch),
    #[error("missing `end` token")]
    End(#[source] TokenMismatch),
}

impl HaveFailureMode for FuncDefFailure {
    fn mode(&self) -> FailureMode {
        match self {
            FuncDefFailure::ParL(_) => FailureMode::Mismatch,
            FuncDefFailure::ArgListComma(_) => FailureMode::Malformed,
            FuncDefFailure::ArgListIdent(_) => FailureMode::Malformed,
            FuncDefFailure::ParR(_) => FailureMode::Malformed,
            FuncDefFailure::End(_) => FailureMode::Malformed,
        }
    }
}

impl From<ParListMismatch> for FuncDefFailure {
    fn from(value: ParListMismatch) -> Self {
        match value {
            ParListMismatch::Comma(err) => FuncDefFailure::ArgListComma(err),
            ParListMismatch::Ident(err) => FuncDefFailure::ArgListIdent(err),
        }
    }
}

fn parlist<'s, 'origin>(
    mut stack: StackView<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = u32,
    Success = ParListMismatch,
    Failure = ParListMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use ParListMismatch::*;

        let mut count = 0;

        let state = identifier(s)?.map_failure(Ident).try_map_output(
            |(ident, _)| -> Result<_, CodegenError> {
                let slot = stack.push()?;
                stack.give_name(slot, ident)?;
                count += 1;

                Ok(())
            },
        )?;

        let next = |s: Lexer<'s>| -> Result<_, FailFast> {
            let r = match_token(Token::Comma)
                .parse(s)?
                .map_failure(Comma)
                .and(identifier.map_failure(Ident))?
                .try_map_output(|(_, (ident, _))| -> Result<_, CodegenError> {
                    let slot = stack.push()?;
                    stack.give_name(slot, ident)?;
                    count += 1;

                    Ok(())
                })?;

            Ok(r)
        };

        let r = state.and(next.repeat())?.map_output(|_| {
            stack.commit(true);
            count
        });

        Ok(r)
    }
}

enum ParListMismatch {
    Comma(TokenMismatch),
    Ident(IdentMismatch),
}

impl HaveFailureMode for ParListMismatch {
    fn mode(&self) -> FailureMode {
        use ParListMismatch::*;

        match self {
            Comma(_) => FailureMode::Mismatch,
            Ident(_) => FailureMode::Malformed,
        }
    }
}

impl From<Never> for ParListMismatch {
    fn from(value: Never) -> Self {
        match value {}
    }
}

impl Arrow<ParListMismatch> for Complete {
    type Output = ParListMismatch;

    fn arrow(self, other: ParListMismatch) -> Self::Output {
        other
    }
}
