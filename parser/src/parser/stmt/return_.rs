use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn return_<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailureOrComplete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_list;

        let token_return = match_token(Token::Return)
            .map_failure(|f| ParseFailure::from(ReturnFailure::Return(f)));
        let token_semicolon = match_token(Token::Semicolon).map_failure(|_| Complete);

        let state = token_return
            .parse(s)?
            .try_map_output(|_| frag.stack().top())?
            .and_discard(expr_list(frag.new_fragment()))?
            .and_discard(
                token_semicolon
                    .optional()
                    .map_success(ParseFailureOrComplete::Complete),
            )?
            .try_map_output(|slot| -> Result<_, CodegenError> {
                frag.emit(OpCode::Return(slot))?;
                frag.commit();
                Ok(())
            })?;

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum ReturnFailure {
    #[error("missing return keyword")]
    Return(TokenMismatch),
}

impl HaveFailureMode for ReturnFailure {
    fn mode(&self) -> FailureMode {
        FailureMode::Mismatch
    }
}
