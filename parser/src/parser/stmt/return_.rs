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
            .with_mode(FailureMode::Malformed)
            .map_output(|_| frag.stack().top())
            .and_discard(expr_list(frag.new_fragment()).optional())?
            .and_discard(
                token_semicolon
                    .optional()
                    .map_success(ParseFailureOrComplete::Complete),
            )?
            .map_output(|slot| {
                frag.emit(OpCode::Return(slot));
                frag.commit();
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum ReturnFailure {
    #[error("missing return keyword")]
    Return(TokenMismatch),
}
