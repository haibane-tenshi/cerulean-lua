use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn return_<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailureOrComplete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::expr::expr_list;

        let token_return = match_token(Token::Return)
            .map_failure(|f| ParseFailure::from(ReturnFailure::Return(f)));
        let token_semicolon = match_token(Token::Semicolon).map_failure(|_| Complete);

        let mut frag = core.scope();

        let state = token_return
            .parse(s)?
            .with_mode(FailureMode::Malformed)
            .map_output(|span| span.replace(frag.stack().len()).1)
            .and(expr_list(frag.new_core()).optional(), opt_discard)?
            .and(
                token_semicolon
                    .optional()
                    .map_success(ParseFailureOrComplete::Complete),
                opt_discard,
            )?
            .map_output(|output| {
                let (slot, span) = output.take();

                frag.emit(OpCode::Return(frag.stack_slot(slot)));
                frag.commit();

                span
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
