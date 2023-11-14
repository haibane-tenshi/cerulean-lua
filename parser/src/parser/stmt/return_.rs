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
        let return_values = frag.stack_slot(frag.stack().len());

        let source = s.source();
        let _span = trace_span!("return").entered();

        let state = Source(s)
            .and(token_return)?
            .with_mode(FailureMode::Malformed)
            .and(expr_list(frag.new_core()).optional(), opt_discard)?
            .and(
                token_semicolon
                    .optional()
                    .map_success(ParseFailureOrComplete::Complete),
                opt_discard,
            )?
            .inspect(|output| {
                frag.emit(OpCode::Return(return_values));
                frag.commit();

                trace!(span=?output.span(), str=&source[output.span()]);
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
