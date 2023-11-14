use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn break_<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let token_break =
            match_token(Token::Break).map_failure(|f| ParseFailure::from(BreakFailure::Break(f)));

        let mut frag = core.decl();

        let source = s.source();
        let _span = trace_span!("break").entered();

        let state = token_break
            .parse(s)?
            .try_map_output(|span| -> Result<_, CodegenError> {
                frag.emit_break()?;
                frag.commit();

                trace!(span=?span.span(), str=&source[span.span()]);

                Ok(span)
            })?;

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum BreakFailure {
    #[error("missing `break` keyword")]
    Break(TokenMismatch),
}
