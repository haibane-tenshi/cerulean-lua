use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn goto<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let token_goto =
            match_token(Token::Goto).map_failure(|f| ParseFailure::from(GotoFailure::Goto(f)));
        let identifier = identifier.map_failure(|f| ParseFailure::from(GotoFailure::Ident(f)));

        let mut frag = core.decl();

        let source = s.source();
        let _span = trace_span!("goto").entered();

        let state = Source(s)
            .and(token_goto)?
            .with_mode(FailureMode::Malformed)
            .map_output(Spanned::put_range)
            .and(identifier, keep)?
            .map_output(|output| {
                let ((goto_span, label), span) = output.take();

                frag.emit_goto(label, DebugInfo::Goto { goto: goto_span });
                frag.commit();

                trace!(span=?span.span(), str=&source[span.span()]);

                span
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum GotoFailure {
    #[error("missing `goto` keyword")]
    Goto(TokenMismatch),

    #[error("missing target label identifier")]
    Ident(IdentMismatch),
}
