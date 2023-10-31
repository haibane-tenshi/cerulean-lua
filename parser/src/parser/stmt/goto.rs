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

        let state = token_goto
            .parse(s)?
            .with_mode(FailureMode::Malformed)
            .and(identifier, replace)?
            .map_output(|output| {
                let (label, span) = output.take();

                frag.emit_goto(label);
                frag.commit();

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
