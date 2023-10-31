use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn label<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let token_double_colon = match_token(Token::DoubleColon)
            .map_failure(|f| ParseFailure::from(LabelFailure::DoubleColon(f)));
        let identifier = identifier.map_failure(|f| ParseFailure::from(LabelFailure::Ident(f)));

        let mut frag = core.decl();

        let state = token_double_colon
            .parse(s)?
            .with_mode(FailureMode::Malformed)
            .and(identifier, replace)?
            .and(token_double_colon, discard)?
            .try_map_output(|output| -> Result<_, CodegenError> {
                let (label, span) = output.take();

                frag.try_emit_label(label)?;
                frag.commit();

                Ok(span)
            })?
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum LabelFailure {
    #[error("missing double colon keyword")]
    DoubleColon(TokenMismatch),

    #[error("missing label identifier")]
    Ident(IdentMismatch),
}
