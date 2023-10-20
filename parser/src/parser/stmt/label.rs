use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn label<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
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
            .and_replace(identifier)?
            .and_discard(token_double_colon)?
            .try_map_output(|(label, _)| -> Result<_, CodegenError> {
                frag.try_emit_label(label)?;
                frag.commit();

                Ok(())
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
