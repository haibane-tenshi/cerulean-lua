use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn function<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::func_def::func_body;

        let token_function = match_token(Token::Function)
            .map_failure(|f| ParseFailure::from(FunctionFailure::Function(f)));

        let r = token_function
            .parse(s)?
            .with_mode(FailureMode::Malformed)
            .and(func_body(frag.new_fragment()))?
            .map_output(|(_, func_id)| {
                frag.emit_load_literal(Literal::Function(func_id));

                frag.commit();
            })
            .collapse();

        Ok(r)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse function constructor")]
pub enum FunctionFailure {
    Function(TokenMismatch),
}
