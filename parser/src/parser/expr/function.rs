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
        use FunctionFailure::*;

        let function = |s: Lexer<'s>| -> Result<_, FailFast> {
            Ok(match_token(Token::Function)
                .parse(s)?
                .map_failure(Function)
                .map_failure(Into::<ParseFailure>::into))
        };

        let r = function(s)?
            .and(func_body(frag.new_fragment()))?
            .try_map_output(|(_, func_id)| -> Result<_, CodegenError> {
                frag.emit_load_literal(Literal::Function(func_id))?;

                frag.commit();
                Ok(())
            })?;

        Ok(r)
    }
}

#[derive(Debug, Error)]
#[error("failed to parse function constructor")]
pub enum FunctionFailure {
    Function(TokenMismatch),
}

impl HaveFailureMode for FunctionFailure {
    fn mode(&self) -> FailureMode {
        FailureMode::Mismatch
    }
}
