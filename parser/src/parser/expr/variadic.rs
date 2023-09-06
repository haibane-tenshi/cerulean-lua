use crate::parser::prelude::*;
use logos::Span;
use thiserror::Error;

pub(crate) fn variadic<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = Complete,
    Failure = TokenMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr();

        let state = match_token(Token::TripleDot)
            .parse(s)?
            .try_map_output(|span| {
                if frag.signature().is_variadic {
                    Ok(())
                } else {
                    let err = VariadicExprError { span };

                    Err(CodegenError::VariadicExpr(err))
                }
            })?
            .map_output(move |_| {
                frag.emit(OpCode::LoadVariadic);
                frag.commit();
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
#[error("variadic expression used outside variadic function")]
pub struct VariadicExprError {
    span: Span,
}
