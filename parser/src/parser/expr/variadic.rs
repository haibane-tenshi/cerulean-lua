use crate::parser::prelude::*;
use logos::Span;
use thiserror::Error;

pub(crate) fn variadic<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = TokenMismatch,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let mut frag = core.expr();

        let state = match_token(Token::TripleDot)
            .parse(s)?
            .try_map_output(|r| {
                if frag.signature().is_variadic {
                    Ok(r)
                } else {
                    let err = VariadicExprError { span: r.span };

                    Err(CodegenError::VariadicExpr(err))
                }
            })?
            .inspect(move |_| {
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
