use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn function<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::func_def::func_body;

        let mut frag = core.expr();

        let token_function = match_token(Token::Function)
            .map_failure(|f| ParseFailure::from(FunctionFailure::Function(f)));

        let r = Source(s)
            .and(token_function)?
            .with_mode(FailureMode::Malformed)
            .and(func_body(frag.new_core(), false), replace)?
            .map_output(|output| {
                let (func_id, span) = output.take();

                frag.emit(OpCode::MakeClosure(func_id));
                frag.commit();

                span
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
