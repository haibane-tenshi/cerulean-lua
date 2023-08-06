use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn local_function<'s, 'origin>(
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

        let token_local = match_token(Token::Local)
            .map_failure(|f| ParseFailure::from(LocalFunctionFailure::Local(f)));
        let token_function = match_token(Token::Function)
            .map_failure(|f| ParseFailure::from(LocalFunctionFailure::Function(f)));
        let identifier =
            identifier.map_failure(|f| ParseFailure::from(LocalFunctionFailure::Ident(f)));

        let state = token_local
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and(token_function)?
            .with_mode(FailureMode::Malformed)
            .and_with(identifier, |_, (ident, _)| ident)?
            .try_map_output(|ident| -> Result<_, CodegenError> {
                // Lua disambiguates this case by introducing local variable first and assigning to it later.
                // This is relevant for recursive functions.
                // We only need to introduce the name:
                // it will get assigned to after function body is parsed.
                let slot = frag.stack_mut().push()?;
                frag.stack_mut().give_name(slot, ident)?;
                Ok(())
            })?
            .and_replace(func_body(frag.new_fragment()))?
            .try_map_output(|func_id| -> Result<_, CodegenError> {
                let const_id = frag.const_table_mut().insert(Literal::Function(func_id))?;

                // Stack is already adjusted, we just need to silently write to correct slot here.
                frag.emit_raw(OpCode::LoadConstant(const_id), false)?;

                frag.commit();
                Ok(())
            })?
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum LocalFunctionFailure {
    #[error("missing `local` keyword")]
    Local(#[source] TokenMismatch),
    #[error("missing `function` keyword")]
    Function(#[source] TokenMismatch),
    #[error("expected function name")]
    Ident(#[source] IdentMismatch),
}
