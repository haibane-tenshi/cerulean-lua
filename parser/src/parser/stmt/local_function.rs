use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn local_function<'s, 'origin>(
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

        let token_local = match_token(Token::Local)
            .map_failure(|f| ParseFailure::from(LocalFunctionFailure::Local(f)));
        let token_function = match_token(Token::Function)
            .map_failure(|f| ParseFailure::from(LocalFunctionFailure::Function(f)));
        let identifier =
            identifier.map_failure(|f| ParseFailure::from(LocalFunctionFailure::Ident(f)));

        let mut frag = core.decl();

        let state = Source(s)
            .and(token_local)?
            .with_mode(FailureMode::Ambiguous)
            .and(token_function, discard)?
            .with_mode(FailureMode::Malformed)
            .and(identifier, replace)?
            .map_output(|output| {
                // Lua disambiguates this case by introducing local variable first and assigning to it later.
                // This is relevant for recursive functions.
                let (ident, span) = output.take();
                frag.push_temporary(Some(ident));

                span
            })
            .and(func_body(frag.new_core()), replace)?
            .map_output(|output| {
                let (func_id, span) = output.take();

                frag.emit_load_literal(Literal::Function(func_id));
                // Stack is already adjusted, remove unnecessary temporary.
                frag.pop_temporary();
                frag.commit();

                span
            })
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
