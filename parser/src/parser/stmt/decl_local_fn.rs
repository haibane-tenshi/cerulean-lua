use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn decl_local_fn<'s, 'origin>(
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
            .map_failure(|f| ParseFailure::from(DeclLocalFnFailure::Local(f)));
        let token_function = match_token(Token::Function)
            .map_failure(|f| ParseFailure::from(DeclLocalFnFailure::Function(f)));
        let identifier =
            identifier.map_failure(|f| ParseFailure::from(DeclLocalFnFailure::Ident(f)));

        let mut frag = core.decl();

        let source = s.source();
        let _span = trace_span!("decl_local_fn").entered();

        let state = Source(s)
            .and(token_local)?
            .with_mode(FailureMode::Ambiguous)
            .map_output(Spanned::put_range)
            .and(token_function, keep_range)?
            .with_mode(FailureMode::Malformed)
            .and(identifier, keep_with_range)?
            .then(|output| {
                // Lua disambiguates this case by introducing local variable first and assigning to it later.
                // This is relevant for recursive functions.
                let (((local_span, fn_span), ident, ident_span), span) = output.take();
                frag.push_temporary(Some(ident));

                func_body(frag.new_core(), false, ident.0, span.span().start)
                    .map_output(|output| (local_span, fn_span, ident_span, replace(span, output)))
            })?
            .map_output(|(local_span, fn_span, ident_span, output)| {
                let (recipe_id, output) = output.take();

                frag.emit_with_debug(
                    OpCode::MakeClosure(recipe_id),
                    DebugInfo::FnDecl {
                        local: Some(local_span),
                        function: fn_span,
                        name: ident_span,
                        total_span: output.span(),
                    },
                );
                // Stack is already adjusted, remove unnecessary temporary.
                frag.pop_temporary();
                frag.commit();

                trace!(span=?output.span(), str=&source[output.span()]);

                output
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum DeclLocalFnFailure {
    #[error("missing `local` keyword")]
    Local(#[source] TokenMismatch),
    #[error("missing `function` keyword")]
    Function(#[source] TokenMismatch),
    #[error("expected function name")]
    Ident(#[source] IdentMismatch),
}
