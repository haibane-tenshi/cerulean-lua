use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn decl_global_fn<'s, 'origin>(
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

        let token_function = match_token(Token::Function)
            .map_failure(|f| ParseFailure::from(DeclGlobalFnFailure::Function(f)));

        let mut frag = core.decl();

        let source = s.source();
        let _span = trace_span!("decl_global_fn").entered();

        let state = Source(s)
            .and(token_function)?
            .with_mode(FailureMode::Malformed)
            .and(fn_name(frag.new_core()), replace)?
            .then(|output| {
                let (self_arg, span) = output.take();

                func_body(frag.new_core(), self_arg).map_output(|output| replace(span, output))
            })?
            .map_output(|output| {
                let (func_id, span) = output.take();

                frag.emit(OpCode::MakeClosure(func_id));
                frag.emit(OpCode::TabSet);
                frag.commit();

                trace!(span=?span.span(), str=&source[span.span()]);

                span
            })
            .collapse();

        Ok(state)
    }
}

pub(crate) fn fn_name<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<bool>,
    Success = CompleteOr<ParseFailure>,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    |s: Lexer<'s>| {
        let identifier =
            identifier.map_failure(|f| ParseFailure::from(DeclGlobalFnFailure::Ident(f)));
        let token_dot = match_token(Token::Dot)
            .map_failure(|f| ParseFailure::from(DeclGlobalFnFailure::Dot(f)));
        let token_colon = match_token(Token::Dot)
            .map_failure(|f| ParseFailure::from(DeclGlobalFnFailure::Colon(f)));

        let mut envelope = core.expr();

        let state = Source(s)
            .and(identifier)?
            .with_mode(FailureMode::Malformed)
            .try_map_output(|output| -> Result<_, CodegenError> {
                use crate::codegen::fragment::UpvalueSource;

                let (ident, span) = output.take();

                let opcode = match envelope.capture_global_env()? {
                    UpvalueSource::Temporary(slot) => OpCode::LoadStack(slot),
                    UpvalueSource::Upvalue(slot) => OpCode::LoadUpvalue(slot),
                };

                envelope.emit(opcode);
                envelope.emit_load_literal(Literal::String(ident.to_string()));

                Ok(span)
            })?
            .and(
                (|s: Lexer<'s>| -> Result<_, FailFast> {
                    use crate::codegen::stack::CommitKind;
                    let mut frag = envelope.new_fragment_at_boundary(CommitKind::Expr);

                    let state = Source(s)
                        .and(token_dot)?
                        .with_mode(FailureMode::Malformed)
                        .and(identifier, replace)?
                        .map_output(|output| {
                            let (ident, span) = output.take();

                            frag.emit(OpCode::TabGet);
                            frag.emit_load_literal(Literal::String(ident.to_string()));
                            frag.commit();

                            span
                        })
                        .collapse();

                    Ok(state)
                })
                .repeat_with(discard)
                .optional(),
                opt_discard,
            )?
            .and(
                (|s: Lexer<'s>| -> Result<_, FailFast> {
                    use crate::codegen::stack::CommitKind;
                    let mut frag = envelope.new_fragment_at_boundary(CommitKind::Expr);

                    let state = Source(s)
                        .and(token_colon)?
                        .with_mode(FailureMode::Malformed)
                        .and(identifier, replace)?
                        .map_output(|output| {
                            let (ident, span) = output.take();

                            frag.emit(OpCode::TabGet);
                            frag.emit_load_literal(Literal::String(ident.to_string()));
                            frag.commit();

                            span
                        })
                        .map_success(CompleteOr::Complete)
                        .collapse();

                    Ok(state)
                })
                .optional(),
                opt_replace,
            )?
            .map_output(|output| {
                envelope.commit();
                output.map(|t| t.is_some())
            })
            .collapse();

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum DeclGlobalFnFailure {
    #[error("missing `function` keyword")]
    Function(#[source] TokenMismatch),
    #[error("missing dot")]
    Dot(#[source] TokenMismatch),
    #[error("missing colon")]
    Colon(#[source] TokenMismatch),
    #[error("expected function name")]
    Ident(#[source] IdentMismatch),
}
