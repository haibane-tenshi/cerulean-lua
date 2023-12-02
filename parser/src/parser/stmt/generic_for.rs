use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn generic_for<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = Complete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::block::block;
        use crate::parser::expr::expr_list_adjusted_to;

        let token_for =
            match_token(Token::For).map_failure(|f| ParseFailure::from(GenericForFailure::For(f)));
        let token_in =
            match_token(Token::In).map_failure(|f| ParseFailure::from(GenericForFailure::In(f)));
        let token_do =
            match_token(Token::Do).map_failure(|f| ParseFailure::from(GenericForFailure::Do(f)));
        let token_end =
            match_token(Token::End).map_failure(|f| ParseFailure::from(GenericForFailure::End(f)));
        let name_list = name_list.map_failure(|f| ParseFailure::from(GenericForFailure::Ident(f)));

        let mut envelope = core.scope();

        let source = s.source();
        let _span = trace_span!("for_generic").entered();

        let state = Source(s)
            .and(token_for)?
            .with_mode(FailureMode::Ambiguous)
            .map_output(Spanned::put_range)
            .and(name_list, keep)?
            .and(token_in, keep_range)?
            .with_mode(FailureMode::Malformed)
            .and(expr_list_adjusted_to(4, envelope.new_core()), discard)?
            .then(|output| {
                |s| -> Result<_, FailFast> {
                    let (((for_span, names), in_span), span) = output.take();

                    let iter = envelope.stack_slot(FragmentStackSlot(0));
                    let state = iter + 1;
                    let control = iter + 2;
                    // Currently unimplemented.
                    let _close = iter + 3;

                    let mut loop_body = envelope.new_scope();
                    loop_body.mark_as_loop();
                    let iter_args = loop_body.stack_slot(FragmentStackSlot(0));

                    loop_body.emit(OpCode::LoadStack(iter), in_span.clone());
                    loop_body.emit(OpCode::StoreCallable, in_span.clone());
                    loop_body.emit(OpCode::LoadStack(state), in_span.clone());
                    loop_body.emit(OpCode::LoadStack(control), in_span.clone());
                    loop_body.emit(OpCode::Invoke(iter_args), in_span.clone());

                    loop_body.emit_adjust_to(FragmentStackSlot(names.len()), in_span.clone());

                    // Assign names.
                    loop_body.adjust_stack_to(FragmentStackSlot(0));
                    for name in names {
                        loop_body.push_temporary(Some(name));
                    }

                    // First output of iterator is the new value for control variable.
                    let new_control = iter_args;

                    loop_body.emit(OpCode::LoadStack(new_control), for_span.clone());
                    loop_body.emit_load_literal(Literal::Nil, for_span.clone());
                    loop_body.emit(RelBinOp::Eq.into(), for_span.clone());
                    loop_body.emit_jump_to_end(Some(true), for_span.clone());

                    loop_body.emit(OpCode::LoadStack(new_control), for_span.clone());
                    loop_body.emit(OpCode::StoreStack(control), for_span.clone());

                    let state = Source(s)
                        .and(token_do)?
                        .and(block(loop_body.new_core()), opt_discard)?
                        .and(token_end, replace_range)?
                        .inspect(|output| {
                            let end_span = output.value.clone();
                            loop_body.emit_loop_to(end_span.clone());
                            loop_body.commit(end_span);
                        })
                        .map_output(|output| replace(span, output));

                    Ok(state)
                }
            })?
            .collapse()
            .map_output(move |output| {
                let (end_span, span) = output.take();
                envelope.commit(end_span);

                trace!(span=?span.span(), str=&source[span.span()]);

                span
            });

        Ok(state)
    }
}

#[derive(Debug, Error)]
pub(crate) enum GenericForFailure {
    #[error("missing `for` token")]
    For(#[source] TokenMismatch),
    #[error("missing identifier for control variable")]
    Ident(#[source] IdentMismatch),
    #[error("missing `in` token")]
    In(#[source] TokenMismatch),
    #[error("missing `do` token")]
    Do(#[source] TokenMismatch),
    #[error("missing `end` token")]
    End(#[source] TokenMismatch),
}

fn name_list(
    s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<Vec<Ident>>, NameListSuccess, IdentMismatch>, LexError>
{
    let mut result = Vec::new();
    let token_comma = match_token(Token::Comma).map_failure(|_| NameListSuccess::Comma);
    let mut identifier = identifier.map_output(|output: Spanned<_>| {
        let (ident, span) = output.take();
        result.push(ident);
        span
    });

    let state = Source(s)
        .and(identifier.as_mut())?
        .and(
            (|s| {
                let state = Source(s).and(token_comma)?.and(
                    identifier.as_mut().map_failure(NameListSuccess::Ident),
                    discard,
                )?;

                Ok(state)
            })
            .repeat_with(discard)
            .optional(),
            opt_discard,
        )?
        .map_output(|span| span.put(result));

    Ok(state)
}

enum NameListSuccess {
    Comma,
    Ident(IdentMismatch),
}

impl From<Never> for NameListSuccess {
    fn from(value: Never) -> Self {
        match value {}
    }
}

impl Arrow<NameListSuccess> for Complete {
    type Output = NameListSuccess;

    fn arrow(self, other: NameListSuccess) -> Self::Output {
        other
    }
}

impl Arrow<ParseFailure> for NameListSuccess {
    type Output = ParseFailure;

    fn arrow(self, other: ParseFailure) -> Self::Output {
        match self {
            NameListSuccess::Comma => other,
            NameListSuccess::Ident(f) => {
                ParseFailure::from(GenericForFailure::Ident(f)).arrow(other)
            }
        }
    }
}
