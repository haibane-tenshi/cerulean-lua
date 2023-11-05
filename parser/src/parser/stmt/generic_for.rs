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

        let state = Source(s)
            .and(token_for)?
            .with_mode(FailureMode::Ambiguous)
            .and(name_list, replace)?
            .and(token_in, discard)?
            .with_mode(FailureMode::Malformed)
            .and(
                |s| {
                    let top = envelope.stack_slot(envelope.stack().len());
                    expr_list_adjusted_to(4, envelope.new_core())
                        .map_output(|span: Spanned<_>| span.put(top))
                        .parse_once(s)
                },
                keep,
            )?
            .then(|output| {
                |s| -> Result<_, FailFast> {
                    let ((names, top), span) = output.take();

                    let iter = top;
                    let state = top + 1;
                    let control = top + 2;
                    // Currently unimplemented.
                    let _close = top + 3;

                    let mut loop_body = envelope.new_scope();
                    loop_body.mark_as_loop();
                    let mark = loop_body.stack().len();
                    let count = names.len();
                    let iterator = loop_body.stack_slot(mark);

                    loop_body.emit(OpCode::LoadStack(iter));
                    loop_body.emit(OpCode::LoadStack(state));
                    loop_body.emit(OpCode::LoadStack(control));
                    loop_body.emit(OpCode::Invoke(iterator));

                    loop_body.emit_adjust_to(mark + count);

                    // Assign names.
                    loop_body.adjust_stack_to(mark);
                    for name in names {
                        loop_body.push_temporary(Some(name));
                    }

                    // First output of iterator is the new value for control variable.
                    let new_control = iterator;

                    loop_body.emit(OpCode::LoadStack(new_control));
                    loop_body.emit_load_literal(Literal::Nil);
                    loop_body.emit(RelBinOp::Eq.into());
                    loop_body.emit_jump_to(loop_body.id(), Some(true));

                    loop_body.emit(OpCode::LoadStack(new_control));
                    loop_body.emit(OpCode::StoreStack(control));

                    let state = Source(s)
                        .and(token_do)?
                        .and(block(loop_body.new_core()), opt_discard)?
                        .and(token_end, discard)?
                        .inspect(|_| {
                            loop_body.emit_loop_to();
                            loop_body.commit();
                        })
                        .map_output(|output| discard(span, output));

                    Ok(state)
                }
            })?
            .collapse()
            .inspect(move |_| envelope.commit());

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
) -> Result<ParsingState<Lexer, (), Spanned<Vec<&str>>, NameListSuccess, IdentMismatch>, LexError> {
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
