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

        let mut outer_frag = core.scope();

        let state = token_for
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and(
                name_list.map_failure(|f| ParseFailure::from(GenericForFailure::Ident(f))),
                replace,
            )?
            .and(token_in, discard)?
            .with_mode(FailureMode::Malformed)
            .and(
                |s| {
                    let top = outer_frag.stack_slot(outer_frag.stack().len());
                    expr_list_adjusted_to(4, outer_frag.new_core())
                        .map_output(|span: Spanned<_>| span.put(top))
                        .parse_once(s)
                },
                keep,
            )?
            .map_output(|output| {
                let ((names, top), span) = output.take();

                let iter = top;
                let state = top + 1;
                let control = top + 2;
                // Currently unimplemented.
                let _close = top + 3;

                let mut frag = outer_frag.new_scope();
                frag.mark_as_loop();
                let mark = frag.stack().len();
                let count: u32 = names.len().try_into().unwrap();
                let iterator = frag.stack_slot(mark);

                frag.emit(OpCode::LoadStack(iter));
                frag.emit(OpCode::LoadStack(state));
                frag.emit(OpCode::LoadStack(control));
                frag.emit(OpCode::Invoke(iterator));

                frag.emit_adjust_to(mark + count);

                // Assign names.
                frag.adjust_stack_to(mark);
                for name in names {
                    frag.push_temporary(Some(name));
                }

                // First output of iterator is the new value for control variable.
                let new_control = iterator;

                frag.emit(OpCode::LoadStack(new_control));
                frag.emit_load_literal(Literal::Nil);
                frag.emit(RelBinOp::Eq.into());
                frag.emit_jump_to(frag.id(), Some(true));

                frag.emit(OpCode::LoadStack(new_control));
                frag.emit(OpCode::StoreStack(control));

                span.replace(frag).1
            })
            .and(token_do, discard)?
            .then(|frag| {
                |s| -> Result<_, FailFast> {
                    let (mut frag, span) = frag.take();

                    let state = block(frag.new_core())
                        .parse_once(s)?
                        .map_output(|output| opt_replace(span, output).put(frag));

                    Ok(state)
                }
            })?
            .and(token_end, discard)?
            .map_output(|output| {
                let (mut frag, span) = output.take();

                frag.emit_loop_to();
                frag.commit();

                span
            })
            .collapse();

        let state = state.inspect(move |_| {
            outer_frag.commit();
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

fn name_list<'s>(
    s: Lexer<'s>,
) -> Result<ParsingState<Lexer<'s>, Spanned<Vec<&str>>, NameListSuccess, IdentMismatch>, LexError> {
    let (ident, state) = match identifier(s)? {
        ParsingState::Success(s, output, success) => {
            let (ident, span) = output.take();

            (ident, ParsingState::Success(s, span, success))
        }
        ParsingState::Failure(failure) => return Ok(ParsingState::Failure(failure)),
    };

    let mut r = vec![ident];

    let next = |s: Lexer<'s>| -> Result<_, LexError> {
        let state = match_token(Token::Comma)
            .parse(s)?
            .map_failure(|_| NameListSuccess::Comma)
            .and(identifier.map_failure(NameListSuccess::Ident), replace)?
            .map_output(|output| {
                let (ident, span) = output.take();
                r.push(ident);

                span
            });

        Ok(state)
    };

    let state = state
        .and(next.repeat_with(discard).optional(), opt_discard)?
        .map_output(|span| span.replace(r).1);

    Ok(state)
}

enum NameListSuccess {
    Comma,
    Ident(IdentMismatch),
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
