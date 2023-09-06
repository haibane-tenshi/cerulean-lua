use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn generic_for<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
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
            .and_with(
                name_list.map_failure(|f| ParseFailure::from(GenericForFailure::Ident(f))),
                |_, names| names,
            )?
            .and_discard(token_in)?
            .with_mode(FailureMode::Malformed)
            .and(|s| {
                let top = outer_frag.stack_slot(outer_frag.stack().len());
                expr_list_adjusted_to(4, outer_frag.new_core())
                    .map_output(|_| top)
                    .parse_once(s)
            })?
            .map_output(|(names, top)| {
                let iter = top;
                let state = top + 1;
                let control = top + 2;
                // Currently unimplemented.
                let _close = top + 3;

                let mut frag = outer_frag.new_scope();
                let mark = frag.stack().len();
                let count: u32 = names.len().try_into().unwrap();
                let iterator = frag.stack_slot(mark);

                frag.emit(OpCode::LoadStack(iter));
                frag.emit(OpCode::LoadStack(state));
                frag.emit(OpCode::LoadStack(control));
                frag.emit(OpCode::Invoke(iterator));

                frag.emit_adjust_to(mark + count);

                // Assign names.
                frag.stack_mut().adjust_to(mark);
                for name in names {
                    frag.stack_mut().push(Some(name));
                }

                // First output of iterator is the new value for control variable.
                let new_control = iterator;

                frag.emit(OpCode::LoadStack(new_control));
                frag.emit_load_literal(Literal::Nil);
                frag.emit(RelBinOp::Eq.into());
                frag.emit_jump_to(frag.id(), Some(true));

                frag.emit(OpCode::LoadStack(new_control));
                frag.emit(OpCode::StoreStack(control));

                frag
            })
            .and_discard(token_do)?
            .then(|mut frag| {
                |s| -> Result<_, FailFast> {
                    Ok(block(frag.new_core()).parse_once(s)?.map_output(|_| frag))
                }
            })?
            .and_discard(token_end)?
            .map_output(|mut frag| {
                frag.emit_loop_to();
                frag.commit();
            })
            .collapse();

        let state = state.map_output(move |_| {
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
) -> Result<ParsingState<Lexer<'s>, Vec<&str>, NameListSuccess, IdentMismatch>, LexError> {
    let (ident, state) = match identifier(s)? {
        ParsingState::Success(s, (output, _), success) => {
            (output, ParsingState::Success(s, (), success))
        }
        ParsingState::Failure(failure) => return Ok(ParsingState::Failure(failure)),
    };

    let mut r = vec![ident];

    let next = |s: Lexer<'s>| -> Result<_, LexError> {
        let state = match_token(Token::Comma)
            .parse(s)?
            .map_failure(|_| NameListSuccess::Comma)
            .and(identifier.map_failure(NameListSuccess::Ident))?
            .map_output(|(_, (ident, _))| r.push(ident));

        Ok(state)
    };

    let state = state.and(next.repeat())?.map_output(|_| r);

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
