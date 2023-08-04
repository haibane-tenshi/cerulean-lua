use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn generic_for<'s, 'origin>(
    mut outer_frag: Fragment<'s, 'origin>,
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

        let state = token_for
            .parse_once(s)?
            .with_mode(FailureMode::Ambiguous)
            .and_with(
                name_list.map_failure(GenericForFailure::Ident),
                |_, names| names,
            )?
            .and_discard(token_in)?
            .with_mode(FailureMode::Malformed)
            .and(|s| {
                let top = outer_frag.stack().top()?;
                expr_list_adjusted_to(4, outer_frag.new_fragment())
                    .map_output(|_| top)
                    .parse_once(s)
            })?
            .try_map_output(|(names, top)| -> Result<_, CodegenError> {
                let iter = top;
                let state = top + 1;
                let control = top + 2;
                // Currently unimplemented.
                let _close = top + 3;

                let nil = outer_frag.const_table_mut().insert(Literal::Nil)?;

                let mut frag = outer_frag.new_fragment();
                let new_control = frag.stack().top()?;

                frag.emit(OpCode::LoadStack(iter))?;
                frag.emit(OpCode::LoadStack(state))?;
                frag.emit(OpCode::LoadStack(control))?;
                frag.emit(OpCode::Invoke(new_control))?;

                let count: u32 = names.len().try_into().unwrap();
                frag.emit_adjust_to(new_control + count)?;

                frag.emit(OpCode::LoadStack(new_control))?;
                frag.emit(OpCode::LoadConstant(nil))?;
                frag.emit(OpCode::RelBinOp(RelBinOp::Eq))?;
                frag.emit_jump_to(frag.id(), Some(true))?;

                frag.emit(OpCode::LoadStack(new_control))?;
                frag.emit(OpCode::StoreStack(control))?;

                // Assign names
                for (name, slot) in names.into_iter().zip((new_control.0..).map(StackSlot)) {
                    frag.stack_mut().give_name(slot, name)?;
                }

                Ok(frag)
            })?
            .and_discard(token_do)?
            .then(|mut frag| {
                |s| -> Result<_, FailFast> {
                    Ok(block(frag.new_fragment())
                        .parse_once(s)?
                        .map_output(|_| frag))
                }
            })?
            .and_discard(token_end)?
            .try_map_output(|mut frag| -> Result<_, CodegenError> {
                frag.emit_loop_to()?;
                frag.commit_scope();
                Ok(())
            })?
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

impl HaveFailureMode for GenericForFailure {
    fn mode(&self) -> FailureMode {
        match self {
            GenericForFailure::For(_) => FailureMode::Mismatch,
            GenericForFailure::Ident(_) => FailureMode::Ambiguous,
            GenericForFailure::In(_) => FailureMode::Ambiguous,
            GenericForFailure::Do(_) => FailureMode::Malformed,
            GenericForFailure::End(_) => FailureMode::Malformed,
        }
    }
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

impl Combine<ParseFailure> for NameListSuccess {
    type Output = ParseFailure;

    fn combine(self, other: ParseFailure) -> Self::Output {
        match self {
            NameListSuccess::Comma => other,
            NameListSuccess::Ident(err) => {
                ParseFailure::from(GenericForFailure::Ident(err)).combine(other)
            }
        }
    }
}
