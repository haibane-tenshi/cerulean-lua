use crate::parser::prelude::*;
use thiserror::Error;

pub(super) fn generic_for<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_list_adjusted_to;
    use GenericForFailure::*;

    let (s, _, Complete) = match_token(s, Token::For).map_parse(For)?;
    let (s, names, _) = name_list(s).map_parse(Ident)?;
    let (s, _, Complete) = match_token(s, Token::In).map_parse(In)?;

    let top = outer_frag.stack().top()?;
    let (s, (), _) = expr_list_adjusted_to(s, 4, chunk, outer_frag.new_fragment())
        .with_mode(FailureMode::Malformed)?;

    let iter = top;
    let state = top + 1;
    let control = top + 2;
    // Currently unimplemented.
    let _close = top + 3;

    let nil = chunk.constants.insert(Literal::Nil)?;

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

    let (s, _, Complete) = match_token(s, Token::Do).map_parse(Do)?;
    let (s, (), _) = block(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _, status) = match_token(s, Token::End).map_parse(End)?;

    frag.emit_loop_to()?;
    frag.commit_scope();
    outer_frag.commit();

    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub enum GenericForFailure {
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

fn name_list<'s>(s: Lexer<'s>) -> Result<(Lexer<'s>, Vec<&'s str>, ()), ParseError<IdentMismatch>> {
    let (mut s, (ident, _), Complete) = identifier(s)?;
    let mut r = vec![ident];

    let next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, _, _), _> {
        let (s, _, Complete) = match_token(s, Token::Comma).map_err(|_| ())?;
        identifier(s).map_err(|_| ())
    };

    let status = loop {
        let ident;
        (s, ident) = match next_part(s.clone()) {
            Ok((s, (ident, _), _)) => (s, ident),
            Err(err) => break err,
        };
        r.push(ident);
    };

    Ok((s, r, status))
}
