use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn while_do<'s>(
    s: Lexer<'s>,
    mut outer_frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_adjusted_to_1;
    use WhileDoFailure::*;

    let (s, _) = match_token(s, Token::While).map_parse(While)?;

    let mut frag = outer_frag.new_fragment();

    let (s, ()) = expr_adjusted_to_1(s, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _) = match_token(s, Token::Do).map_parse(Do)?;

    frag.emit_jump_to(frag.id(), Some(false))?;

    let (s, ()) = block(s, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _) = match_token(s, Token::End).map_parse(End)?;

    frag.emit_loop_to()?;
    frag.commit();
    outer_frag.commit();

    Ok((s, ()))
}

#[derive(Debug, Error)]
pub(crate) enum WhileDoFailure {
    #[error("missing `while` keyword")]
    While(#[source] TokenMismatch),
    #[error("missing `do` keyword")]
    Do(#[source] TokenMismatch),
    #[error("missing `end` keyword")]
    End(#[source] TokenMismatch),
}

impl HaveFailureMode for WhileDoFailure {
    fn mode(&self) -> FailureMode {
        match self {
            WhileDoFailure::While(_) => FailureMode::Mismatch,
            WhileDoFailure::Do(_) => FailureMode::Malformed,
            WhileDoFailure::End(_) => FailureMode::Malformed,
        }
    }
}
