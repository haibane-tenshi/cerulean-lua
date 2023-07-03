use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn repeat_until<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use crate::parser::block::inner_block;
    use crate::parser::expr::expr_adjusted_to_1;
    use RepeatUntilFailure::*;

    let (s, _) = match_token(s, Token::Repeat).map_parse(Repeat)?;

    let mut frag = outer_frag.new_fragment();

    let (s, ()) = inner_block(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _) = match_token(s, Token::Until).map_parse(Until)?;
    let (s, ()) =
        expr_adjusted_to_1(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    frag.emit_jump_to(frag.id(), Some(true))?;
    frag.emit_loop_to()?;
    frag.commit_scope();
    outer_frag.commit();

    Ok((s, ()))
}

#[derive(Debug, Error)]
pub(crate) enum RepeatUntilFailure {
    #[error("missing `repeat` keyword")]
    Repeat(#[source] TokenMismatch),
    #[error("missing `until` keyword")]
    Until(#[source] TokenMismatch),
}

impl HaveFailureMode for RepeatUntilFailure {
    fn mode(&self) -> FailureMode {
        match self {
            RepeatUntilFailure::Repeat(_) => FailureMode::Mismatch,
            RepeatUntilFailure::Until(_) => FailureMode::Malformed,
        }
    }
}
