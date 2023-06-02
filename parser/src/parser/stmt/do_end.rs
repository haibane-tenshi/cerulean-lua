use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn do_end<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use crate::parser::block::block;
    use DoEndFailure::*;

    let (s, _, Complete) = match_token(s, Token::Do).map_parse(Do)?;
    let (s, (), _) = block(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _, status) = match_token(s, Token::End).map_parse(End)?;

    frag.commit_scope();
    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub(crate) enum DoEndFailure {
    #[error("missing `do` keyword")]
    Do(#[source] TokenMismatch),
    #[error("missing `end` keyword")]
    End(#[source] TokenMismatch),
}

impl HaveFailureMode for DoEndFailure {
    fn mode(&self) -> FailureMode {
        match self {
            DoEndFailure::Do(_) => FailureMode::Mismatch,
            DoEndFailure::End(_) => FailureMode::Malformed,
        }
    }
}
