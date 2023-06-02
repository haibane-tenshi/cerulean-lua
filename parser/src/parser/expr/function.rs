use thiserror::Error;

use crate::parser::prelude::*;

pub(crate) fn function<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use crate::parser::func_def::func_body;
    use FunctionFailure::*;

    let (s, _, Complete) = match_token(s, Token::Function).map_parse(Function)?;
    let (s, func_id, status) =
        func_body(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    let const_id = chunk.constants.insert(Literal::Function(func_id))?;
    frag.emit(OpCode::LoadConstant(const_id))?;

    frag.commit();
    Ok((s, (), status))
}

#[derive(Debug, Error)]
#[error("failed to parse function constructor")]
pub enum FunctionFailure {
    Function(TokenMismatch),
}

impl HaveFailureMode for FunctionFailure {
    fn mode(&self) -> FailureMode {
        FailureMode::Mismatch
    }
}
