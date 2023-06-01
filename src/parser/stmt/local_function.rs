use crate::parser::prelude::*;
use thiserror::Error;

pub(super) fn local_function<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use crate::parser::func_def::func_body;
    use LocalFunctionFailure::*;

    let (s, _, Complete) = match_token(s, Token::Local).map_parse(Local)?;
    let (s, _, Complete) = match_token(s, Token::Function).map_parse(Function)?;
    let (s, (ident, _), Complete) = identifier(s).map_parse(Ident)?;

    // Lua disambiguates this case by introducing local variable first and assigning to it later.
    // This is relevant for recursive functions.
    // We only need to introduce the name:
    // it will get assigned to after function body is parsed.
    let slot = frag.stack_mut().push()?;
    frag.stack_mut().give_name(slot, ident)?;

    let (s, func_id, status) =
        func_body(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    let const_id = chunk.constants.insert(Literal::Function(func_id))?;

    // Stack is already adjusted, we just need to silently write to correct slot here.
    frag.emit_raw(OpCode::LoadConstant(const_id), false)?;
    frag.commit();

    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub enum LocalFunctionFailure {
    #[error("missing `local` keyword")]
    Local(#[source] TokenMismatch),
    #[error("missing `function` keyword")]
    Function(#[source] TokenMismatch),
    #[error("expected function name")]
    Ident(#[source] IdentMismatch),
}

impl HaveFailureMode for LocalFunctionFailure {
    fn mode(&self) -> FailureMode {
        match self {
            LocalFunctionFailure::Local(_) => FailureMode::Mismatch,
            LocalFunctionFailure::Function(_) => FailureMode::Ambiguous,
            LocalFunctionFailure::Ident(_) => FailureMode::Malformed,
        }
    }
}
