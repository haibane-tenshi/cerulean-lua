use repr::opcode::FunctionId;
use crate::parser::prelude::*;
use crate::tracker2::stack::StackView;
use thiserror::Error;

pub(crate) fn func_body<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, FunctionId, Complete), Error<ParseFailure>> {
    use crate::parser::block::block;
    use crate::tracker2::function::Function;
    use FuncDefFailure::*;

    let (s, _, Complete) = match_token(s, Token::ParL).map_parse(ParL)?;

    // Start function
    let mut fun = Function::new();
    let mut frag = Fragment::new(&mut fun, outer_frag.stack_mut().new_frame());

    // Currently this slot contains pointer to function itself.
    // In the future we will put environment here instead.
    frag.stack_mut().push()?;

    let (s, param_count, _) = parlist(s.clone(), frag.stack_mut()).optional(s);
    let param_count = param_count.unwrap_or(0);

    // An extra stack slot is taken by function pointer itself.
    let height = param_count + 1;

    let (s, _, Complete) = match_token(s, Token::ParR).map_parse(ParR)?;
    let (s, (), _) = block(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;
    let (s, _, status) = match_token(s, Token::End).map_parse(End)?;

    // Finish function
    frag.commit_scope();
    let fun = fun.resolve(height);
    let func_id = chunk.functions.push(fun)?;

    // Drop outer fragment to make sure we didn't mess up current function.
    drop(outer_frag);

    Ok((s, func_id, status))
}

#[derive(Debug, Error)]
pub enum FuncDefFailure {
    #[error("missing opening parenthesis")]
    ParL(#[source] TokenMismatch),
    #[error("missing closing parenthesis")]
    ParR(#[source] TokenMismatch),
    #[error("missing `end` token")]
    End(#[source] TokenMismatch),
}

impl HaveFailureMode for FuncDefFailure {
    fn mode(&self) -> FailureMode {
        match self {
            FuncDefFailure::ParL(_) => FailureMode::Mismatch,
            FuncDefFailure::ParR(_) => FailureMode::Malformed,
            FuncDefFailure::End(_) => FailureMode::Malformed,
        }
    }
}

fn parlist<'s>(
    s: Lexer<'s>,
    stack: &mut StackView<'s, '_>,
) -> Result<(Lexer<'s>, u32, Error<ParListMismatch>), Error<IdentMismatch>> {
    let mut count = 0;

    let (mut s, (ident, _), Complete) = identifier(s)?;
    let slot = stack.push()?;
    stack.give_name(slot, ident)?;
    count += 1;

    let mut next_ident = |s: Lexer<'s>| -> Result<_, Error<ParListMismatch>> {
        use ParListMismatch::*;

        let (s, _, Complete) = match_token(s, Token::Comma).map_parse(Comma)?;
        let (s, (ident, _), Complete) = identifier(s).map_parse(Ident)?;

        let slot = stack.push()?;
        stack.give_name(slot, ident)?;
        count += 1;

        Ok(s)
    };

    let status = loop {
        s = match next_ident(s.clone()) {
            Ok(s) => s,
            Err(err) => break err,
        };
    };

    Ok((s, count, status))
}

enum ParListMismatch {
    Comma(TokenMismatch),
    Ident(IdentMismatch),
}
