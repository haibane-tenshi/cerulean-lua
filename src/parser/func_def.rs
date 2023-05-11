use crate::opcode::FunctionId;
use crate::parser::prelude::*;

pub fn func_body<'s, 'fun, 'stack>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, 'fun, 'stack>,
) -> Result<(Lexer<'s>, FunctionId), LexParseError> {
    use crate::parser::block::block;
    use crate::tracker2::function::Function;

    let (s, ()) = match_token(s, Token::ParL)?;

    // Start function
    let mut fun = Function::new();
    let mut frag = Fragment::new(&mut fun, outer_frag.stack_mut().new_frame());

    // Currently this slot contains pointer to function itself.
    // In the future we will put environment here instead.
    frag.stack_mut().push()?;

    let mut parlist = |s: Lexer<'s>| -> Result<_, LexParseError> {
        let mut count = 0;

        let (mut s, ident) = identifier(s)?;
        let slot = frag.stack_mut().push()?;
        frag.stack_mut().give_name(slot, ident)?;
        count += 1;

        let mut next_ident = |s: Lexer<'s>| -> Result<_, LexParseError> {
            let (s, ()) = match_token(s, Token::Comma)?;
            let (s, ident) = identifier(s).require()?;

            let slot = frag.stack_mut().push()?;
            frag.stack_mut().give_name(slot, ident)?;
            count += 1;

            Ok(s)
        };

        loop {
            s = match next_ident(s.clone()) {
                Ok(s) => s,
                Err(_) => break,
            };
        }

        Ok((s, count))
    };

    let (s, param_count) = parlist(s.clone()).optional(s);
    let param_count = param_count.unwrap_or(0);

    // An extra stack slot is taken by function pointer itself.
    let height = param_count + 1;

    let (s, ()) = match_token(s, Token::ParR).require()?;
    let (s, ()) = block(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    // Finish function
    frag.commit();
    let fun = fun.resolve(height);
    let func_id = chunk.functions.push(fun)?;

    // Drop outer fragment to make sure we didn't mess up current function.
    drop(outer_frag);

    Ok((s, func_id))
}
