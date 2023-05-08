use crate::opcode::FunctionId;
use crate::parser::prelude::*;

pub fn func_body<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, FunctionId), LexParseError> {
    use crate::parser::block::block;

    let (s, ()) = match_token(s, Token::ParL)?;

    // Start function
    tracker.start_fn()?;

    // Currently this slot contains pointer to function itself.
    // In the future we will put environment here instead.
    tracker.current_mut()?.push_stack(None)?;

    let mut parlist = |s: Lexer<'s>| -> Result<_, LexParseError> {
        let mut count = 0;

        let (mut s, ident) = identifier(s)?;
        tracker.current_mut()?.push_stack(Some(ident))?;
        count += 1;

        let mut next_ident = |s: Lexer<'s>| -> Result<_, LexParseError> {
            let (s, ()) = match_token(s, Token::Comma)?;
            let (s, ident) = identifier(s).require()?;

            tracker.current_mut()?.push_stack(Some(ident))?;
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
    let (s, ()) = block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    // Finish function
    let func_id = tracker.finish_fn(height).map_err(|_| ParseError)?;

    Ok((s, func_id))
}
