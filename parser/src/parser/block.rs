use crate::parser::prelude::*;
use crate::parser::stmt::return_::{ReturnFailure, ReturnSuccess};
use either::Either;

pub(crate) fn block<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), BlockSuccess), Error<ParseFailure>> {
    let r = inner_block(s, chunk, frag.new_fragment())?;

    frag.commit_scope();
    Ok(r)
}

pub(crate) fn inner_block<'s>(
    mut s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), BlockSuccess), Error<ParseFailure>> {
    use crate::parser::stmt::return_::return_;
    use crate::parser::stmt::statement;

    let _ = loop {
        s = match statement(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, _, _)) => s,
            Err(err) => break err,
        };
    };

    let (s, _, status) = return_(s.clone(), chunk, frag.new_fragment()).optional(s);

    frag.commit();
    Ok((s, (), BlockSuccess(status)))
}

pub(crate) struct BlockSuccess(Either<ReturnSuccess, Error<ReturnFailure>>);
