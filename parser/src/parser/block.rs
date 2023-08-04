use crate::parser::prelude::*;

pub(crate) fn block<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = CompleteOr<ParseFailure>,
    Failure = Never,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        let state = inner_block(frag.new_fragment())
            .parse_once(s)?
            .map_output(|_| {
                frag.commit_scope();
            });

        Ok(state)
    }
}

pub(crate) fn inner_block<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = CompleteOr<ParseFailure>,
    Failure = Never,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::stmt::return_::return_;
        use crate::parser::stmt::statement;

        let statement = |s: Lexer<'s>| statement(frag.new_fragment()).parse_once(s);

        let r = statement
            .repeat()
            .parse_once(s)?
            .and(return_(frag.new_fragment()).optional())?
            .map_output(move |_| {
                frag.commit();
            });

        Ok(r)
    }
}
