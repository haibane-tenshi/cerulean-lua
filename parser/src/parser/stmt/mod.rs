pub(crate) mod assignment;
pub(crate) mod do_end;
pub(crate) mod generic_for;
pub(crate) mod if_then;
pub(crate) mod local_assignment;
pub(crate) mod local_function;
pub(crate) mod numerical_for;
pub(crate) mod repeat_until;
pub(crate) mod return_;
pub(crate) mod while_do;

use crate::parser::prelude::*;

pub(crate) fn statement<'s, 'origin>(
    mut frag: Fragment<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = (),
    Success = ParseFailureOrComplete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use assignment::assignment;
        use do_end::do_end;
        use generic_for::generic_for;
        use if_then::if_then;
        use local_assignment::local_assignment;
        use local_function::local_function;
        use numerical_for::numerical_for;
        use repeat_until::repeat_until;
        use while_do::while_do;

        let state = semicolon(s.clone())?
            .map_success(ParseFailureOrComplete::Complete)
            .or_else(|| (s.clone(), local_assignment(frag.new_fragment())))?
            .or_else(|| (s.clone(), assignment(frag.new_fragment())))?
            .or_else(|| (s.clone(), if_then(frag.new_fragment())))?
            .or_else(|| (s.clone(), numerical_for(frag.new_fragment())))?
            .or_else(|| (s.clone(), generic_for(frag.new_fragment())))?
            .or_else(|| (s.clone(), local_function(frag.new_fragment())))?
            .or_else(|| (s.clone(), while_do(frag.new_fragment())))?
            .or_else(|| (s.clone(), do_end(frag.new_fragment())))?
            .or_else(|| (s.clone(), repeat_until(frag.new_fragment())))?
            .map_failure(|f| {
                let mut s = s;
                let _ = s.next_token();

                let err = ParseFailure::from(ParseCause::ExpectedStmt(s.span()));

                f.arrow(err)
            })
            .map_output(|_| frag.commit_decl());

        Ok(state)
    }
}

fn semicolon(s: Lexer) -> Result<ParsingState<Lexer, (), Complete, ParseFailure>, LexError> {
    match_token(Token::Semicolon)
        .map_failure(|t: TokenMismatch| ParseFailure {
            mode: FailureMode::Mismatch,
            cause: ParseCause::ExpectedStmt(t.span),
        })
        .map_output(|_| ())
        .parse(s)
}
