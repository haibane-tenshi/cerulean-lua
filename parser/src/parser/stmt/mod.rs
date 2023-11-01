pub(crate) mod assignment;
pub(crate) mod break_;
pub(crate) mod do_end;
pub(crate) mod generic_for;
pub(crate) mod goto;
pub(crate) mod if_then;
pub(crate) mod label;
pub(crate) mod local_assignment;
pub(crate) mod local_function;
pub(crate) mod numerical_for;
pub(crate) mod repeat_until;
pub(crate) mod return_;
pub(crate) mod while_do;

use crate::parser::prelude::*;

pub(crate) fn statement<'s, 'origin>(
    core: Core<'s, 'origin>,
) -> impl ParseOnce<
    Lexer<'s>,
    Output = Spanned<()>,
    Success = ParseFailureOrComplete,
    Failure = ParseFailure,
    FailFast = FailFast,
> + 'origin {
    move |s: Lexer<'s>| {
        use crate::parser::prefix_expr::func_call;
        use assignment::assignment;
        use break_::break_;
        use do_end::do_end;
        use generic_for::generic_for;
        use goto::goto;
        use if_then::if_then;
        use label::label;
        use local_assignment::local_assignment;
        use local_function::local_function;
        use numerical_for::numerical_for;
        use repeat_until::repeat_until;
        use while_do::while_do;

        let mut frag = core.decl();

        let state = Source(s)
            .or(semicolon)?
            .map_success(ParseFailureOrComplete::Complete)
            .or(local_assignment(frag.new_core()))?
            .or(assignment(frag.new_core()))?
            .or(if_then(frag.new_core()))?
            .or(numerical_for(frag.new_core()))?
            .or(generic_for(frag.new_core()))?
            .or(local_function(frag.new_core()))?
            .or(func_call(frag.new_core()))?
            .or(while_do(frag.new_core()))?
            .or(break_(frag.new_core()))?
            .or(do_end(frag.new_core()))?
            .or(repeat_until(frag.new_core()))?
            .or(label(frag.new_core()))?
            .or(goto(frag.new_core()))?
            .else_failure(|mut s, f| {
                let _ = s.next_token();

                let err = ParseFailure::from(ParseCause::ExpectedStmt(s.span()));

                f.arrow(err)
            })
            .inspect(|_| frag.commit());

        Ok(state)
    }
}

fn semicolon(
    s: Lexer,
) -> Result<ParsingState<Lexer, (), Spanned<()>, Complete, ParseFailure>, LexError> {
    match_token(Token::Semicolon)
        .map_failure(|t: TokenMismatch| ParseFailure {
            mode: FailureMode::Mismatch,
            cause: ParseCause::ExpectedStmt(t.span),
        })
        .parse(s)
}
