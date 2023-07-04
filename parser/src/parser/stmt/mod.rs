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

pub(crate) fn statement<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use assignment::assignment;
    use do_end::do_end;
    use generic_for::generic_for;
    use if_then::if_then;
    use local_assignment::local_assignment;
    use local_function::local_function;
    use numerical_for::numerical_for;
    use repeat_until::repeat_until;
    use while_do::while_do;

    let mut inner = || {
        let mut err = match semicolon(s.clone()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match do_end(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match if_then(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match while_do(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match repeat_until(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match numerical_for(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match generic_for(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match local_assignment(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match local_function(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        err |= match assignment(s.clone(), frag.new_fragment()) {
            Ok((s, ())) => return Ok((s, ())),
            Err(err) => err,
        };

        Err(err)
    };

    let r = inner()?;
    frag.commit();
    Ok(r)
}

fn semicolon(s: Lexer) -> Result<(Lexer, ()), Error<ParseFailure>> {
    match match_token(s, Token::Semicolon) {
        Ok((s, _)) => Ok((s, ())),
        Err(_) => {
            let err = ParseFailure {
                mode: FailureMode::Mismatch,
                cause: ParseCause::ExpectedStatement,
            };

            Err(Error::Parse(err))
        }
    }
}
