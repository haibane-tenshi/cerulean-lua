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

pub(in crate::parser) fn statement<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ()), Error<ParseFailure>> {
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
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(_) => {
                let err = ParseFailure {
                    mode: FailureMode::Mismatch,
                    cause: ParseCause::ExpectedStatement,
                };

                Error::Parse(err)
            }
        };

        err |= match do_end(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match if_then(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match while_do(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match repeat_until(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), _status)) => return Ok((s, (), Complete)),
            Err(err) => err,
        };

        err |= match numerical_for(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match generic_for(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match local_assignment(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), _status)) => return Ok((s, (), Complete)),
            Err(err) => err,
        };

        err |= match local_function(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status)),
            Err(err) => err,
        };

        err |= match assignment(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), _status)) => return Ok((s, (), Complete)),
            Err(err) => err,
        };

        Err(err)
    };

    let (s, (), _) = inner()?;
    frag.commit();
    Ok((s, (), ()))
}

fn semicolon(s: Lexer) -> Result<(Lexer, (), Complete), ParseError<TokenMismatch>> {
    let (s, _, status) = match_token(s, Token::Semicolon)?;
    Ok((s, (), status))
}
