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

use crate::parser::expr::{ExprListSuccess, ExprSuccess};
use crate::parser::prelude::*;

pub(crate) fn statement<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), StatementSuccess), Error<ParseFailure>> {
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
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match do_end(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match if_then(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match while_do(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match repeat_until(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match numerical_for(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match generic_for(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match local_assignment(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match local_function(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        err |= match assignment(s.clone(), chunk, frag.new_fragment()) {
            Ok((s, (), status)) => return Ok((s, (), status.into())),
            Err(err) => err,
        };

        Err(err)
    };

    let r = inner()?;
    frag.commit();
    Ok(r)
}

pub(crate) enum StatementSuccess {
    Complete(Complete),
    Expr(ExprSuccess),
    ExprList(ExprListSuccess),
}

impl From<Complete> for StatementSuccess {
    fn from(value: Complete) -> Self {
        StatementSuccess::Complete(value)
    }
}

impl From<ExprSuccess> for StatementSuccess {
    fn from(value: ExprSuccess) -> Self {
        StatementSuccess::Expr(value)
    }
}

impl From<ExprListSuccess> for StatementSuccess {
    fn from(value: ExprListSuccess) -> Self {
        StatementSuccess::ExprList(value)
    }
}

fn semicolon(s: Lexer) -> Result<(Lexer, (), Complete), Error<ParseFailure>> {
    match match_token(s, Token::Semicolon) {
        Ok((s, _, status)) => Ok((s, (), status)),
        Err(_) => {
            let err = ParseFailure {
                mode: FailureMode::Mismatch,
                cause: ParseCause::ExpectedStatement,
            };

            Err(Error::Parse(err))
        }
    }
}
