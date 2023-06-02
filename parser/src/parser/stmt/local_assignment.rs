use crate::parser::expr::ExprListSuccess;
use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn local_assignment<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), ExprListSuccess), Error<ParseFailure>> {
    use crate::parser::expr::expr_list;
    use LocalAssignmentFailure::*;

    let (s, _, Complete) = match_token(s, Token::Local).map_parse(Local)?;

    let stack_start = frag.stack().top()?;

    let (s, idents, _) = ident_list(s).map_parse(Ident)?;
    let (s, _, Complete) = match_token(s, Token::EqualsSign).map_parse(EqualsSign)?;
    let (s, (), status) =
        expr_list(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    let count: u32 = idents.len().try_into().unwrap();
    frag.emit_adjust_to(stack_start + count)?;

    for (ident, slot) in idents.into_iter().zip((stack_start.0..).map(StackSlot)) {
        frag.stack_mut().give_name(slot, ident)?;
    }

    frag.commit();
    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub enum LocalAssignmentFailure {
    #[error("missing `local` keyword")]
    Local(#[source] TokenMismatch),
    #[error("assignment list should have at least one identifier")]
    Ident(#[source] IdentMismatch),
    #[error("missing equals sign")]
    EqualsSign(#[source] TokenMismatch),
}

impl HaveFailureMode for LocalAssignmentFailure {
    fn mode(&self) -> FailureMode {
        match self {
            LocalAssignmentFailure::Local(_) => FailureMode::Mismatch,
            LocalAssignmentFailure::Ident(_) => FailureMode::Ambiguous,
            LocalAssignmentFailure::EqualsSign(_) => FailureMode::Ambiguous,
        }
    }
}

fn ident_list(s: Lexer) -> Result<(Lexer, Vec<&str>, IdentListSuccess), ParseError<IdentMismatch>> {
    let (mut s, (ident, _), Complete) = identifier(s)?;
    let mut r = vec![ident];

    let mut next_part = |s| {
        use IdentListSuccess::*;

        let (s, _, _) = match_token(s, Token::Comma).map_err(Comma)?;
        let (s, (ident, _), status) = identifier(s).map_err(Ident)?;
        r.push(ident);

        Ok((s, (), status))
    };

    let status = loop {
        s = match next_part(s.clone()) {
            Ok((s, _, _)) => s,
            Err(err) => break err,
        };
    };

    Ok((s, r, status))
}

enum IdentListSuccess {
    Comma(ParseError<TokenMismatch>),
    Ident(ParseError<IdentMismatch>),
}
