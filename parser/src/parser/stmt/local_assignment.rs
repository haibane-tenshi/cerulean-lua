use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn local_assignment<'s>(
    s: Lexer<'s>,
    mut frag: Fragment<'s, '_>,
) -> Result<(Lexer<'s>, ()), Error<ParseFailure>> {
    use crate::parser::expr::expr_list;
    use LocalAssignmentFailure::*;

    let (s, _) = match_token(s, Token::Local).map_parse(Local)?;

    let stack_start = frag.stack().top()?;

    let (s, idents) = ident_list(s).map_parse(Ident)?;
    let (s, _) = match_token(s, Token::EqualsSign).map_parse(EqualsSign)?;
    let (s, ()) = expr_list(s, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    let count: u32 = idents.len().try_into().unwrap();
    frag.emit_adjust_to(stack_start + count)?;

    for (ident, slot) in idents.into_iter().zip((stack_start.0..).map(StackSlot)) {
        frag.stack_mut().give_name(slot, ident)?;
    }

    frag.commit();
    Ok((s, ()))
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

fn ident_list(s: Lexer) -> Result<(Lexer, Vec<&str>), ParseError<IdentMismatch>> {
    let (mut s, (ident, _)) = identifier(s)?;
    let mut r = vec![ident];

    let mut next_part = |s| -> Result<_, IdentListSuccess> {
        use IdentListSuccess::*;

        let (s, _) = match_token(s, Token::Comma).map_err(Comma)?;
        let (s, (ident, _)) = identifier(s).map_err(Ident)?;
        r.push(ident);

        Ok((s, ()))
    };

    loop {
        s = match next_part(s.clone()) {
            Ok((s, _)) => s,
            Err(_err) => break,
        };
    }

    Ok((s, r))
}

enum IdentListSuccess {
    Comma(ParseError<TokenMismatch>),
    Ident(ParseError<IdentMismatch>),
}
