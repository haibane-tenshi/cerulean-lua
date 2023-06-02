use crate::codegen::fragment::FragmentId;
use crate::parser::block::BlockSuccess;
use crate::parser::prelude::*;
use thiserror::Error;

pub(crate) fn if_then<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), Complete), Error<ParseFailure>> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_adjusted_to_1;
    use IfThenFailure::*;

    let outer = outer_frag.id();

    let (s, _, Complete) = match_token(s, Token::If).map_parse(If)?;
    let (s, (), _) = expr_adjusted_to_1(s, chunk, outer_frag.new_fragment())
        .with_mode(FailureMode::Malformed)?;
    let (s, _, Complete) = match_token(s, Token::Then).map_parse(Then)?;

    let mut frag = outer_frag.new_fragment();
    frag.emit_jump_to(frag.id(), Some(false))?;

    let (mut s, (), _) = block(s, chunk, frag.new_fragment()).with_mode(FailureMode::Malformed)?;

    if match_token(s.clone(), Token::End).is_err() {
        frag.emit_jump_to(outer, None)?;
    }
    frag.commit();

    loop {
        s = match else_if_clause(s.clone(), outer, chunk, outer_frag.new_fragment()) {
            Ok((s, (), _)) => s,
            Err(_) => break,
        };
    }

    let (s, _, _) = else_clause(s.clone(), chunk, outer_frag.new_fragment()).optional(s);
    let (s, _, status) = match_token(s, Token::End).map_parse(End)?;

    outer_frag.commit();
    Ok((s, (), status))
}

#[derive(Debug, Error)]
pub(crate) enum IfThenFailure {
    #[error("missing `if` token")]
    If(#[source] TokenMismatch),
    #[error("missing `then` token")]
    Then(#[source] TokenMismatch),
    #[error("missing `end` token")]
    End(#[source] TokenMismatch),
}

impl HaveFailureMode for IfThenFailure {
    fn mode(&self) -> FailureMode {
        match self {
            IfThenFailure::If(_) => FailureMode::Mismatch,
            IfThenFailure::Then(_) => FailureMode::Malformed,
            IfThenFailure::End(_) => FailureMode::Malformed,
        }
    }
}

fn else_if_clause<'s>(
    s: Lexer<'s>,
    outer: FragmentId,
    chunk: &mut Chunk,
    mut frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), BlockSuccess), Error<ElseIfFailure>> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_adjusted_to_1;
    use ElseIfFailure::*;

    let (s, _, Complete) = match_token(s, Token::ElseIf).map_parse(ElseIf)?;
    let (s, (), _) = expr_adjusted_to_1(s, chunk, frag.new_fragment())
        .with_mode(FailureMode::Malformed)
        .map_parse(Expr)?;
    let (s, _, Complete) = match_token(s, Token::Then).map_parse(Then)?;

    frag.emit_jump_to(frag.id(), Some(false))?;

    let (s, (), status) = block(s, chunk, frag.new_fragment())
        .with_mode(FailureMode::Malformed)
        .map_parse(Expr)?;

    if match_token(s.clone(), Token::End).is_err() {
        frag.emit_jump_to(outer, None)?;
    }

    frag.commit();
    Ok((s, (), status))
}

enum ElseIfFailure {
    ElseIf(TokenMismatch),
    Expr(ParseFailure),
    Then(TokenMismatch),
}

fn else_clause<'s>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    frag: Fragment<'s, '_, '_>,
) -> Result<(Lexer<'s>, (), BlockSuccess), Error<ElseFailure>> {
    use crate::parser::block::block;
    use ElseFailure::*;

    let (s, _, Complete) = match_token(s, Token::Else).map_parse(Else)?;
    let (s, (), status) = block(s, chunk, frag)
        .with_mode(FailureMode::Malformed)
        .map_parse(Expr)?;

    Ok((s, (), status))
}

enum ElseFailure {
    Else(TokenMismatch),
    Expr(ParseFailure),
}
