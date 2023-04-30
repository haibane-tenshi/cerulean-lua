use super::{LexParseError, NextToken};
use crate::lex::Lexer;
use crate::parser::tracker::ChunkTracker;

pub(super) fn literal<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::lex::{Number, Token};
    use crate::opcode::OpCode;
    use crate::parser::ParseError;
    use crate::value::Literal;

    let literal = match s.next_token()? {
        Token::Nil => Literal::Nil,
        Token::True => Literal::Bool(true),
        Token::False => Literal::Bool(false),
        Token::Numeral(Number::Int(value)) => Literal::Int(value),
        Token::Numeral(Number::Float(value)) => Literal::Float(value),
        Token::ShortLiteralString(value) => Literal::String(value.to_string()),
        _ => return Err(ParseError.into()),
    };

    let id = tracker.insert_literal(literal)?;
    tracker.current_mut()?.emit(OpCode::LoadConstant(id))?;

    Ok((s, ()))
}
