use thiserror::Error;

use crate::opcode::ConstCapacityError;
use crate::parser2::basic;
use crate::parser2::prelude::*;
use crate::tracker2::EmitError;

#[derive(Debug, Error)]
pub(crate) enum ParseReason {
    #[error("local assignments must start with `local` keyword")]
    MissingLocal(basic::Reason),
    #[error("expected identifier for new temporary")]
    ExpectedIdent(basic::Reason),
    #[error("missing equals sign")]
    MissingEqual(basic::Reason),
    #[error("only literals are supported right now")]
    ExpectedLiteral(basic::Reason),
}

#[derive(Debug, Error)]
pub(crate) enum CodegenReason {
    #[error("failed to emit opcode")]
    Emit(#[from] EmitError),

    #[error("exceeded const table capacity")]
    ConstCapacity(#[from] ConstCapacityError),
}

#[derive(Debug, Error)]
pub(crate) enum Reason {
    #[error("failed to parse local assignment")]
    Parse(#[from] ParseReason),

    #[error("failed to generate code")]
    Codegen(#[from] CodegenReason),
}

impl From<EmitError> for Reason {
    fn from(value: EmitError) -> Self {
        Reason::Codegen(value.into())
    }
}

impl From<ConstCapacityError> for Reason {
    fn from(value: ConstCapacityError) -> Self {
        Reason::Codegen(value.into())
    }
}

pub(crate) fn local_assignment<'s, 'a>(
    stack: StackView<'s, 'a>,
    constants: &'a mut ConstTracker,
) -> impl Parser<Lexer<'s>, Fragment, Complete, Reason> + 'a {
    move |s: Lexer<'s>| {
        let mut frag = FragmentBuilder::new(stack);

        let (s, _, Complete) = token(Token::Local)
            .parse(s)
            .map_err(ParseReason::MissingLocal)?;
        let (s, _name, Complete) = identifier().parse(s).map_err(ParseReason::ExpectedIdent)?;
        let (s, _, Complete) = token(Token::Assign)
            .parse(s)
            .map_err(ParseReason::MissingEqual)?;
        let (s, (literal, _), Complete) =
            literal().parse(s).map_err(ParseReason::ExpectedLiteral)?;

        let const_id = constants.insert(literal)?;
        frag.emit(OpCode::LoadConstant(const_id))?;

        let frag = frag.finalize();

        Ok((s, frag, Complete))
    }
}
