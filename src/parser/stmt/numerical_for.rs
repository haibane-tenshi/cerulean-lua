use crate::parser::prelude::*;
use crate::tracker2::fragment::EmitError;

#[derive(Debug, Copy, Clone)]
enum StackSlotOrConstId {
    StackSlot(StackSlot),
    ConstId(ConstId),
}

impl StackSlotOrConstId {
    fn load(self, frag: &mut Fragment) -> Result<InstrId, EmitError> {
        use StackSlotOrConstId::*;

        match self {
            StackSlot(slot) => frag.emit(OpCode::LoadStack(slot)),
            ConstId(const_id) => frag.emit(OpCode::LoadConstant(const_id)),
        }
    }
}

pub(super) fn numerical_for<'s, 'fun, 'stack>(
    s: Lexer<'s>,
    chunk: &mut Chunk,
    mut outer_frag: Fragment<'s, 'fun, 'stack>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    use crate::parser::block::block;
    use crate::parser::expr::expr_adjusted_to_1;

    let exit = outer_frag.id();

    let (s, ()) = match_token(s, Token::For)?;
    let (s, ident) = identifier(s).require()?;
    let (s, ()) = match_token(s, Token::EqualsSign).require()?;

    let loop_var = outer_frag.stack().top()?;
    let (s, ()) = expr_adjusted_to_1(s, chunk, outer_frag.new_fragment()).require()?;
    outer_frag.stack_mut().give_name(loop_var, ident)?;

    let (s, ()) = match_token(s, Token::Comma).require()?;

    let limit = outer_frag.stack().top()?;
    let (s, ()) = expr_adjusted_to_1(s, chunk, outer_frag.new_fragment()).require()?;

    let mut maybe_step = |s: Lexer<'s>| -> Result<(Lexer<'s>, StackSlot), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma).require()?;

        let step = outer_frag.stack().top()?;
        let (s, ()) = expr_adjusted_to_1(s, chunk, outer_frag.new_fragment()).require()?;

        Ok((s, step))
    };

    let (s, step) = maybe_step(s.clone()).optional(s);

    let step = match step {
        Some(slot) => StackSlotOrConstId::StackSlot(slot),
        None => {
            let const_id = chunk.constants.insert(Literal::Int(1))?;
            StackSlotOrConstId::ConstId(const_id)
        }
    };

    // Loop controls.

    let zero = chunk.constants.insert(Literal::Int(0))?;

    // Panic if increment is 0.
    if let StackSlotOrConstId::StackSlot(step) = step {
        let error_msg = chunk
            .constants
            .insert(Literal::String("loop increment is 0".to_string()))?;

        let mut zero_check = outer_frag.new_fragment();

        zero_check.emit(OpCode::LoadStack(step))?;
        zero_check.emit(OpCode::LoadConstant(zero))?;
        zero_check.emit(OpCode::RelBinOp(RelBinOp::Eq))?;
        zero_check.emit_jump_to(zero_check.id(), Some(false))?;

        // Panic path.
        zero_check.emit(OpCode::LoadConstant(error_msg))?;
        zero_check.emit(OpCode::Panic)?;

        // Happy path.
        zero_check.commit();
    }

    let mut frag = outer_frag.new_fragment();
    let mut controls = frag.new_fragment();
    let controls_id = controls.id();
    let mut positive_step = controls.new_fragment();

    step.load(&mut positive_step)?;
    positive_step.emit(OpCode::LoadConstant(zero))?;
    positive_step.emit(OpCode::RelBinOp(RelBinOp::Gt))?;
    positive_step.emit_jump_to(positive_step.id(), Some(false))?;

    // Path: positive step.
    positive_step.emit(OpCode::LoadStack(loop_var))?;
    positive_step.emit(OpCode::LoadStack(limit))?;
    positive_step.emit(OpCode::RelBinOp(RelBinOp::Le))?;
    positive_step.emit_jump_to(exit, Some(false))?;
    positive_step.emit_jump_to(controls_id, None)?;

    positive_step.commit();

    // Path: negative step.
    // We assume total ordering for the variable.
    controls.emit(OpCode::LoadStack(loop_var))?;
    controls.emit(OpCode::LoadStack(limit))?;
    controls.emit(OpCode::RelBinOp(RelBinOp::Ge))?;
    controls.emit_jump_to(exit, Some(false))?;

    controls.commit();

    let (s, ()) = match_token(s, Token::Do).require()?;
    let (s, ()) = block(s, chunk, frag.new_fragment()).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    // Increment control variable.
    frag.emit(OpCode::LoadStack(loop_var))?;
    step.load(&mut frag)?;
    frag.emit(OpCode::AriBinOp(AriBinOp::Add))?;
    frag.emit(OpCode::StoreStack(loop_var))?;
    frag.emit_loop_to()?;

    // Clean up.
    frag.commit();
    outer_frag.commit_scope();

    Ok((s, ()))
}
