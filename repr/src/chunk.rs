use std::fmt::Display;

use rle_vec::RleVec;

use crate::index::{ConstId, FunctionId, InstrId, StackSlot, UpvalueSlot};
use crate::literal::Literal;
use crate::opcode::OpCode;
use crate::tivec::TiVec;

#[derive(Debug, Clone)]
pub struct Chunk {
    pub functions: TiVec<FunctionId, Function>,
    pub constants: TiVec<ConstId, Literal>,
}

impl Chunk {
    pub fn get_constant(&self, index: ConstId) -> Option<&Literal> {
        self.constants.get(index)
    }

    pub fn get_function(&self, index: FunctionId) -> Option<&Function> {
        self.functions.get(index)
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "== chunk ==")?;

        writeln!(f, "== constant table ==")?;
        for (i, literal) in self.constants.iter().enumerate() {
            writeln!(f, "[{i:03}] {literal}")?;
        }

        writeln!(f)?;

        writeln!(f, "== function table ==")?;
        for (i, fun) in self.functions.iter().enumerate() {
            writeln!(f, ":: function id {i:3} ::")?;
            writeln!(f, "{fun}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub codes: TiVec<InstrId, OpCode>,
    pub lines: RleVec<u32>,
    pub signature: Signature,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[derive(Copy, Clone)]
        enum LineNumber {
            Explicit(u32),
            Repeat,
        }

        writeln!(f, "# signature")?;
        let arg_count = self.signature.height.checked_sub(1).unwrap_or_default();
        writeln!(
            f,
            "  height:   {} ({arg_count} real args)",
            self.signature.height
        )?;
        writeln!(f, "  variadic: {}", self.signature.is_variadic)?;

        writeln!(f, "  upvalues: {}", self.signature.upvalues.len())?;
        if !self.signature.upvalues.is_empty() {
            for (i, upvalue) in self.signature.upvalues.iter().enumerate() {
                writeln!(f, "    [{i:02}] {upvalue:?}")?;
            }
        }

        writeln!(f)?;
        writeln!(f, "# body")?;

        let iter = self.codes.iter().copied().enumerate().zip(
            self.lines
                .runs()
                .flat_map(|run| {
                    std::iter::once(LineNumber::Explicit(*run.value))
                        .chain(std::iter::repeat(LineNumber::Repeat).take(run.len - 1))
                })
                .map(Some)
                .chain(std::iter::repeat(None)),
        );

        for ((i, code), line) in iter {
            let line = match line {
                Some(LineNumber::Explicit(n)) => n.to_string(),
                Some(LineNumber::Repeat) => "|".to_string(),
                None => "?".to_string(),
            };

            writeln!(f, "  {i:04} {line:>3} {code}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Signature {
    pub height: usize,
    pub is_variadic: bool,
    pub upvalues: TiVec<UpvalueSlot, UpvalueSource>,
}

#[derive(Debug, Copy, Clone)]
pub enum UpvalueSource {
    Temporary(StackSlot),
    Upvalue(UpvalueSlot),
}
