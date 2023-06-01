use std::fmt::Display;

use rle_vec::RleVec;

use crate::index::{ConstId, FunctionId, InstrId};
use crate::index_vec::IndexVec;
use crate::literal::Literal;
use crate::opcode::OpCode;

#[derive(Debug, Clone)]
pub struct Chunk {
    pub functions: IndexVec<FunctionId, Function>,
    pub constants: IndexVec<ConstId, Literal>,
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

        writeln!(f, "==constant table==")?;
        for (i, literal) in self.constants.iter().enumerate() {
            writeln!(f, "[{i:03}] {literal}")?;
        }

        writeln!(f)?;

        writeln!(f, "==function table==")?;
        for (i, fun) in self.functions.iter().enumerate() {
            writeln!(f, "  function slot [{i:3}]")?;
            write!(f, "{fun}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub codes: IndexVec<InstrId, OpCode>,
    pub lines: RleVec<u32>,
    pub height: u32,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[derive(Copy, Clone)]
        enum LineNumber {
            Explicit(u32),
            Repeat,
        }

        writeln!(f, "initial stack: {}", self.height)?;

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

            writeln!(f, "{i:04} {line:>3} {code}")?;
        }

        Ok(())
    }
}
