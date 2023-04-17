use std::fmt::Display;

use rle_vec::RleVec;

use crate::value::Value;

pub type Index = u32;

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Return,
    LoadConstant(Index),
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpCode::*;

        let s = match *self {
            Return => "Return",
            LoadConstant(_) => "LoadConst",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub codes: Vec<OpCode>,
    pub constants: Vec<Value>,
    pub lines: RleVec<u32>,
}

impl Chunk {
    pub fn get_constant(&self, index: Index) -> Option<&Value> {
        let index: usize = index.try_into().ok()?;
        self.constants.get(index)
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "== chunk ==")?;

        #[derive(Copy, Clone)]
        enum LineNumber {
            Explicit(u32),
            Repeat,
        }

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
            use OpCode::*;

            let line = match line {
                Some(LineNumber::Explicit(n)) => n.to_string(),
                Some(LineNumber::Repeat) => "|".to_string(),
                None => "?".to_string(),
            };

            write!(f, "{i:04} {line:>3} {code:<9}")?;

            match code {
                Return => (),
                LoadConstant(index) => {
                    let constant = self
                        .get_constant(index)
                        .map(|t| t.to_string())
                        .unwrap_or_else(|| "<FAILED TO RESOLVE CONSTANT>".to_string());

                    writeln!(f, " {index:03} {constant}")?;
                }
            }
        }

        Ok(())
    }
}
