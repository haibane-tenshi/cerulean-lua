use crate::value::Value;
use std::fmt::Display;

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

        for (i, code) in self.codes.iter().copied().enumerate() {
            use OpCode::*;

            match code {
                Return => writeln!(f, "{i:04} {code:<9}")?,
                LoadConstant(index) => {
                    let constant = self
                        .get_constant(index)
                        .map(|t| t.to_string())
                        .unwrap_or_else(|| "<FAILED TO RESOLVE CONSTANT>".to_string());

                    writeln!(f, "{i:04} {code:<9} {index:03} {constant}")?;
                }
            }
        }

        Ok(())
    }
}
