use std::fmt::Display;

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Return,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub codes: Vec<OpCode>,
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "== chunk ==")?;

        for (i, code) in self.codes.iter().copied().enumerate() {
            writeln!(f, "{i:04} {code}")?;
        }

        Ok(())
    }
}
