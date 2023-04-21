use std::fmt::Display;
use std::num::NonZeroU32;

use rle_vec::RleVec;

use crate::value::Literal;

pub type Index = u32;

#[derive(Debug, Copy, Clone)]
pub struct ConstId(pub Index);

impl Display for ConstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct StackSlot(pub Index);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct FunctionId(pub Index);

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Invoke(StackSlot),
    Return,
    LoadConstant(ConstId),
    LoadStack(StackSlot),
    StoreStack(StackSlot),
    AdjustStack(StackSlot),
    #[deprecated]
    PopStack(NonZeroU32),
    AriUnaOp(AriUnaOp),
    AriBinOp(AriBinOp),
    BitUnaOp(BitUnaOp),
    BitBinOp(BitBinOp),
    RelBinOp(RelBinOp),
    StrBinOp(StrBinOp),
    Jump {
        offset: u32,
    },
    JumpIf {
        cond: bool,
        offset: u32,
    },
    Loop {
        offset: u32,
    },
    LoopIf {
        cond: bool,
        offset: u32,
    },
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpCode::*;

        let s = match *self {
            Invoke(StackSlot(index)) => format!("{:<10} [{index:>3}]", "Invoke"),
            Return => "Return".to_string(),
            LoadConstant(ConstId(index)) => format!("{:<10} [{index:>3}]", "LoadConst"),
            LoadStack(StackSlot(index)) => format!("{:<10} [{index:>3}]", "LoadStack"),
            StoreStack(StackSlot(index)) => format!("{:<10} [{index:>3}]", "StoreStack"),
            AdjustStack(StackSlot(height)) => format!("{:<10} [{height:>3}]", "AdjustStack"),
            PopStack(count) => format!("{:<10} [{count:>3}]", "PopStack"),
            AriUnaOp(op) => format!("{:<10} [{op}]", "AriUnaOp"),
            AriBinOp(op) => format!("{:<10} [{op}]", "AriBinOp"),
            BitUnaOp(op) => format!("{:<10} [{op}]", "BitUnaOp"),
            BitBinOp(op) => format!("{:<10} [{op}]", "BitBinOp"),
            RelBinOp(op) => format!("{:<10} [{op}]", "RelBinOp"),
            StrBinOp(op) => format!("{:<10} [{op}]", "StrBinOp"),
            Jump { offset } => format!("{:<10} [{offset:>3}]", "Jump"),
            JumpIf { cond, offset } => format!("{:<10} [{cond:>5}] [{offset:>3}]", "JumpIf"),
            Loop { offset } => format!("{:<10} [{offset:>3}]", "Loop"),
            LoopIf { cond, offset } => format!("{:<10} [{cond:>5}] [{offset:>3}]", "LoopIf"),
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AriUnaOp {
    Neg,
}

impl Display for AriUnaOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match *self {
            AriUnaOp::Neg => '-',
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AriBinOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Rem,
    Exp,
}

impl Display for AriBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AriBinOp::*;

        let s = match *self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            FloorDiv => "//",
            Rem => "%",
            Exp => "^",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BitUnaOp {
    Not,
}

impl Display for BitUnaOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match *self {
            BitUnaOp::Not => "~",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BitBinOp {
    And,
    Or,
    Xor,
    ShL,
    ShR,
}

impl Display for BitBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match *self {
            BitBinOp::And => "&",
            BitBinOp::Or => "|",
            BitBinOp::Xor => "~",
            BitBinOp::ShL => "<<",
            BitBinOp::ShR => ">>",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone)]
pub enum RelBinOp {
    Eq,
    Neq,
    Le,
    Lt,
    Ge,
    Gt,
}

impl Display for RelBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match *self {
            RelBinOp::Eq => "==",
            RelBinOp::Neq => "~=",
            RelBinOp::Le => "<",
            RelBinOp::Lt => "<=",
            RelBinOp::Ge => ">",
            RelBinOp::Gt => ">=",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone)]
pub enum StrBinOp {
    Concat,
}

impl Display for StrBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match *self {
            StrBinOp::Concat => "..",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub functions: Vec<Function>,
    pub constants: Vec<Literal>,
}

impl Chunk {
    pub fn get_constant(&self, index: ConstId) -> Option<&Literal> {
        let index: usize = index.0.try_into().ok()?;
        self.constants.get(index)
    }

    pub fn get_function(&self, index: FunctionId) -> Option<&Function> {
        let index: usize = index.0.try_into().ok()?;
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
    pub codes: Vec<OpCode>,
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
