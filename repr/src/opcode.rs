use std::fmt::Display;

use crate::index::{ConstId, InstrOffset, StackSlot};

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Panic,
    Invoke(StackSlot),
    Return(StackSlot),
    LoadConstant(ConstId),
    LoadStack(StackSlot),
    StoreStack(StackSlot),
    AdjustStack(StackSlot),
    AriUnaOp(AriUnaOp),
    AriBinOp(AriBinOp),
    BitUnaOp(BitUnaOp),
    BitBinOp(BitBinOp),
    RelBinOp(RelBinOp),
    StrBinOp(StrBinOp),
    Jump { offset: InstrOffset },
    JumpIf { cond: bool, offset: InstrOffset },
    Loop { offset: InstrOffset },
    LoopIf { cond: bool, offset: InstrOffset },
    TabCreate,
    TabGet,
    TabSet,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpCode::*;

        let s = match *self {
            Panic => format!("{:<10}", "Panic"),
            Invoke(StackSlot(index)) => format!("{:<10} [{index:>3}]", "Invoke"),
            Return(StackSlot(index)) => format!("{:<10} [{index:>3}]", "Return"),
            LoadConstant(ConstId(index)) => format!("{:<10} [{index:>3}]", "LoadConst"),
            LoadStack(StackSlot(index)) => format!("{:<10} [{index:>3}]", "LoadStack"),
            StoreStack(StackSlot(index)) => format!("{:<10} [{index:>3}]", "StoreStack"),
            AdjustStack(StackSlot(height)) => format!("{:<10} [{height:>3}]", "AdjustStack"),
            AriUnaOp(op) => format!("{:<10} [{op}]", "AriUnaOp"),
            AriBinOp(op) => format!("{:<10} [{op}]", "AriBinOp"),
            BitUnaOp(op) => format!("{:<10} [{op}]", "BitUnaOp"),
            BitBinOp(op) => format!("{:<10} [{op}]", "BitBinOp"),
            RelBinOp(op) => format!("{:<10} [{op}]", "RelBinOp"),
            StrBinOp(op) => format!("{:<10} [{op}]", "StrBinOp"),
            Jump { offset } => format!("{:<10} [{:>3}]", "Jump", offset.0),
            JumpIf { cond, offset } => format!("{:<10} [{cond:>5}] [{:>3}]", "JumpIf", offset.0),
            Loop { offset } => format!("{:<10} [{:>3}]", "Loop", offset.0),
            LoopIf { cond, offset } => format!("{:<10} [{cond:>5}] [{:>3}]", "LoopIf", offset.0),
            TabCreate => format!("{:<10}", "TabCreate"),
            TabSet => format!("{:<10}", "TabSet"),
            TabGet => format!("{:<10}", "TabGet"),
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
