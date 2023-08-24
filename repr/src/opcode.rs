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
    BinOp(BinOp),
    UnaOp(UnaOp),
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
            BinOp(op) => format!("{:<10} [{op}]", "BinaryOp"),
            UnaOp(op) => format!("{:<10} [{op}]", "UnaryOp"),
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

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Ari(AriBinOp),
    Bit(BitBinOp),
    Rel(RelBinOp),
    Str(StrBinOp),
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Ari(t) => write!(f, "{}", t),
            BinOp::Bit(t) => write!(f, "{}", t),
            BinOp::Rel(t) => write!(f, "{}", t),
            BinOp::Str(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaOp {
    AriNeg,
    BitNot,
}

impl Display for UnaOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaOp::AriNeg => "-",
            UnaOp::BitNot => "~",
        };

        write!(f, "{}", s)
    }
}
