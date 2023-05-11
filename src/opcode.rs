use std::fmt::Display;
use std::ops::{Add, AddAssign, Sub, SubAssign};

use rle_vec::RleVec;
use thiserror::Error;

use crate::index_vec::{Index, IndexVec};
use crate::tracker2::stack::StackOffset;
use crate::value::Literal;

#[derive(Debug, Copy, Clone)]
pub struct ConstId(pub u32);

#[derive(Debug, Error)]
#[error("constant count overflowed u32")]
pub struct ConstCapacityError;

impl Index for ConstId {
    type Error = ConstCapacityError;
    const MAX: Self = ConstId(u32::MAX);

    fn try_from(val: usize) -> Result<Self, Self::Error> {
        let inner = val.try_into().map_err(|_| ConstCapacityError)?;
        Ok(ConstId(inner))
    }

    fn into(self) -> usize {
        self.0.try_into().expect("u32 should fit into usize")
    }
}

impl Display for ConstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct StackSlot(pub u32);

impl StackSlot {
    pub(crate) fn prev(self) -> Option<Self> {
        let index = self.0.checked_sub(1)?;
        Some(StackSlot(index))
    }

    pub(crate) fn checked_sub(self, rhs: Self) -> Option<StackOffset> {
        let inner = self.0.checked_sub(rhs.0)?;
        Some(StackOffset(inner))
    }

    pub(crate) fn sub(self, rhs: Self) -> StackOffset {
        self.checked_sub(rhs).unwrap()
    }
}

impl AddAssign<u32> for StackSlot {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl Add<u32> for StackSlot {
    type Output = Self;

    fn add(mut self, rhs: u32) -> Self::Output {
        self += rhs;
        self
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct FunctionId(pub u32);

#[derive(Debug, Error)]
#[error("function count overflowed u32")]
pub struct FunctionCapacityError;

impl Index for FunctionId {
    type Error = FunctionCapacityError;
    const MAX: Self = FunctionId(u32::MAX);

    fn try_from(val: usize) -> Result<Self, Self::Error> {
        let inner = val.try_into().map_err(|_| FunctionCapacityError)?;
        Ok(FunctionId(inner))
    }

    fn into(self) -> usize {
        self.0.try_into().expect("u32 should fit into usize")
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash)]
pub struct InstrId(pub u32);

#[derive(Debug, Error)]
#[error("instruction count overflowed u32")]
pub struct InstrCountError;

impl InstrId {
    pub fn checked_sub_offset(self, rhs: InstrOffset) -> Option<Self> {
        let val = self.0.checked_sub(rhs.0)?;
        Some(InstrId(val))
    }

    pub fn checked_sub(self, rhs: InstrId) -> Option<InstrOffset> {
        let offset = self.0.checked_sub(rhs.0)?;
        Some(InstrOffset(offset))
    }
}

impl Index for InstrId {
    type Error = InstrCountError;
    const MAX: Self = InstrId(u32::MAX);

    fn try_from(val: usize) -> Result<Self, Self::Error> {
        let inner = val.try_into().map_err(|_| InstrCountError)?;
        Ok(InstrId(inner))
    }

    fn into(self) -> usize {
        self.0.try_into().expect("u32 should fit into usize")
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default, Hash)]
pub struct InstrOffset(pub u32);

impl AddAssign<u32> for InstrId {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl Add<u32> for InstrId {
    type Output = Self;

    fn add(mut self, rhs: u32) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign<InstrOffset> for InstrId {
    fn add_assign(&mut self, rhs: InstrOffset) {
        self.0 += rhs.0
    }
}

impl Add<InstrOffset> for InstrId {
    type Output = Self;

    fn add(mut self, rhs: InstrOffset) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<InstrOffset> for InstrId {
    fn sub_assign(&mut self, rhs: InstrOffset) {
        self.0 -= rhs.0
    }
}

impl Sub<InstrOffset> for InstrId {
    type Output = Self;

    fn sub(mut self, rhs: InstrOffset) -> Self::Output {
        self -= rhs;
        self
    }
}

impl Sub for InstrId {
    type Output = InstrOffset;

    fn sub(self, rhs: Self) -> Self::Output {
        let offset = self.0 - rhs.0;
        InstrOffset(offset)
    }
}

impl AddAssign<u32> for InstrOffset {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl Add<u32> for InstrOffset {
    type Output = Self;

    fn add(mut self, rhs: u32) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<u32> for InstrOffset {
    fn sub_assign(&mut self, rhs: u32) {
        self.0 -= rhs;
    }
}

impl Sub<u32> for InstrOffset {
    type Output = Self;

    fn sub(mut self, rhs: u32) -> Self::Output {
        self -= rhs;
        self
    }
}

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
