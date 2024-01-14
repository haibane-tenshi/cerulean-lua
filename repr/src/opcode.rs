//! Opcodes are used to modify runtime's internal state in a specific way.
//!
//! In order to understand opcode semantics we must first understand how runtime
//! itself is organized.
//!
//! # Runtime structure
//!
//! Our runtime is a [stack machine](https://en.wikipedia.org/wiki/Stack_machine),
//! e.g. it is accompanied by a stack of temporaries (simply referred to as "stack")
//! which is used as a primary storage space during execution.
//! Most opcodes either put/take values from top of the stack or target specific places on it.
//!
//! # Lua Function ABI
//!
//! Every function is in its essence just a `Vec<OpCode>`.
//! Execution starts with the first opcode and ends upon either executing terminal opcode
//! or exhausting instruction list, which is equivalent to clearing stack and returning.
//!
//! Function have access to multiple storage spaces which may contain data.
//!
//! ## Stack
//!
//! Every function upon being entered gets its own stack space isolated from other functions.
//! Addressing always starts with `StackSlot(0)`.
//!
//! Most opcodes access stack in one way or another.
//!
//! Stack is also used to communicate with other functions.
//! Any values that function finds on the stack upon starting execution are to be considered arguments,
//! and any values left after the execution are to be considered return values.
//! By convention arguments are placed on the stack in the same order as they are declared in the source.
//!
//! Lua functions only get special treatment from runtime:
//! the number of values on the stack when function is entered the first time will be adjusted to number of
//! arguments reqested through function's [`Signature`](crate::chunk::Signature).
//! This doesn't apply to Rust functions invoked through FFI:
//! those will need to handle provided argument list on their own.
//!
//! Lua functions have variadic returns,
//! e.g. there can be any number of return values and it can differ from one call to another.
//! This is not part of function signature.
//!
//! ## Upvalues
//!
//! Every Lua function get exactly `upvalue_count` upvalue slots as specified in its [`Signature`](crate::chunk::Signature).
//! This number cannot be altered while function is running.
//! Rust functions invoked through FFI have no access to upvalues.
//!
//! Upvalue refers to *place* (e.g. storage location) external to current function
//! as opposed to stack variable which is local to it.
//! Otherwise it behaves similarly to on-stack variable.
//! Upvalues are only resolved at runtime and can be initialized with help of [`ClosureRecipe`](crate::chunk::ClosureRecipe).
//!
//! There are only two opcodes that can access upvalues: `StoreUpvalue` and `LoadUpvalue`.
//!
//! ## Constant table
//!
//! Lua function get direct access to constant table of the chunk it is defined in.
//! As the name suggests it contains predefined constants which can be loaded onto the stack at any time.
//! Compile time constants are more restrictive than you might think:
//! tables, userdata and any closures require garbage collector to exist,
//! so the only things that can be put into constant tables are [literals](crate::literal::Literal):
//! `nil`, booleans, integers, floats and strings.
//!
//! There is only one opcode that can access constant table: `LoadConstant`.
//!
//! ## Variadic register
//!
//! Variadic register contains any variadic args (above `arg_count`) that function received upon call,
//! as such it may contain an arbitrary number of values.
//! The register is always accessible but it will be empty unless function requests varargs
//! in its [`Signature`](crate::chunk::Signature).
//! Otherwise even if extra arguments are provided those will be discarded.
//!
//! The register is read-only, and can be accessed only with one opcode: `LoadVariadic`.
//! Values are cloned on read, so you can read from register multiple times.
//!
//! ### Why it exists
//!
//! The reason why variadic register exists is straightforward:
//! we address places on stack starting from the base,
//! which means if we keep varargs on stack any value put on top of them
//! does not have statically known address.
//! Which is a problem considering basically everything that happens inside the function
//! will be placed there.
//! We solve the issue by providing its own space for extra arguments.

use std::fmt::Display;

use crate::index::{ConstId, InstrOffset, RecipeId, StackSlot, UpvalueSlot};

/// A single instruction executable by runtime.
///
/// # Overview
///
/// Below are groupings of opcodes based on their functionality.
///
/// ## Terminating
///
/// Terminating opcodes halt execution of current function and pass control somewhere else.
/// There are three such instructions:
///
/// * [`Panic`](OpCode::Panic) - invoke runtime error and pass control back to Rust host.
/// * [`Return`](OpCode::Return) - gracefully finish execution and provide return values to caller.
/// * [`Invoke`](OpCode::Invoke) - suspend execution and call another function, passing some arguments to it.
///   Control will return to current function if the callee finishes gracefully.
///
/// Additionally, exhausting all instructions within current function is equivalent
/// to clearing the stack and gracefully returning.
///
/// ## Jumps
///
/// Jumping instruction allow direct manipulation of instruction pointer.
/// There are three such instructions: [`JumpIf`](OpCode::JumpIf), [`Jump`](OpCode::Jump) and [`Loop`](OpCode::Loop).
/// `Loop` decrements the pointer, whereas the first two increment it.
///
/// If the function contains `len` instructions, setting instruction pointer to any value
/// outside `0..=len` range will result in Lua panic.
/// Setting pointer to `len` causes function to terminate.
///
/// During execution instruction pointer always points at the currently executed instruction.
/// Jumping instructions will override the autoincrement,
///  e.g. the instruction pointed at after calculation will be the next one to be executed.
/// For example:
///
/// * `Jump(InstrOffset(1))` will jump to next instruction and is equivalent to noop.
/// * `Jump(InstrOffset(0))` will jump to this same instruction, effectively trapping the execution.
///
/// It is valid to perform 0 offset jumps: there is a miriad ways to trap execution in some infinite loop;
/// there is no point in preventing just one particular case.
///
/// ## Direct stack manipulation
///
/// There is only one instruction that can directly manipulate stack: [`AdjustStack`](OpCode::AdjustStack).
/// The opcode will use its parameter to set stack length.
/// There is no way to observe stack length or iterate through all stack values on Lua side.
///
/// ## Load instructions
///
/// All loading instructions produce values on top of the stack,
/// differing only in source they take value from.
///
/// Most important ones produce a single value, copying it from the source:
///
/// * [`LoadStack`](OpCode::LoadStack)
/// * [`LoadUpvalue`](OpCode::LoadUpvalue)
/// * [`LoadConstant`](OpCode::LoadConstant)
///
/// But there are two instructions with special treatment:
///
/// * [`LoadVariadic`](OpCode::LoadVariadic) - load *all* values from [variadic register](crate::opcode#variadic-register) (which can contain any number of values).
/// * [`MakeClosure`](OpCode::MakeClosure) - opcode doesn't load data directly, but rather constructs new Lua closure using specified recipe.
///
/// ## Store instructions
///
/// All store instructions move value from the top of stack into specified place.
/// There are three such instructions:
///
/// * [`StoreStack`](OpCode::StoreStack)
/// * [`StoreUpvalue`](OpCode::StoreUpvalue)
///
/// ## Binary/unary ops
///
/// Operations consume Lua values producing new value as a result.
/// All ops are grouped under either [`BinOp`](OpCode::BinOp) variant for binary operations
/// or [`UnaOp`](OpCode::UnaOp) variant for unary operations.
///
/// See individual ops for description of their behavior.
///
/// ## Table manipulation
///
/// Table instructions allow to manipulate Lua tables.
/// There are currently three such instructions:
///
/// * [`TabCreate`](OpCode::TabCreate) - create fresh empty table.
/// * [`TabGet`](OpCode::TabGet) - lookup key in table and clone the value onto the stack.
/// * [`TabSet`](OpCode::TabSet) - move value into table under specified key.
#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Panic,
    Invoke(StackSlot),
    Return(StackSlot),
    LoadStack(StackSlot),
    LoadUpvalue(UpvalueSlot),
    LoadConstant(ConstId),
    LoadVariadic,
    MakeClosure(RecipeId),
    StoreStack(StackSlot),
    StoreUpvalue(UpvalueSlot),
    AdjustStack(StackSlot),
    BinOp(BinOp),
    UnaOp(UnaOp),
    Jump { offset: InstrOffset },
    JumpIf { cond: bool, offset: InstrOffset },
    Loop { offset: InstrOffset },
    TabCreate,
    TabGet,
    TabSet,
}

impl OpCode {
    pub fn name(self) -> &'static str {
        use OpCode::*;

        match self {
            Panic => "Panic",
            Invoke(_) => "Invoke",
            Return(_) => "Return",
            LoadStack(_) => "LoadStack",
            LoadUpvalue(_) => "LoadUpvalue",
            LoadConstant(_) => "LoadConst",
            LoadVariadic => "LoadVariadic",
            StoreStack(_) => "StoreStack",
            StoreUpvalue(_) => "StoreUpvalue",
            MakeClosure(_) => "MakeClosure",
            AdjustStack(_) => "AdjustStack",
            BinOp(_) => "BinaryOp",
            UnaOp(_) => "UnaryOp",
            Jump { .. } => "Jump",
            JumpIf { .. } => "JumpIf",
            Loop { .. } => "Loop",
            TabCreate => "TabCreate",
            TabSet => "TabSet",
            TabGet => "TabGet",
        }
    }
}

impl From<BinOp> for OpCode {
    fn from(value: BinOp) -> Self {
        OpCode::BinOp(value)
    }
}

impl From<AriBinOp> for OpCode {
    fn from(value: AriBinOp) -> Self {
        OpCode::BinOp(value.into())
    }
}

impl From<BitBinOp> for OpCode {
    fn from(value: BitBinOp) -> Self {
        OpCode::BinOp(value.into())
    }
}

impl From<EqBinOp> for OpCode {
    fn from(value: EqBinOp) -> Self {
        OpCode::BinOp(value.into())
    }
}

impl From<RelBinOp> for OpCode {
    fn from(value: RelBinOp) -> Self {
        OpCode::BinOp(value.into())
    }
}

impl From<StrBinOp> for OpCode {
    fn from(value: StrBinOp) -> Self {
        OpCode::BinOp(value.into())
    }
}

impl From<UnaOp> for OpCode {
    fn from(value: UnaOp) -> Self {
        OpCode::UnaOp(value)
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpCode::*;

        let name = self.name();

        let s = match *self {
            Panic => format!("{name:<11}"),
            Invoke(StackSlot(index)) => format!("{name:<11} [{index:>3}]"),
            Return(StackSlot(index)) => format!("{name:<11} [{index:>3}]"),
            LoadVariadic => format!("{name:<11}"),
            LoadConstant(ConstId(index)) => format!("{name:<11} [{index:>3}]"),
            LoadStack(StackSlot(index)) => format!("{name:<11} [{index:>3}]"),
            StoreStack(StackSlot(index)) => format!("{name:<11} [{index:>3}]"),
            AdjustStack(StackSlot(height)) => format!("{name:<11} [{height:>3}]"),
            MakeClosure(RecipeId(id)) => format!("{name:<11} [{id:>3}]"),
            LoadUpvalue(UpvalueSlot(index)) => format!("{name:<11} [{index:>3}]"),
            StoreUpvalue(UpvalueSlot(index)) => format!("{name:<11} [{index:>3}]"),
            BinOp(op) => format!("{name:<11} [{op}]"),
            UnaOp(op) => format!("{name:<11} [{op}]"),
            Jump { offset } => format!("{name:<11} [{:>3}]", offset.0),
            JumpIf { cond, offset } => format!("{name:<11} [{cond:>5}] [{:>3}]", offset.0),
            Loop { offset } => format!("{name:<11} [{:>3}]", offset.0),
            TabCreate => format!("{name:<11}"),
            TabSet => format!("{name:<11}"),
            TabGet => format!("{name:<11}"),
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
    Pow,
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
            Pow => "^",
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
pub enum EqBinOp {
    Eq,
    Neq,
}

impl Display for EqBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match *self {
            EqBinOp::Eq => "==",
            EqBinOp::Neq => "~=",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Copy, Clone)]
pub enum RelBinOp {
    LtEq,
    Lt,
    GtEq,
    Gt,
}

impl Display for RelBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match *self {
            RelBinOp::LtEq => "<=",
            RelBinOp::Lt => "<",
            RelBinOp::GtEq => ">=",
            RelBinOp::Gt => ">",
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
    Eq(EqBinOp),
    Rel(RelBinOp),
    Str(StrBinOp),
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Ari(t) => write!(f, "{}", t),
            BinOp::Bit(t) => write!(f, "{}", t),
            BinOp::Eq(t) => write!(f, "{}", t),
            BinOp::Rel(t) => write!(f, "{}", t),
            BinOp::Str(t) => write!(f, "{}", t),
        }
    }
}

impl From<AriBinOp> for BinOp {
    fn from(value: AriBinOp) -> Self {
        BinOp::Ari(value)
    }
}

impl From<BitBinOp> for BinOp {
    fn from(value: BitBinOp) -> Self {
        BinOp::Bit(value)
    }
}

impl From<EqBinOp> for BinOp {
    fn from(value: EqBinOp) -> Self {
        BinOp::Eq(value)
    }
}

impl From<RelBinOp> for BinOp {
    fn from(value: RelBinOp) -> Self {
        BinOp::Rel(value)
    }
}

impl From<StrBinOp> for BinOp {
    fn from(value: StrBinOp) -> Self {
        BinOp::Str(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaOp {
    AriNeg,
    BitNot,
    StrLen,
    LogNot,
}

impl Display for UnaOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaOp::AriNeg => "-",
            UnaOp::BitNot => "~",
            UnaOp::StrLen => "#",
            UnaOp::LogNot => "not",
        };

        write!(f, "{}", s)
    }
}
