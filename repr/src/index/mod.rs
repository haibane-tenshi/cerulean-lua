mod const_id;
mod function_id;
mod instr_id;
mod stack_slot;

pub use const_id::{ConstCapacityError, ConstId};
pub use function_id::{FunctionCapacityError, FunctionId};
pub use instr_id::{InstrCountError, InstrId, InstrOffset};
pub use stack_slot::{StackOffset, StackSlot};
