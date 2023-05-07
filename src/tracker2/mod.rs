pub(crate) mod stack;

// use std::collections::HashMap;
// use crate::opcode::{OpCode, InstrId, StackSlot};
// use crate::index_vec::IndexVec;
//
// use stack::Stack;
//
// #[derive(Debug)]
// pub enum Backpatch {
//
// }
//
// #[derive(Debug)]
// pub enum Instr {
//     OpCode(OpCode),
//     Unfinished(Backpatch),
// }
//
// #[derive(Debug, Default)]
// pub struct Fragment {
//     opcodes: IndexVec<InstrId, Instr>,
// }
//
// #[derive(Debug, Default)]
// pub struct FragmentBuilder<'s> {
//     opcodes: IndexVec<InstrId, Instr>,
//     stack: Stack<'s>,
// }
