//! Builtin operations on Lua values.
//!
//! This module contains implementation of native ops on [`Value`s](crate::value::Value).
//! These ops are not implemented on `Value` type directly
//! since unlike for strong types from [`value`](crate::value) module,
//! there is only a small number of type combinations that returns meaningful results.
//! If you already have strongly typed Lua values (like [`Int`](crate::value::Int) or [`Float`](crate::value::Float))
//! it is best to use ops and functions provided directly on those types.
//!
//! Lua behavior when evaluating basic ops can be separated into three steps:
//!
//! 1. coercion
//! 2. raw builtins
//! 3. metamethod call
//!
//! Methods inside this module cover the first two steps:
//! * [`raw`](self::raw) module provides raw operations disregarding possible coercions.
//! * [`coerce`](self::coerce) module provides methods for controlling and performing type coercions in various situations.
//!
//! The last step requires access to runtime (for obvious reasons), but there are a few helper methods to assist you in the task.
//!
//! # Coercion warnings
//!
//! You should be aware that not all coercions defined by Lua is part of builtin behavior.
//! Some are instead performed as part of metamethod calls.
//! See [`coerce`] module-level docs for more information.
//!
//! Additionally, raw bultins **do not perform operations between floats and integers**.
//! This happens because inside the runtime we keep distinction between two numeric types.
//! In particular, it implies that you need to perform numeric coercion if you want to get native Lua behavior.

pub mod coerce;
pub mod full;
pub mod raw;

// use crate::runtime::TransientStackFrame;
// use crate::gc::Heap;
// use crate::value::{Value, Callable, Weak, Types};

// pub struct NotCallableError;

// pub fn prepare_invoke<Ty>(mut callable: Value<Weak, Ty>, mut stack: TransientStackFrame<'_, Ty>, heap: &Heap<Ty>) -> Result<Callable<Weak, Ty>, NotCallableError>
// where
//     Ty: Types
// {
//     use repr::index::StackSlot;

//     let starting_len = stack.len();

//     let err = loop {
//         callable = match callable {
//             Value::Function(callable) => return Ok(callable),
//             t => t,
//         };

//         let new_callable = self
//             .core
//             .metatable_of(&callable)?
//             .map(|mt| {
//                 let key = self.core.lookup_event(Event::Call);
//                 self.core
//                     .gc
//                     .get(mt)
//                     .ok_or(AlreadyDroppedError)
//                     .map(|table| table.get(&key))
//             })
//             .transpose()?
//             .unwrap_or_default();

//         // Keys associated with nil are not considered part of the table.
//         if new_callable == Value::Nil {
//             break NotCallableError;
//         }

//         stack.insert(StackSlot(0), callable);
//         callable = new_callable;
//     };

//     // We failed to find callable, remove everything that was put on the stack.
//     let end = StackSlot(stack.len() - starting_len);
//     stack.drain(..end);

//     Err(err)
// }
