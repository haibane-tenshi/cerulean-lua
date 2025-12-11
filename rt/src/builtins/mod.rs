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
//! Coercion step always succeeds.
//! Metamethod is attempted when raw builtin fails.
//! When metamethod fails Lua raises an error.
//!
//! Methods inside this module cover the first two steps:
//! * [`raw`] module provides raw operations disregarding possible coercions.
//! * [`coerce`] module provides methods for controlling and performing type coercions in various situations.
//!
//! The last step requires access to runtime (for obvious reasons), but there are a few helper methods to assist you in the task.
//!
//! ## Exceptions
//!
//! ### Length (`#`) operator
//!
//! String length operator `#` when applied to tables have different behavior.
//! It performs metamethod call *before* raw builtin, so the actual order is
//!
//! 1. coercion
//! 2. metamethod call
//! 3. length raw builtin
//!
//! In this case when metamethod is not found raw builtin is executed which always succeeds.
//!
//! Note that length operator behaves normally when applied to arguments of other types (including strings).
//!
//! ### Equality (`==`) and inequality (`~=`) operators
//!
//! Equality operator have a fallback stage in case metamethod lookup fails:
//!
//! 1. coercion
//! 2. eq raw builtin
//! 3. metamethod call
//! 4. raw identity comparison
//!
//! The last step always succeeds (it carries result from 2nd step so it always returns `false` for equality op).
//!
//! Inequality is defined exactly as negation of equality, so its behavior is identical.
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
pub mod raw_ops;
pub mod table;

use crate::error::{AlreadyDroppedOr, NotCallableError};
use crate::gc::Heap;
use crate::runtime::thread::TransientStackGuard;
use crate::runtime::MetatableRegistry;
use crate::value::{Key, StrongCallable, Types, Value, WeakKey, WeakValue};

// Temporary reexport.
pub use table::find_metavalue;

pub fn prepare_invoke<Ty>(
    callable: WeakValue<Ty>,
    stack: TransientStackGuard<'_, Ty>,
    heap: &Heap<Ty>,
    registry: &MetatableRegistry<Ty::Table>,
) -> Result<StrongCallable<Ty>, AlreadyDroppedOr<NotCallableError<Ty>>>
where
    Ty: Types,
{
    use crate::gc::{LuaPtr, Upgrade};
    use crate::runtime::thread::frame::BuiltinMetamethod;

    let key = heap
        .find_interned(&BuiltinMetamethod::Call.to_str().into())
        .map(|s| Key::String(LuaPtr(s.downgrade())));

    inner_prepare_invoke(callable, stack, heap, registry, key).map_err(|err| match err {
        AlreadyDroppedOr::Dropped(err) => AlreadyDroppedOr::Dropped(err),
        AlreadyDroppedOr::Other(_) => match callable.try_upgrade(heap) {
            Ok(value) => AlreadyDroppedOr::Other(NotCallableError(value)),
            Err(err) => AlreadyDroppedOr::Dropped(err),
        },
    })
}

pub(crate) fn inner_prepare_invoke<Ty>(
    mut callable: WeakValue<Ty>,
    mut stack: TransientStackGuard<'_, Ty>,
    heap: &Heap<Ty>,
    registry: &MetatableRegistry<Ty::Table>,
    key: Option<WeakKey<Ty>>,
) -> Result<StrongCallable<Ty>, AlreadyDroppedOr<InnerNotCallableError>>
where
    Ty: Types,
{
    use crate::gc::{TryGet, Upgrade};
    use crate::value::TableIndex;
    use repr::index::StackSlot;

    let starting_len = stack.len();

    let mut inner = || {
        if let Value::Function(callable) = callable {
            return Ok(callable.try_upgrade(heap)?);
        }

        let not_callable = AlreadyDroppedOr::Other(InnerNotCallableError);

        let Some(key) = key else {
            return Err(not_callable);
        };

        loop {
            callable = match callable {
                Value::Function(callable) => break Ok(callable.try_upgrade(heap)?),
                t => t,
            };

            let metatable = callable.metatable(heap, registry)?.ok_or(not_callable)?;
            let metavalue = heap.try_get(metatable)?.get(&key);

            // Keys associated with nil are not considered part of the table.
            if metavalue == Value::Nil {
                break Err(not_callable);
            }

            stack.insert(StackSlot(0), callable);
            callable = metavalue;
        }
    };

    let r = inner();

    if r.is_err() {
        // We failed to find callable, remove everything that was put on the stack.
        let end = StackSlot(stack.len() - starting_len);
        stack.drain(..end);
    }

    r
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct InnerNotCallableError;
