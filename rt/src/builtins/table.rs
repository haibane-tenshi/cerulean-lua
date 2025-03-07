//! Raw operations related to table and metatable access.
//!
//! This module contains functions emulating raw indexing operations.
//!
//! # Provided high-level APIs
//!
//! * [`find_metavalue`] - resolve metavalue on a sequence of values
//! * [`get_index`] - retrieve value under specific key from target
//! * [`set_index`] - assign value under specific key on target
//! * [`GetIndexCache`] - caches *lookup chain* for faster multiple indexing retrievals
//! * [`SetIndexCache`] - caches *lookup chain* for faster multiple indexing assignments
//!
//! # Provided low-level APIs
//!
//! * [`get_index_chain`] - iterate over *lookup chain* for indexing retrievals
//! * [`set_index_chain`] - iterate over *lookup chain* for indexing assignments
//!
//! In general you should have no reason to reach for those, high-level APIs should be able to cover almost all needs.
use std::fmt::{Debug, Display};
use std::ops::ControlFlow;

use gc::{GcCell, RootCell};

use crate::error::{AlreadyDroppedError, AlreadyDroppedOr};
use crate::gc::Heap;
use crate::runtime::MetatableRegistry;
use crate::value::{Key, StrongCallable, Types, WeakKey, WeakValue};

/// Resolve metavalue out of a list of values.
///
/// This method will seek metavalue with specified key inside metatables for provided values.
/// Values will be checked in order, concluding with the first non-`nil` metavalue produced.
/// `nil` will be returned if no suitable metavalue is resolved.
///
/// This method will error-out if any value or metatable is already garbage-collected.
pub fn find_metavalue<Ty>(
    values: impl IntoIterator<Item = WeakValue<Ty>>,
    key: WeakKey<Ty>,
    heap: &Heap<Ty>,
    registry: &MetatableRegistry<Ty::Table>,
) -> Result<WeakValue<Ty>, AlreadyDroppedError>
where
    Ty: Types,
{
    values
        .into_iter()
        .find_map(|value| {
            use crate::value::{TableIndex, Value};

            let metatable = match value.metatable(heap, registry) {
                Err(err) => return Some(Err(err)),
                Ok(None) => return None,
                Ok(Some(mt)) => mt,
            };
            let metatable = heap.get(metatable)?;

            match metatable.get(&key) {
                Value::Nil => None,
                value => Some(Ok(value)),
            }
        })
        .transpose()
        .map(Option::unwrap_or_default)
}

fn lookup_chain<'h, 'r, Ty>(
    target: WeakValue<Ty>,
    heap: &'h Heap<Ty>,
    registry: &'r MetatableRegistry<Ty::Table>,
    mut key: Option<WeakKey<Ty>>,
) -> impl Iterator<Item = Result<LookupEntry<Ty>, AlreadyDroppedOr<LookupError<Ty>>>> + use<'h, 'r, Ty>
where
    Ty: Types,
{
    use crate::gc::{LuaPtr, Upgrade};
    use crate::value::Value;

    let first = match target {
        Value::Table(LuaPtr(ptr)) => Some(Ok(LookupEntry::Table(ptr))),
        _ => None,
    };

    let initial = target;
    let mut current = target;
    let rest = std::iter::from_fn(move || {
        let true_key = key?;

        loop {
            let prev = current;
            current = match find_metavalue([current], true_key, heap, registry) {
                Ok(t) => t,
                Err(err) => return Some(Err(err.into())),
            };

            match current {
                Value::Table(LuaPtr(ptr)) => break Some(Ok(LookupEntry::Table(ptr))),
                Value::Function(callable) => {
                    // This stops any further lookup.
                    key = None;

                    let func = match callable.try_upgrade(heap) {
                        Ok(t) => t,
                        Err(err) => return Some(Err(err.into())),
                    };

                    let r = LookupEntry::Call(CallRequired { func, target: prev });
                    break Some(Ok(r));
                }
                Value::Nil => {
                    // This stops any further lookup.
                    key = None;

                    // Convert terminating value into error when necessary.
                    let r = match prev {
                        Value::Table(_) => None,
                        // We need to be careful when initial value is function.
                        // Any *metavalue* functions should be included in chain,
                        // however the initial value (even if it is function) should not.
                        // Check converts this case into an error.
                        Value::Function(_) if prev != initial => None,
                        value => Some(Err(AlreadyDroppedOr::Other(LookupError(value)))),
                    };

                    break r;
                }
                _ => (),
            };
        }
    });

    first.into_iter().chain(rest)
}

/// Entries in *lookup chain*.
pub enum LookupEntry<Ty>
where
    Ty: Types,
{
    /// Table entry.
    Table(GcCell<Ty::Table>),

    /// Lookup chain termitaed with call to function.
    Call(CallRequired<Ty>),
}

/// Call to the function is required to evaluate op.
///
/// It is expected that function will receive `target` as its first argument.
pub struct CallRequired<Ty>
where
    Ty: Types,
{
    pub func: StrongCallable<Ty>,
    pub target: WeakValue<Ty>,
}

impl<Ty> Debug for CallRequired<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallRequired")
            .field("func", &self.func)
            .field("target", &self.target)
            .finish()
    }
}

impl<Ty> Clone for CallRequired<Ty>
where
    Ty: Types,
{
    fn clone(&self) -> Self {
        let Self { func, target } = self;

        Self {
            func: func.clone(),
            target: *target,
        }
    }
}

/// Lookup chain ended in a value which cannot be used for indexing.
pub struct LookupError<Ty>(pub WeakValue<Ty>)
where
    Ty: Types;

impl<Ty> Clone for LookupError<Ty>
where
    Ty: Types,
{
    fn clone(&self) -> Self {
        *self
    }
}

impl<Ty> Copy for LookupError<Ty> where Ty: Types {}

impl<Ty> Debug for LookupError<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("LookupError").field(&self.0).finish()
    }
}

impl<Ty> Display for LookupError<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "cannot index into value {}", self.0.fmt_stringless())
    }
}

impl<Ty> std::error::Error for LookupError<Ty> where Ty: Types {}

/// Produce iterator over *lookup chain* for the purpose of indexing retrieval.
///
/// This is a low-level API.
/// Refer to other methods inside this module to cover common needs.
///
/// This *lookup chain* can only be used for retrieving value under an index.
/// Refer to [`set_index_chain`] instead in order to construct *lookup chain* for indexing assignments.
///
/// # Emulated Lua behavior
///
/// Purpose of this function is to construct an iterator over Lua values (more specifically tables),
/// that will be interacted with when Lua attempts to perform indexing operation.
/// [According to spec][lua#2.4], when attempting to index a value the following happens:
///
/// * if the value is a table and index is part of table (e.g. its associated value is not `nil`) then this is the result,
/// * otherwise indexing is performed on `__index` field of the metatable.
///
/// The last point is recursive, that is it may trigger looking into `__index` metavalue on this new value and so on.
/// Recursion breaks if any of the following is fulfilled:
///
/// * Metavalue is a table and contains non-`nil` value under the index
/// * Metavalue is a function (value of type `function`, not just arbitrary callable; this ignores values with `__call` metamethod).
///
///    Lua expects the function to accept exactly 2 arguments: target (table or other value) and index.
///    Returns should be adjusted by the caller to single value which is the result of the indexing process.
///
/// * Metavalue is a `nil`.
///
///    `nil` indicates absence of value, so any `nil` metavalue will terminate the process even if there is a metatable associated with it.
///    Note that you can still index into `nil` itself and it works exactly the way you would expect.
///
///    In this case raw indexing into previous value becomes result of indexing process.
///    It is going to be `nil` if the previous value is a table and invokes error otherwise.
///
/// If you put it all together, then what Lua constructs here is a *lookup chain*:
/// a sequence of tables which is traversed until individual lookup succeeds, sequence is exhausted or a function is found.
///
/// # Notes
///
/// This iterator **may not be finite**.
/// There is nothing that prevents creating cycles between value and its `__index` metavalue.
///
/// The only Lua type permitting raw indexing is tables.
/// For this reason iterator will only produce references to tables with possible exception of the last item which may be a function.
///
/// Iterator will end with [`LookupError`] if the last value in chain (the one preceding `nil`) cannot be used for indexing.
/// This includes case when `target` is not a table and don't have properly configured metatable.
/// In particular this implies that the iterator will always produce at least one item (even if it is just `LookupError`).
///
/// No specific guarantees are given about output following after an error.
///
/// [lua#2.4]: https://www.lua.org/manual/5.4/manual.html#2.4
pub fn get_index_chain<'h, 'r, Ty>(
    target: WeakValue<Ty>,
    heap: &'h Heap<Ty>,
    registry: &'r MetatableRegistry<Ty::Table>,
) -> impl Iterator<Item = Result<LookupEntry<Ty>, AlreadyDroppedOr<LookupError<Ty>>>> + use<'h, 'r, Ty>
where
    Ty: Types,
{
    use crate::gc::LuaPtr;
    use crate::runtime::thread::frame::BuiltinMetamethod;

    let key = heap
        .find_interned(&Ty::String::from(BuiltinMetamethod::Index.to_str()))
        .map(|name| Key::String(LuaPtr(name.downgrade())));

    lookup_chain(target, heap, registry, key)
}

/// Produce iterator over *lookup chain* for the purpose of indexing assignment.
///
/// This is a low-level API.
/// Refer to other methods inside this module to cover common needs.
///
/// This *lookup chain* can only be used for assigning value under an index.
/// Refer to [`get_index_chain`] instead in order to construct *lookup chain* for indexing retrieval.
///
/// # Emulated Lua behavior
///
/// Purpose of this function is to construct an iterator over Lua values (more specifically tables),
/// that will be interacted with when Lua attempts to perform indexing operation.
/// [According to spec][lua#2.4], when attempting to index a value the following happens:
///
/// * if the value is a table and index is part of table (e.g. its associated value is not `nil`) then it is overwritten,
/// * otherwise indexing is performed on `__newindex` field of the metatable.
///
/// The last point is recursive, that is it may trigger looking into `__newindex` metavalue on this new value and so on.
/// Recursion breaks if any of the following is fulfilled:
///
/// * Metavalue is a table and contains non-`nil` value under the index
/// * Metavalue is a function (value of type `function`, not just arbitrary callable; this ignores values with `__call` metamethod).
///
///    Lua expects the function to accept exactly 3 arguments: target (table or other value), index and value.
///    Returns should be discarded by the caller.
///
/// * Metavalue is a `nil`.
///
///    `nil` indicates absence of value, so any `nil` metavalue will terminate the process even if there is a metatable associated with it.
///    Note that you can still index into `nil` itself and it works exactly the way you would expect.
///
///    In this case raw indexing assignment into previous value is performed.
///    If the previous value is not a table operation result in error.
///
/// If you put it all together, then what Lua constructs here is a *lookup chain*:
/// a sequence of tables which is traversed until individual lookup succeeds, sequence is exhausted or a function is found.
///
/// # Notes
///
/// This iterator **may not be finite**.
/// There is nothing that prevents creating cycles between value and its `__index` metavalue.
///
/// The only Lua type permitting raw indexing is tables.
/// For this reason iterator will only produce references to tables with possible exception of the last item which may be a function.
///
/// Iterator will end with [`LookupError`] if the last value in chain (the one preceding `nil`) cannot be used for indexing.
/// This includes case when `target` is not a table and don't have properly configured metatable.
/// In particular this implies that the iterator will always produce at least one item (even if it is just `LookupError`).
///
/// No specific guarantees are given about output following after an error.
///
/// [lua#2.4]: https://www.lua.org/manual/5.4/manual.html#2.4
pub fn set_index_chain<'h, 'r, Ty>(
    target: WeakValue<Ty>,
    heap: &'h Heap<Ty>,
    registry: &'r MetatableRegistry<Ty::Table>,
) -> impl Iterator<Item = Result<LookupEntry<Ty>, AlreadyDroppedOr<LookupError<Ty>>>> + use<'h, 'r, Ty>
where
    Ty: Types,
{
    use crate::gc::LuaPtr;
    use crate::runtime::thread::frame::BuiltinMetamethod;

    let key = heap
        .find_interned(&Ty::String::from(BuiltinMetamethod::NewIndex.to_str()))
        .map(|name| Key::String(LuaPtr(name.downgrade())));

    lookup_chain(target, heap, registry, key)
}

/// Perform indexing retrieval on a value.
///
/// This operation is Lua-specific extension to raw table indexing.
/// Unlike other raw ops it requires metatable access and will directly resolve function it is delegated to.
///
/// This function will traverse `target`'s [*lookup chain*](get_index_chain#emulated-lua-behavior), attempting to index all tables in order
/// until either an individual lookup produces non-`nil`, sequence is exhausted or a function is found.
///
/// # Delegation
///
/// [`CallRequired`] will be produced in case a function call is required to finish the operation.
/// Unlike `MetamethodRequired` produced by other raw ops, `CallRequired` already contains resolved function reference.
///
/// Lua expects the function to receive exactly 2 arguments: target and index.
/// Note that you should provide target supplied with `CallRequired`, not original one!
/// Caller should adjust results to 1 value which becomes the result of indexing.
///
/// # Coercion warnings
///
/// As is customary with all raw ops, this function will not perform any coercions.
/// You should perform any desired coercions prior to calling this function.
///
/// However, you should be careful in case indexing results in function call.
/// Lua spec dictates that only tables observe coerced indices!
/// Function call should receive original `key` before any coercions.
#[allow(clippy::type_complexity)]
pub fn get_index<Ty>(
    target: WeakValue<Ty>,
    key: WeakKey<Ty>,
    heap: &Heap<Ty>,
    registry: &MetatableRegistry<Ty::Table>,
) -> Result<ControlFlow<WeakValue<Ty>, CallRequired<Ty>>, AlreadyDroppedOr<LookupError<Ty>>>
where
    Ty: Types,
{
    use crate::gc::LuaPtr;
    use crate::runtime::thread::frame::BuiltinMetamethod;

    let index_key = heap
        .find_interned(&Ty::String::from(BuiltinMetamethod::Index.to_str()))
        .map(|name| Key::String(LuaPtr(name.downgrade())));

    get_index_inner(target, key, heap, registry, index_key)
}

#[allow(clippy::type_complexity)]
pub(crate) fn get_index_inner<Ty>(
    target: WeakValue<Ty>,
    key: WeakKey<Ty>,
    heap: &Heap<Ty>,
    registry: &MetatableRegistry<Ty::Table>,
    index_key: Option<WeakKey<Ty>>,
) -> Result<ControlFlow<WeakValue<Ty>, CallRequired<Ty>>, AlreadyDroppedOr<LookupError<Ty>>>
where
    Ty: Types,
{
    use crate::gc::TryGet;
    use crate::value::{TableIndex, Value};

    for entry in lookup_chain(target, heap, registry, index_key) {
        match entry? {
            LookupEntry::Table(ptr) => {
                let table = heap.try_get(ptr)?;
                let value = table.get(&key);

                if value != Value::Nil {
                    return Ok(ControlFlow::Break(value));
                }
            }
            LookupEntry::Call(t) => return Ok(ControlFlow::Continue(t)),
        }
    }

    // Reaching here means we produced at least one table as guaranteed by `get_index_chain`.
    Ok(ControlFlow::Break(Value::Nil))
}

/// Perform indexing assignment on a value.
///
/// This operation is Lua-specific extension to raw table indexing.
/// Unlike other raw ops it requires metatable access and will directly resolve function it is delegated to.
///
/// This function will traverse `target`'s [*lookup chain*](set_index_chain#emulated-lua-behavior), attempting to index all tables in order
/// until either an individual lookup produces non-`nil`, sequence is exhausted or a function is found.
///
/// # Delegation
///
/// [`CallRequired`] will be produced in case a function call is required to finish the operation.
/// Unlike `MetamethodRequired` produced by other raw ops, `CallRequired` already contains resolved function reference.
///
/// Lua expects the function to receive exactly 3 arguments: target, index and value.
/// Note that you should provide target supplied with `CallRequired`, not original one!
/// Caller should discard any results produced.
///
/// # Coercion warnings
///
/// As is customary with all raw ops, this function will not perform any coercions.
/// You should perform any desired coercions prior to calling this function.
///
/// However, you should be careful in case indexing results in function call.
/// Lua spec dictates that only tables observe coerced indices!
/// Function call should receive original `key` before any coercions.
pub fn set_index<Ty>(
    target: WeakValue<Ty>,
    key: WeakKey<Ty>,
    value: WeakValue<Ty>,
    heap: &mut Heap<Ty>,
    registry: &MetatableRegistry<Ty::Table>,
) -> Result<ControlFlow<(), CallRequired<Ty>>, AlreadyDroppedOr<LookupError<Ty>>>
where
    Ty: Types,
{
    use crate::gc::LuaPtr;
    use crate::runtime::thread::frame::BuiltinMetamethod;

    let newindex_key = heap
        .find_interned(&Ty::String::from(BuiltinMetamethod::NewIndex.to_str()))
        .map(|name| Key::String(LuaPtr(name.downgrade())));

    set_index_inner(target, key, value, heap, registry, newindex_key)
}

pub(crate) fn set_index_inner<Ty>(
    target: WeakValue<Ty>,
    key: WeakKey<Ty>,
    value: WeakValue<Ty>,
    heap: &mut Heap<Ty>,
    registry: &MetatableRegistry<Ty::Table>,
    newindex_key: Option<WeakKey<Ty>>,
) -> Result<ControlFlow<(), CallRequired<Ty>>, AlreadyDroppedOr<LookupError<Ty>>>
where
    Ty: Types,
{
    use crate::gc::TryGet;
    use crate::value::{TableIndex, Value};

    let mut found = None;
    for entry in lookup_chain(target, heap, registry, newindex_key) {
        match entry? {
            LookupEntry::Table(ptr) => {
                found = Some(ptr);
                let table = heap.try_get(ptr)?;
                let value = table.get(&key);

                if value != Value::Nil {
                    break;
                }
            }
            LookupEntry::Call(t) => return Ok(ControlFlow::Continue(t)),
        }
    }

    let ptr = found.expect("`set_index_chain` should produce at least one value");
    let table = heap.try_get_mut(ptr)?;
    table.set(key, value);

    Ok(ControlFlow::Break(()))
}

/// Cache *lookup chain* for multiple indexing retrievals.
///
/// This type is useful in case you intend to perform multiple indexing retrievals from the same value.
/// By caching lookup chain it avoids recalculating it every time it is required.
///
/// Note that cache does not borrow from heap, so entities inside the heap can still be modified while this cache exists.
/// However, it is your responsibility to ensure that cache is up-to-date.
/// If any intermediate values are modified in such way that changes lookup chain,
/// indexing performed through this type become semantically incorrect.
pub struct GetIndexCache<Ty>
where
    Ty: Types,
{
    tables: Vec<RootCell<Ty::Table>>,
    call: Option<CallRequired<Ty>>,
    err: Option<LookupError<Ty>>,
}

impl<Ty> GetIndexCache<Ty>
where
    Ty: Types,
{
    /// Construct and cache *lookup chain* for the `target`.
    pub fn new(
        target: WeakValue<Ty>,
        heap: &Heap<Ty>,
        registry: &MetatableRegistry<Ty::Table>,
    ) -> Result<Self, AlreadyDroppedError> {
        use crate::gc::TryGet;

        let mut call = None;
        let mut err = None;
        let mut tables = Vec::new();
        for entry in get_index_chain(target, heap, registry) {
            match entry {
                Ok(LookupEntry::Table(ptr)) => {
                    let ptr = heap.try_upgrade(ptr)?;
                    tables.push(ptr)
                }
                Ok(LookupEntry::Call(func)) => call = Some(func),
                Err(AlreadyDroppedOr::Other(e)) => err = Some(e),
                Err(AlreadyDroppedOr::Dropped(err)) => return Err(err),
            }
        }

        debug_assert!(!tables.is_empty() || call.is_some() || err.is_some());

        let r = GetIndexCache { tables, call, err };

        Ok(r)
    }

    /// Retrieve value under the `key`.
    pub fn get(
        &self,
        key: WeakKey<Ty>,
        heap: &Heap<Ty>,
    ) -> Result<ControlFlow<WeakValue<Ty>, CallRequired<Ty>>, LookupError<Ty>> {
        use crate::value::{TableIndex, Value};

        for table in &self.tables {
            let table = heap.get_root(table);
            let value = table.get(&key);

            if value != Value::Nil {
                return Ok(ControlFlow::Break(value));
            }
        }

        if let Some(call) = &self.call {
            return Ok(ControlFlow::Continue(call.clone()));
        }

        if let Some(err) = &self.err {
            return Err(*err);
        }

        Ok(ControlFlow::Break(Value::Nil))
    }

    /// Iterate over *lookup chain*.
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = Result<LookupEntry<Ty>, LookupError<Ty>>> + use<'_, Ty> {
        self.tables
            .iter()
            .map(|table| LookupEntry::Table(table.downgrade()))
            .chain(self.call.clone().map(LookupEntry::Call))
            .map(Ok)
            .chain(self.err.map(Err))
    }
}

impl<Ty> Debug for GetIndexCache<Ty>
where
    Ty: Types,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GetIndexCache")
            .field("tables", &self.tables)
            .field("call", &self.call)
            .field("err", &self.err)
            .finish()
    }
}

/// Cache *lookup chain* for multiple indexing assignments.
///
/// This type is useful in case you intend to perform multiple indexing assignments on the same value.
/// By caching lookup chain it avoids recalculating it every time it is required.
///
/// Note that cache does not borrow from heap, so entities inside the heap can still be modified while this cache exists.
/// However, it is your responsibility to ensure that cache is up-to-date.
/// If any intermediate values are modified in such way that changes lookup chain,
/// indexing performed through this type become semantically incorrect.
pub struct SetIndexCache<Ty>
where
    Ty: Types,
{
    tables: Vec<RootCell<Ty::Table>>,
    call: Option<CallRequired<Ty>>,
    err: Option<LookupError<Ty>>,
}

impl<Ty> SetIndexCache<Ty>
where
    Ty: Types,
{
    /// Construct and cache *lookup chain* for the `target`.
    pub fn new(
        target: WeakValue<Ty>,
        heap: &Heap<Ty>,
        registry: &MetatableRegistry<Ty::Table>,
    ) -> Result<Self, AlreadyDroppedError> {
        use crate::gc::TryGet;

        let mut call = None;
        let mut err = None;
        let mut tables = Vec::new();
        for entry in set_index_chain(target, heap, registry) {
            match entry {
                Ok(LookupEntry::Table(ptr)) => {
                    let ptr = heap.try_upgrade(ptr)?;
                    tables.push(ptr)
                }
                Ok(LookupEntry::Call(func)) => call = Some(func),
                Err(AlreadyDroppedOr::Other(e)) => err = Some(e),
                Err(AlreadyDroppedOr::Dropped(err)) => return Err(err),
            }
        }

        debug_assert!(!tables.is_empty() || call.is_some() || err.is_some());

        let r = SetIndexCache { tables, call, err };

        Ok(r)
    }

    /// Assign `value` under the `key`.
    pub fn set(
        &self,
        key: WeakKey<Ty>,
        value: WeakValue<Ty>,
        heap: &mut Heap<Ty>,
    ) -> Result<ControlFlow<(), CallRequired<Ty>>, LookupError<Ty>> {
        use crate::value::{TableIndex, Value};

        for table in &self.tables {
            let table = heap.get_root_mut(table);
            let contained = table.get(&key);

            if contained != Value::Nil {
                table.set(key, value);
                return Ok(ControlFlow::Break(()));
            }
        }

        if let Some(call) = &self.call {
            return Ok(ControlFlow::Continue(call.clone()));
        }

        if let Some(err) = &self.err {
            return Err(*err);
        }

        // Reaching here means there is at least one table in the chain.
        let table = self.tables.last().unwrap();
        let table = heap.get_root_mut(table);
        table.set(key, value);

        Ok(ControlFlow::Break(()))
    }

    /// Iterate over *lookup chain*.
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = Result<LookupEntry<Ty>, LookupError<Ty>>> + use<'_, Ty> {
        self.tables
            .iter()
            .map(|table| LookupEntry::Table(table.downgrade()))
            .chain(self.call.clone().map(LookupEntry::Call))
            .map(Ok)
            .chain(self.err.map(Err))
    }
}
