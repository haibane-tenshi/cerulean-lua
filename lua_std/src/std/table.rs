use rt::ffi::{boxed, DLuaFfi};
use rt::gc::LuaPtr;
use rt::runtime::Core;
use rt::value::{Callable, KeyValue, TableIndex, Types, Value};

use crate::traits::{RootTable, TableEntry};

/// Stringify and concatenate table elements.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(list: table, [sep: string, [i: int, [j: int]]]) -> string`
///
/// Given a list where all elements are strings or numbers, returns the string `list[i]..sep..list[i+1] ··· sep..list[j]`.
/// The default value for `sep` is the empty string, the default for `i` is 1, and the default for `j` is `#list`.
/// If `i` is greater than `j`, returns the empty string.
///
/// Remember that, whenever an operation needs the length of a table, all caveats about the length operator apply (see §3.4.7).
/// All functions ignore non-numeric keys in the tables given as arguments.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes getting length and values out of the table.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  Numeric entries will always get coerced to strings, irrespective of runtime configuration.
///    This is due to that fact that method documentation explicitly states that it can process integer and float elements.
///
///    Usual caveats about int/float-to-string conversion apply:
///    exact representation details are implementation-specific and are subject to change, however result is promised to be human-readable.
///    In particular it is not guaranteed that numbers can be round-tripped.
///    Render numbers manually to have better control over output.
///
///    Note that raw concatenation between strings always takes priority over metamethod even if one is configured.
#[expect(non_camel_case_types)]
pub struct concat;

impl<Ty> TableEntry<Ty> for concat
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
    Ty::String: Unpin,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::table::concat();
        let key = core.gc.intern("concat".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Insert value into table under integer key.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(list: table, [pos: int,] value: any) -> ()`
///
/// Inserts element `value` at position `pos` in list, shifting up the elements `list[pos]`, `list[pos+1]`, ···, `list[#list]`.
/// The default value for `pos` is `#list+1`, so that a call `table.insert(t,x)` inserts `x` at the end of the list `t`.
///
/// Remember that, whenever an operation needs the length of a table, all caveats about the length operator apply (see §3.4.7).
/// All functions ignore non-numeric keys in the tables given as arguments.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes calculating length as well as getting and setting values on the table.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  In essence this function will perform assignments of the form `list[n+1] = list[n]` in order to shift values.
///
///    Order of operations is implementation-specific (including between getters and setters).
#[expect(non_camel_case_types)]
pub struct insert;

impl<Ty> TableEntry<Ty> for insert
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::table::insert();
        let key = core.gc.intern("insert".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Copy range of values from one table into another.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(a1: table, f: int, e: int, t: int, [a2: table]) -> table`
///
/// Moves elements from the table `a1` to the table `a2`, performing the equivalent to the following multiple assignment: `a2[t],··· = a1[f],···,a1[e]`.
/// The default for `a2` is `a1`.
/// The destination range can overlap with the source range.
/// The number of elements to be moved must fit in a Lua integer.
///
/// Returns the destination table `a2`.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes both getting and setting values on tables.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  Order of operations is undefined.
#[expect(non_camel_case_types)]
pub struct move_;

impl<Ty> TableEntry<Ty> for move_
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::table::move_();
        let key = core.gc.intern("move".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Pack all arguments into a new table.
///
/// # From Lua documentation
///
/// Returns a new table with all arguments stored into keys 1, 2, etc. and with a field **"n"** with the total number of arguments.
/// Note that the resulting table may not be a sequence, if some arguments are `nil`.
#[expect(non_camel_case_types)]
pub struct pack;

impl<Ty> TableEntry<Ty> for pack
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::table::pack();
        let key = core.gc.intern("pack".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Remove element from a table.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(list: table, [pos: int]) -> any`
///
/// Removes from list the element at position `pos`, returning the value of the removed element.
/// When `pos` is an integer between 1 and `#list`,
/// it shifts down the elements `list[pos+1]`, `list[pos+2]`, ···, `list[#list]` and erases element `list[#list]`;
/// The index `pos` can also be 0 when `#list` is 0, or `#list + 1`.
///
/// The default value for `pos` is `#list`, so that a call `table.remove(l)` removes the last element of the list `l`.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes both getting and setting values on the table.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  Order of operations is undefined.
#[expect(non_camel_case_types)]
pub struct remove;

impl<Ty> TableEntry<Ty> for remove
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::table::remove();
        let key = core.gc.intern("remove".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}

/// Place table elements on the stack.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `(list: table, [i: int, [j: int]]) -> (...: any)`
///
/// Returns the elements from the given list.
/// This function is equivalent to
///
/// ```lua
/// return list[i], list[i+1], ···, list[j]
/// ```
///
/// By default, `i` is 1 and `j` is `#list`.
///
/// # Implementation-specific behavior
///
/// *  Operations performed by this function are *regular* that is it may invoke metamethods.
///    This includes acquiring length and getting values out of the table.
///    
///    This replicates behavior of vanilla implementation.
///
/// *  Order of operations is undefined.
#[expect(non_camel_case_types)]
pub struct unpack;

impl<Ty> TableEntry<Ty> for unpack
where
    Ty: Types<RustClosure = Box<dyn DLuaFfi<Ty>>>,
{
    fn build(self, table: &RootTable<Ty>, core: &mut Core<Ty>) {
        let fn_body = crate::ffi::table::unpack();
        let key = core.gc.intern("unpack".into());
        let callback = core.gc.alloc_cell(boxed(fn_body));

        core.gc[table].set(
            KeyValue::String(LuaPtr(key.downgrade())),
            Value::Function(Callable::Rust(LuaPtr(callback.downgrade()))),
        );
    }
}
