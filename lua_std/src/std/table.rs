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
