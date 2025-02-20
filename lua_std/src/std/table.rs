use rt::ffi::{boxed, DLuaFfi};
use rt::gc::LuaPtr;
use rt::runtime::Core;
use rt::value::{Callable, KeyValue, TableIndex, Types, Value};

use crate::traits::{RootTable, TableEntry};

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
