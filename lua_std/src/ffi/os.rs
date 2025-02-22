/// Operating system facilities
///
/// # From Lua documentation
///
/// This library is implemented through table `os`.
use rt::ffi::{self, LuaFfi};
use rt::value::Types;

/// Return CPU time consumed by the entire process.
///
/// # From Lua documentation
///
/// **Signature:**
/// * `() -> int`
///
/// Returns an approximation of the amount in seconds of CPU time used by the program, as returned by the underlying ISO C function `clock`.
pub fn clock<Ty>() -> impl LuaFfi<Ty>
where
    Ty: Types,
{
    let body = || {
        ffi::delegate::from_mut(|mut rt| {
            use cpu_time::ProcessTime;
            use rt::ffi::arg_parser::ParseArgs;
            use rt::value::Value;

            let () = rt.stack.parse(&mut rt.core.gc)?;

            let dur = ProcessTime::now().as_duration();
            let secs = dur.as_secs();
            let output = secs.try_into().unwrap();

            rt.stack.transient().push(Value::Int(output));
            Ok(())
        })
    };

    ffi::from_fn(body, "lua_std::os::clock", ())
}
