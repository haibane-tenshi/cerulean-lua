use crate::value::{CoreTypes, Types, Value, Weak, WeakValue};
use repr::opcode::{AriBinOp, BitBinOp, StrBinOp};

/// Define fine aspects of runtime behavior.
///
/// Currently this type is used to control which type coercions runtime should perform.
/// See individual methods for details on altered behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct DialectBuilder {
    una_op_bit_not_float_to_int: bool,
    bin_op_bit_float_to_int: bool,
    bin_op_ari_int_to_float: bool,
    bin_op_ari_exp_2int_to_float: bool,
    bin_op_ari_div_2int_to_float: bool,
    bin_op_str_concat_int_to_str: bool,
    bin_op_str_concat_float_to_str: bool,
    bin_op_rel_cmp_float_and_int: bool,
    tab_set_float_to_int: bool,
    tab_get_float_to_int: bool,
}

impl DialectBuilder {
    /// Construct builder with no enabled type coercions.
    pub fn no_coercions() -> Self {
        Default::default()
    }

    /// Construct builder enabled with all coercions compatible with Lua 5.4
    pub fn lua_5_4() -> Self {
        *Self::no_coercions()
            .with_bit_float_to_int(true)
            .with_ari_int_to_float(true)
            .with_concat_number_to_str(true)
            .with_rel_cmp_float_and_int(true)
            .with_tab_index_float_to_int(true)
    }

    /// Perform float to int coercion in unary bitwise operators.
    ///
    /// This flag applies to following unary ops:
    /// * `~` - bitwise not
    ///
    /// Conversion applies if operand has type `float`.
    /// * When flag is set to `true` runtime will attempt to convert float to int.
    ///     If successful, op is performed on resulting integer.
    ///     If failed runtime will proceed to next step.
    /// * When flag is set to `false` runtime will proceed to next step.
    ///
    /// If no conversion happened (regardless of whether it failed or flag is set to `false`),
    /// runtime will attempt to invoke corresponding metamethod.
    /// If no metamethod found, Lua panic is invoked.
    ///
    /// See [Int::try_from](crate::value::Int::try_from) for conversion details.
    pub fn with_una_op_bit_not_float_to_int(&mut self, value: bool) -> &mut Self {
        self.una_op_bit_not_float_to_int = value;
        self
    }

    /// Perfrom float to int coercion in binary bitwise operators.
    ///
    /// This flag applies to following binary ops:
    /// * `&` - bitwise and
    /// * `|` - bitwise or
    /// * `~` - bitwise xor
    /// * `<<` - bitwise left shift
    /// * `>>` - bitwise right shift
    ///
    /// Converson applies when operands have mixed numeric types or both have type `float`:
    /// * `int @ int` -> `int`
    /// * `float @ int` or `int @ float` -> controlled by this flag
    ///     
    ///     When flag is set to `true` runtime will attempt to convert float to int.
    ///     If successful, op is performed on resulting integers.
    ///     If failed runtime will proceed to next step.
    /// * `float @ float` -> controlled by this flag
    ///     
    ///     When flag is set to `true` runtime will attempt to convert each float to int.
    ///     If both conversions successful, op is performed on resulting integers.
    ///     If either conversion fails runtime will proceed to next step with original values.
    ///
    /// If types are not normalized to `int @ int` (regardless of whether a conversion failed or flag is set to `false`),
    /// runtime will attempt to invoke corresponding metamethod.
    /// If no metamethod found, Lua panic is invoked.
    ///
    /// See [Int::try_from](crate::value::Int::try_from) for conversion details.
    pub fn with_bin_op_bit_float_to_int(&mut self, value: bool) -> &mut Self {
        self.bin_op_bit_float_to_int = value;
        self
    }

    /// Perform float to int coercion in bitwise operators.
    ///
    /// This flag is combination of
    /// [`with_una_op_bit_not_float_to_int`](Self::with_una_op_bit_not_float_to_int)
    /// and [`with_bin_op_bit_float_to_int`](Self::with_bin_op_bit_float_to_int).
    pub fn with_bit_float_to_int(&mut self, value: bool) -> &mut Self {
        self.with_una_op_bit_not_float_to_int(value);
        self.with_bin_op_bit_float_to_int(value)
    }

    /// Perform int to float coercion in binary arithemtic operators on mixed types.
    ///
    /// This flag applies to following binary ops:
    /// * `+` - addition
    /// * `-` - subtraction
    /// * `*` - multiplication
    /// * `/` - division
    /// * `//` - floor division
    /// * `%` - modulo
    /// * `^` - exponentiation
    ///
    /// Converson applies only when operands have mixed numeric types:
    /// * `int @ int` -> `int`
    /// * `float @ float` -> `float`
    /// * `int @ float` or `float @ int` -> controlled by this flag.
    ///     * When flag is set to `true` both operands are converted to floats
    ///       then op is performed on resulting values.
    ///     * When flag is set to `false` Lua panic is invoked.
    pub fn with_bin_op_ari_int_to_float(&mut self, value: bool) -> &mut Self {
        self.bin_op_ari_int_to_float = value;
        self
    }

    /// Perform int to float coercion in exponentiation when both operans are ints.
    ///
    /// This flag applies to following binary ops:
    /// * `^` - exponentiation
    ///
    /// Lua spec prescribes that exponentiation is only performed on floats.
    /// * When flag is set to `true` both operands are converted to floats
    ///     then op is performed on resulting values.
    ///     The result is float.
    /// * When flag is set to `false` op is perfromed on integers
    ///     following the Lua rules for integer arithmetic (wrapping on overflow).
    ///     The result is integer.
    ///
    /// Note that integer exponentiation is only defined for non-negative exponents.
    /// Attempt to use negative exponent will result int Lua panic.
    pub fn with_bin_op_ari_exp_2int_to_float(&mut self, value: bool) -> &mut Self {
        self.bin_op_ari_exp_2int_to_float = value;
        self
    }

    /// Perform int to float coercion in division when both operans are ints.
    ///
    /// This flag applies to following binary ops:
    /// * `/` - division
    ///
    /// Lua spec prescribes that division is only performed on floats.
    /// (Note that it only applies to normal division `/`,
    /// floor div `//` respects its operand types).
    /// * When flag is set to `true` both operands are converted to floats
    ///     then op is performed on resulting values.
    ///     The result is float.
    /// * When flag is set to `false` op is perfromed on integers
    ///     following the Lua rules for integer arithmetic (wrapping on overflow).
    ///     The result is integer.
    ///
    /// Note that division by 0 in integer arithmetic results in Lua panic
    /// (as opposed to `NaN` in floats).
    pub fn with_bin_op_ari_div_2int_to_float(&mut self, value: bool) -> &mut Self {
        self.bin_op_ari_div_2int_to_float = value;
        self
    }

    /// Perfrom int to float coercions in arithmetic operations on mixed types.
    ///
    /// This flag is equivalent to [`with_bin_op_ari_int_to_float`](Self::with_bin_op_ari_int_to_float).
    pub fn with_ari_mixed_int_to_float(&mut self, value: bool) -> &mut Self {
        self.with_bin_op_ari_int_to_float(value)
    }

    /// Perfrom int to float coercions in arithmetic operations.
    ///
    /// This flag is combination of
    /// [`with_bin_op_ari_int_to_float`](Self::with_bin_op_ari_int_to_float),
    /// [`with_bin_op_ari_exp_2int_to_float`](Self::with_bin_op_ari_exp_2int_to_float) and
    /// [`with_bin_op_ari_div_2int_to_float`](Self::with_bin_op_ari_div_2int_to_float).
    pub fn with_ari_int_to_float(&mut self, value: bool) -> &mut Self {
        self.with_ari_mixed_int_to_float(value);
        self.with_bin_op_ari_exp_2int_to_float(value);
        self.with_bin_op_ari_div_2int_to_float(value)
    }

    /// Perform int to string coercion in concatenation operator.
    ///
    /// This flag applies to following binary ops:
    /// * `..` - string concatenation
    ///
    /// Converson applies when any operand (or both) have type `int`:
    /// * `int @ _`, `_ @ int` -> conrolled by this flag
    ///
    /// When flag is set to `false`, Lua panic is invoked.
    ///
    /// Note that Lua spec does not specify formatting details
    /// besides it [being *human-readable*][lua_ref#3.4.3],
    /// in particular it doesn't state anything about possibility to round-trip such coercion
    /// or string being coercible back to integer altogether.
    ///
    /// [lua_ref#3.4.3]: https://www.lua.org/manual/5.4/manual.html#3.4.3
    pub fn with_bin_op_str_concat_int_to_str(&mut self, value: bool) -> &mut Self {
        self.bin_op_str_concat_int_to_str = value;
        self
    }

    /// Perform float to string coercion in concatenation operator.
    ///
    /// This flag applies to following binary ops:
    /// * `..` - string concatenation
    ///
    /// Converson applies when any operand (or both) have type `float`:
    /// * `float @ _`, `_ @ float` -> conrolled by this flag
    ///
    /// When flag is set to `false`, Lua panic is invoked.
    ///
    /// Note that Lua spec does not specify formatting details
    /// besides it [being *human-readable*][lua_ref#3.4.3],
    /// in particular it doesn't state anything about possibility to round-trip such coercion
    /// or string being coercible back to float altogether.
    ///
    /// [lua_ref#3.4.3]: https://www.lua.org/manual/5.4/manual.html#3.4.3
    pub fn with_bin_op_str_concat_float_to_str(&mut self, value: bool) -> &mut Self {
        self.bin_op_str_concat_float_to_str = value;
        self
    }

    /// Perform int/float to string coercion in concatenation operator.
    ///
    /// This flag is combination of
    /// [`with_bin_op_str_concat_int_to_str`](Self::with_bin_op_str_concat_int_to_str),
    /// [`with_bin_op_str_concat_float_to_str`](Self::with_bin_op_str_concat_float_to_str),
    pub fn with_concat_number_to_str(&mut self, value: bool) -> &mut Self {
        self.with_bin_op_str_concat_int_to_str(value);
        self.with_bin_op_str_concat_float_to_str(value)
    }

    /// Control whether floats and integers are comparable.
    ///
    /// This flag applies to following binary ops:
    /// * `==` - equality comparison
    /// * `~=` - inequality comparison
    /// * `<` - less than
    /// * `<=` - less than or equal
    /// * `>` - greater than
    /// * `>=` - greater than or equal
    ///
    /// Behavior applies only when operands have mixed numeric types:
    /// * `int @ int` -> bool
    /// * `float @ float` -> bool
    /// * `int @ float` or `float @ int` -> controlled by this flag
    ///     * When flag is set to `true` runtime will follow Lua spec
    ///         which prescribes that two [numbers are compared according to their methematical values][lua_ref#3.4.4]
    ///         regardless of underlying type.
    ///     * When flag is set to `false` ints and floats are not comparable to each other.
    ///         Equality and inequality comparison will produce `false`:
    ///         
    ///         ```lua
    ///         assert(not(1.0 == 1) and not (1.0 ~= 1))
    ///         ```
    ///         
    ///         Ordering comparisons will result in Lua panic.
    ///
    ///         This behavior is consistent with how other mixed type comparisons are handled.
    ///
    /// [lua_ref#3.4.4]: https://www.lua.org/manual/5.4/manual.html#3.4.4
    pub fn with_rel_cmp_float_and_int(&mut self, value: bool) -> &mut Self {
        self.bin_op_rel_cmp_float_and_int = value;
        self
    }

    /// Perform float to int coercion in table key during assignment.
    ///
    /// Lua spec prescribes that float keys
    /// that represent an exact integer are converted to int on table access.
    /// This flag controls wether such conversion happens on table assignments.
    ///
    /// Note that this applies only to accesses performed by runtime as a result of executing opcodes.
    ///
    /// See [Int::try_from](crate::value::Int::try_from) for conversion details.
    pub fn with_tab_set_float_to_int(&mut self, value: bool) -> &mut Self {
        self.tab_set_float_to_int = value;
        self
    }

    /// Perform float to int coercion in table key during lookup.
    ///
    /// Lua spec prescribes that float keys
    /// that represent an exact integer are converted to int on table access.
    /// This flag controls wether such conversion happens on table lookups.
    ///
    /// Note that this applies only to accesses performed by runtime as a result of executing opcodes.
    ///
    /// See [Int::try_from](crate::value::Int::try_from) for conversion details.
    pub fn with_tab_get_float_to_int(&mut self, value: bool) -> &mut Self {
        self.tab_get_float_to_int = value;
        self
    }

    /// Perform float to int coercion on table key access.
    ///
    /// This flag is combination of
    /// [`with_tab_set_float_to_int`](Self::with_tab_set_float_to_int),
    /// [`with_tab_get_float_to_int`](Self::with_tab_get_float_to_int).
    ///
    /// Note that this applies only to accesses performed by runtime as a result of executing opcodes.
    pub fn with_tab_index_float_to_int(&mut self, value: bool) -> &mut Self {
        self.with_tab_set_float_to_int(value);
        self.with_tab_get_float_to_int(value)
    }
}

pub trait CoerceArgs<Ty: Types>: sealed::Sealed {
    type Gc;

    fn coerce_bin_op_ari(&self, op: AriBinOp, args: [Value<Ty>; 2]) -> [Value<Ty>; 2];
    fn coerce_bin_op_bit(&self, op: BitBinOp, args: [Value<Ty>; 2]) -> [Value<Ty>; 2];
    fn coerce_bin_op_str(
        &self,
        op: StrBinOp,
        args: [Value<Ty>; 2],
        gc: &mut Self::Gc,
    ) -> [Value<Ty>; 2];
    fn coerce_una_op_bit(&self, args: [Value<Ty>; 1]) -> [Value<Ty>; 1];
    fn coerce_tab_set(&self, key: Value<Ty>) -> Value<Ty>;
    fn coerce_tab_get(&self, key: Value<Ty>) -> Value<Ty>;
    fn cmp_float_and_int(&self) -> bool;
}

impl<Ty> CoerceArgs<Weak<Ty>> for DialectBuilder
where
    Ty: CoreTypes,
    Ty::String: From<String>,
{
    type Gc = gc::Heap;

    fn coerce_bin_op_ari(&self, op: AriBinOp, args: [WeakValue<Ty>; 2]) -> [WeakValue<Ty>; 2] {
        use crate::value::{Float, Int};

        match (op, args) {
            (_, [Value::Int(lhs), rhs @ Value::Float(_)]) if self.bin_op_ari_int_to_float => {
                [Float::from(Int(lhs)).into(), rhs]
            }
            (_, [lhs @ Value::Float(_), Value::Int(rhs)]) if self.bin_op_ari_int_to_float => {
                [lhs, Float::from(Int(rhs)).into()]
            }
            (AriBinOp::Pow, [Value::Int(lhs), Value::Int(rhs)])
                if self.bin_op_ari_exp_2int_to_float =>
            {
                [Float::from(Int(lhs)).into(), Float::from(Int(rhs)).into()]
            }
            (AriBinOp::Div, [Value::Int(lhs), Value::Int(rhs)])
                if self.bin_op_ari_div_2int_to_float =>
            {
                [Float::from(Int(lhs)).into(), Float::from(Int(rhs)).into()]
            }
            (_, args) => args,
        }
    }

    fn coerce_bin_op_bit(&self, _op: BitBinOp, args: [WeakValue<Ty>; 2]) -> [WeakValue<Ty>; 2] {
        use crate::value::{Float, Int};

        let try_into = |value: f64| -> WeakValue<Ty> {
            if let Ok(value) = Int::try_from(Float(value)) {
                value.into()
            } else {
                Value::Float(value)
            }
        };

        match args {
            [lhs @ Value::Int(_), Value::Float(rhs)] if self.bin_op_bit_float_to_int => {
                [lhs, try_into(rhs)]
            }
            [Value::Float(lhs), rhs @ Value::Int(_)] if self.bin_op_bit_float_to_int => {
                [try_into(lhs), rhs]
            }
            [Value::Float(lhs), Value::Float(rhs)] if self.bin_op_bit_float_to_int => {
                match [try_into(lhs), try_into(rhs)] {
                    args @ [Value::Int(_), Value::Int(_)] => args,
                    _ => [Value::Float(lhs), Value::Float(rhs)],
                }
            }
            args => args,
        }
    }

    fn coerce_bin_op_str(
        &self,
        op: StrBinOp,
        args: [WeakValue<Ty>; 2],
        gc: &mut Self::Gc,
    ) -> [WeakValue<Ty>; 2] {
        match op {
            StrBinOp::Concat => {
                use crate::gc::StringRef;
                use Value::*;

                let flt_to_string = |x: f64, _gc: &mut Self::Gc| {
                    let value = StringRef::new(x.to_string().into());
                    Value::String(value)
                };

                let int_to_string = |x: i64, _gc: &mut Self::Gc| {
                    let value = StringRef::new(x.to_string().into());
                    Value::String(value)
                };

                match args {
                    [Int(lhs), Int(rhs)] => [int_to_string(lhs, gc), int_to_string(rhs, gc)],
                    [Int(lhs), Float(rhs)] => [int_to_string(lhs, gc), flt_to_string(rhs, gc)],
                    [Int(lhs), String(rhs)] => [int_to_string(lhs, gc), String(rhs)],
                    [Float(lhs), Int(rhs)] => [flt_to_string(lhs, gc), int_to_string(rhs, gc)],
                    [Float(lhs), Float(rhs)] => [flt_to_string(lhs, gc), flt_to_string(rhs, gc)],
                    [Float(lhs), String(rhs)] => [flt_to_string(lhs, gc), String(rhs)],
                    [String(lhs), Int(rhs)] => [String(lhs), int_to_string(rhs, gc)],
                    [String(lhs), Float(rhs)] => [String(lhs), flt_to_string(rhs, gc)],
                    args => args,
                }
            }
        }
    }

    fn coerce_una_op_bit(&self, args: [WeakValue<Ty>; 1]) -> [WeakValue<Ty>; 1] {
        use crate::value::{Float, Int};

        match args {
            [Value::Float(arg)] if self.una_op_bit_not_float_to_int => {
                if let Ok(Int(arg)) = Int::try_from(Float(arg)) {
                    [Value::Int(arg)]
                } else {
                    [Value::Float(arg)]
                }
            }
            args => args,
        }
    }

    fn coerce_tab_set(&self, key: WeakValue<Ty>) -> WeakValue<Ty> {
        use crate::value::{Float, Int};

        if !self.tab_set_float_to_int {
            return key;
        }

        match key {
            Value::Float(value) => {
                if let Ok(value) = Int::try_from(Float(value)) {
                    value.into()
                } else {
                    Value::Float(value)
                }
            }
            arg => arg,
        }
    }

    fn coerce_tab_get(&self, key: WeakValue<Ty>) -> WeakValue<Ty> {
        use crate::value::{Float, Int};

        if !self.tab_get_float_to_int {
            return key;
        }

        match key {
            Value::Float(value) => {
                if let Ok(value) = Int::try_from(Float(value)) {
                    value.into()
                } else {
                    Value::Float(value)
                }
            }
            arg => arg,
        }
    }

    fn cmp_float_and_int(&self) -> bool {
        self.bin_op_rel_cmp_float_and_int
    }
}

mod sealed {
    pub trait Sealed {}

    impl Sealed for super::DialectBuilder {}
}
