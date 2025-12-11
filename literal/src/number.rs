use std::fmt::Display;
use std::num::NonZeroU8;

use lexical::{
    self, parse_with_options, Error, NumberFormatBuilder, ParseFloatOptions, ParseIntegerOptions,
};

const fn configure_int(radix: u8) -> u128 {
    NumberFormatBuilder::new()
        .digit_separator(None)
        .mantissa_radix(radix)
        .base_prefix(None)
        .base_suffix(None)
        .no_positive_mantissa_sign(false)
        .required_mantissa_sign(false)
        .no_integer_leading_zeros(false)
        .build()
}

const fn configure_float(
    radix: u8,
    exponent_base: Option<NonZeroU8>,
    exponent_radix: Option<NonZeroU8>,
    forbid_exponent: bool,
) -> u128 {
    NumberFormatBuilder::new()
        .digit_separator(None)
        .mantissa_radix(radix)
        .exponent_base(exponent_base)
        .exponent_radix(exponent_radix)
        .base_prefix(None)
        .base_suffix(None)
        .required_integer_digits(true)
        .required_mantissa_digits(true)
        .required_exponent_digits(true)
        .no_positive_mantissa_sign(false)
        .required_mantissa_sign(false)
        .no_exponent_notation(forbid_exponent)
        .required_exponent_notation(false)
        .no_positive_exponent_sign(false)
        .required_exponent_sign(false)
        .no_exponent_without_fraction(false)
        .no_special(true)
        .no_integer_leading_zeros(false)
        .no_float_leading_zeros(false)
        // This ensures that character separating exponent is case-insensitive.
        .case_sensitive_exponent(false)
        .build()
}

const N2: NonZeroU8 = NonZeroU8::new(2).unwrap();
const N10: NonZeroU8 = NonZeroU8::new(10).unwrap();

const INT_BASE10: u128 = configure_int(10);
const INT_BASE16: u128 = configure_int(16);

const INT_OPTS: ParseIntegerOptions = ParseIntegerOptions::new();

const FLOAT_BASE10: u128 = configure_float(10, Some(N10), Some(N10), false);
const FLOAT_BASE16: u128 = configure_float(16, Some(N2), Some(N10), false);
const FLOAT_BASE16_NO_EXP: u128 = configure_float(16, None, None, true);

fn float_opts_exp10() -> ParseFloatOptions {
    ParseFloatOptions::builder()
        .exponent(b'e')
        .decimal_point(b'.')
        .build()
        .unwrap()
}

fn float_opts_exp2() -> ParseFloatOptions {
    ParseFloatOptions::builder()
        .exponent(b'p')
        .decimal_point(b'.')
        .build()
        .unwrap()
}

/// Strip prefix indicating hex number.
///
/// This function will also parse (optional) leading sign.
/// Produces `(is_positive, s)` pair, where `is_positive` indicates sign of entire espression.
/// Guarantees that `s` does not have leading sign.
fn strip_hex_prefix(s: &str) -> Option<(bool, &str)> {
    let (sign, s) = if let Some(s) = s.strip_prefix('-') {
        (false, s)
    } else if let Some(s) = s.strip_prefix('+') {
        (true, s)
    } else {
        (true, s)
    };

    let s = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X"))?;

    if s.strip_prefix('-').is_some() || s.strip_prefix('+').is_some() {
        None
    } else {
        Some((sign, s))
    }
}

fn inner_parse_int(s: &str) -> Option<i64> {
    if let Some((is_positive, hex)) = strip_hex_prefix(s) {
        let r: i64 = parse_with_options::<_, _, INT_BASE16>(hex, &INT_OPTS).ok()?;
        if is_positive {
            Some(r)
        } else {
            r.checked_neg()
        }
    } else {
        parse_with_options::<_, _, INT_BASE10>(s, &INT_OPTS).ok()
    }
}

/// Parse string as Lua-denoted integer.
///
/// See [`parse`] for more details.
pub fn parse_int(s: &str) -> Result<i64, UnknownNumberFormatError> {
    inner_parse_int(s).ok_or(UnknownNumberFormatError)
}

/// Main hexadecimal float parser.
#[expect(unused)]
fn parse_float_hex(s: &str) -> Option<f64> {
    parse_with_options::<_, _, FLOAT_BASE16>(s, &float_opts_exp2()).ok()
}

/// Surrogate hexadecimal float parser.
///
/// Out parser is based on top of excellent [`lexical`] library.
/// However, at the time of writing some of their [issues][lexical:issue#87] prevent us from properly parsing hexadecimal float's exponents.
///
/// The workaround we use is to parse main part of the float and its exponent separately and later recombine.
/// This unfortunately implies floating point operations, which can reduce accuracy.
///
/// When the issue is solved, we will switch back to the [main parser](parse_float_hex).
///
/// [lexical:issue#87]: https://github.com/Alexhuszagh/rust-lexical/issues/87
fn parse_float_hex_surrogate(s: &str) -> Option<f64> {
    use lexical::{parse, parse_partial_with_options};

    let (r, offset) =
        match parse_partial_with_options::<_, _, FLOAT_BASE16_NO_EXP>(s, &float_opts_exp2()) {
            Ok(r) => r,
            // Seems to be a bug in lexical.
            // It fails when encounters exponent separator even when exponent is disabled in format.
            // There is no way to remove exponent symbol from opts as well.
            // Hopefully, rustc can optimize this away?
            Err(Error::InvalidExponent(offset)) => {
                let r = parse_with_options::<_, _, FLOAT_BASE16_NO_EXP>(
                    &s[..offset],
                    &float_opts_exp2(),
                )
                .unwrap();
                (r, offset)
            }
            _ => return None,
        };
    let s = &s[offset..];

    let exponent_part = s.strip_prefix('p').or_else(|| s.strip_prefix('P'));
    let exponent = if let Some(s) = exponent_part {
        parse(s).ok()?
    } else if s.is_empty() {
        return Some(r);
    } else {
        return None;
    };

    let mul = 2.0_f64.powi(exponent);

    Some(r * mul)
}

fn inner_parse_float(s: &str) -> Option<f64> {
    if let Some((is_positive, hex)) = strip_hex_prefix(s) {
        let r = parse_float_hex_surrogate(hex)?;
        let r = if is_positive { r } else { -r };
        Some(r)
    } else {
        parse_with_options::<_, _, FLOAT_BASE10>(s, &float_opts_exp10()).ok()
    }
}

/// Parse string as Lua-denoted floating point number.
///
/// See [`parse`] for more details.
pub fn parse_float(s: &str) -> Result<f64, UnknownNumberFormatError> {
    inner_parse_float(s).ok_or(UnknownNumberFormatError)
}

/// Parse string as Lua-denoted number (int or float).
///
/// Entire string is expected to contain exactly one number without any trailing or preceding characters (including whitespace).
///
/// This function will attempt to parse it as an integer first, and if that fails as a float.
/// In particular, if the string contains integer that doesn't fit into `i64` it will be parsed as a floating point number instead.
/// This behavior is [expected by Lua][lua#3.1].
///
/// If you want better control over resulting type consider using [`parse_int`] or [`parse_float`].
///
/// # Lua number notation
///
/// Lua integers can be present in either decimal or hexadecimal notation:
///
/// * `[+-]?[0-9]+`
/// * `[+-]?0[xX][0-9a-fA-F]+`
///
/// Lua floats also have both decimal and hexadecimal notation:
///
/// * `[+-]?[0-9]+(.[0-9]+)?([eE][+-]?[0-9]+)?`
/// * `[+-]?0[xX][0-9a-fA-F]+(.[0-9a-fA-F]+)?([pP][+-]?[0-9]+)?`
///
/// Note that
///
/// * All parsing is case-insensitive, this includes hex digits, exponent separator and hex prefix.
/// * Hexadecimal numbers must be prefixed with `0x` (or `0X`).
/// *   Floats use different exponent separator, based on the form.
///     Decimal numbers use `e`/`E` where hexadecimal use `p`/`P`.
/// * Each part of float (if present) must have at least one digit.
/// * Exponent of hexadecimal float is written in *decimal* notation and uses 2 as base.
///
/// # Differences from Lua
///
/// *   The function permits leading optional sign (`+` or `-`).
///     Lua spec doesn't allow that, but it is mostly done in order to simplify tokenizing and disambiguating Lua programs.
///     Leading signs are still permitted in source, but processed outside of number parser.
///     
///     In general it is expected that numbers can include a leading sign, so we comply to this expectation.
///
/// *   Lua mandates that overflowing *decimal* integers are converted to float, however overflowing *hexadecimal* integers are simply truncated.
///
///     Currently, all integers will be converted to float on overflow.
///
/// [lua#3.1]: https://www.lua.org/manual/5.4/manual.html#3.1
pub fn parse(s: &str) -> Result<Number, UnknownNumberFormatError> {
    if let Ok(r) = parse_int(s) {
        Ok(Number::Int(r))
    } else {
        parse_float(s).map(Number::Float)
    }
}

/// Integer or float.
///
/// This enum only exists to hold output of [`parse`] function.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

/// String does not contain number in a known format.
#[derive(Debug, Clone, Copy)]
pub struct UnknownNumberFormatError;

impl Display for UnknownNumberFormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "string does not encode number in Lua-compatible notation"
        )
    }
}

impl std::error::Error for UnknownNumberFormatError {}

#[cfg(test)]
mod tests {
    use super::*;

    fn fail(s: &str) {
        assert!(parse(s).is_err(), "{}", s)
    }

    fn test_int(s: &str, n: i64) {
        assert_eq!(parse(s).ok(), Some(Number::Int(n)), "{} | {}", s, n);
    }

    fn test_float(s: &str, n: f64) {
        assert_eq!(parse(s).ok(), Some(Number::Float(n)), "{} | {}", s, n);
    }

    #[test]
    fn int_base10() {
        test_int("0", 0);
        test_int("3", 3);
        test_int("345", 345);

        test_int("000", 0);
        test_int("0015", 15);

        test_int("1234567890", 1234567890);

        test_int("9223372036854775807", 9223372036854775807);
    }

    #[test]
    fn int_base16() {
        test_int("0x0", 0);
        test_int("0X0", 0);

        test_int("0x3", 3);
        test_int("0x345", 837);

        test_int("0x000", 0);
        test_int("0x0015", 21);

        test_int("0x12345", 74565);
        test_int("0x67890", 424080);
        test_int("0xabcdef", 11259375);

        test_int("0X12345", 74565);
        test_int("0X67890", 424080);
        test_int("0XABCDEF", 11259375);

        test_int("0x7FFFFFFFFFFFFFFF", 9223372036854775807);
    }

    #[test]
    fn int_leading_sign() {
        test_int("+0", 0);
        test_int("-0", 0);

        test_int("+9223372036854775807", 9223372036854775807);
        test_int("-9223372036854775808", -9223372036854775808);
    }

    #[test]
    fn float_base10() {
        #![expect(clippy::approx_constant)]

        test_float("0.0", 0.0);
        test_float("-0.0", -0.0);
        test_float("+0.0", 0.0);

        test_float("3.0", 3.0);
        test_float("3.1416", 3.1416);

        test_float("34e1", 34.0e1);
        test_float("34e+1", 34.0e1);
        test_float("34e-1", 34.0e-1);
        test_float("34E1", 34.0e1);
        test_float("34.e1", 34.0e1);

        test_float("314.16e2", 314.16e2);
        test_float("314.16e+2", 314.16e2);
        test_float("314.16e-2", 314.16e-2);

        test_float("0.31416e1", 0.31416e1);
        test_float("0.31416E1", 0.31416e1);

        test_float("0123.45678e9", 123456780000.0);
    }

    #[test]
    fn float_base16() {
        test_float("0x0.0", 0.0);
        test_float("+0x0.0", 0.0);
        test_float("-0x0.0", -0.0);
        test_float("0X0.0", 0.0);

        test_float("0x0.1E", 0.1171875);
        test_float("0x0.0e1", 0.054931640625);

        test_float("0x123456.7890", 1193046.4709472656);
        test_float("0xabcd.ef", 43981.93359375);
        test_float("0xABCD.EF", 43981.93359375);
    }

    #[test]
    fn float_base16_exp2() {
        #![expect(clippy::approx_constant)]

        test_float("0xA23p-4", 162.1875);
        test_float("0X1.921FB54442D18P+1", 3.141592653589793);

        test_float("0x1.2p4", 18.0);
        test_float("0x1.2p+4", 18.0);
        test_float("0x1.2p-4", 0.0703125);

        test_float("0x1234.5678p9", 2386092.9375);
        test_float("0xabcdefp-3", 1407421.875);
        test_float("0xABCDEFP-3", 1407421.875);
    }

    #[test]
    fn float_fail() {
        fail(".");
        fail("e");
        fail("E");

        fail("0e");
        fail("0.e");

        fail("0x0p");
        fail("0x0.p");

        fail(".1");
        fail("0x.1");

        fail("0x-1.0");
        fail("0x+1.0");

        fail("0.0p1");
    }
}
