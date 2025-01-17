use std::error::Error;
use std::fmt::Display;
use std::num::NonZeroU8;

use lexical::{parse_with_options, NumberFormatBuilder, ParseFloatOptions, ParseIntegerOptions};

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
        .no_exponent_notation(false)
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

const FLOAT_BASE10: u128 = configure_float(10, Some(N10), Some(N10));
const FLOAT_BASE16: u128 = configure_float(16, Some(N2), Some(N10));

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

fn strip_hex_prefix(s: &str) -> Option<&str> {
    s.strip_prefix("0x").or_else(|| s.strip_prefix("0X"))
}

pub fn parse_int(s: &str) -> Result<i64, UnknownNumberFormatError> {
    let r = if let Some(hex) = strip_hex_prefix(s) {
        parse_with_options::<_, _, INT_BASE16>(hex, &INT_OPTS)
    } else {
        parse_with_options::<_, _, INT_BASE10>(s, &INT_OPTS)
    };

    r.map_err(|_| UnknownNumberFormatError)
}

pub fn parse_float(s: &str) -> Result<f64, UnknownNumberFormatError> {
    let r = if let Some(hex) = strip_hex_prefix(s) {
        parse_with_options::<_, _, FLOAT_BASE16>(hex, &float_opts_exp2())
    } else {
        parse_with_options::<_, _, FLOAT_BASE10>(s, &float_opts_exp10())
    };

    r.map_err(|_| UnknownNumberFormatError)
}

/// We permit leading sign
pub fn parse(s: &str) -> Result<Number, UnknownNumberFormatError> {
    if let Ok(r) = parse_int(s) {
        Ok(Number::Int(r))
    } else {
        parse_float(s).map(Number::Float)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

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

impl Error for UnknownNumberFormatError {}

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
        test_float("+0.0", 0.0);
        test_float("-0.0", -0.0);

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
    }
}
