use std::error::Error;
use std::ffi::OsString;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

use gc::Trace;

use super::{Concat, Len, TypeMismatchOrError, Types, Value};
use crate::gc::{TryFromWithGc, TryIntoWithGc};

pub struct LuaString<T>(pub T);

impl<T, Ty, Gc> TryFromWithGc<Value<Ty>, Gc> for LuaString<T>
where
    Ty: Types,
    Ty::String: TryIntoWithGc<T, Gc>,
{
    type Error = TypeMismatchOrError<<Ty::String as TryIntoWithGc<T, Gc>>::Error>;

    fn try_from_with_gc(value: Value<Ty>, gc: &mut Gc) -> Result<Self, Self::Error> {
        match value {
            Value::String(t) => {
                let r = t.try_into_with_gc(gc).map_err(TypeMismatchOrError::Other)?;
                Ok(LuaString(r))
            }
            value => {
                use super::{Type, TypeMismatchError};

                let err = TypeMismatchError {
                    expected: Type::String,
                    found: value.type_(),
                };

                Err(TypeMismatchOrError::TypeMismatch(err))
            }
        }
    }
}

impl<T, Gc> From<LuaString<T>> for Value<Gc>
where
    Gc: Types,
    T: Into<Gc::String>,
{
    fn from(value: LuaString<T>) -> Self {
        let LuaString(value) = value;

        Value::String(value.into())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct PossiblyUtf8Vec(pub Vec<u8>);

impl Trace for PossiblyUtf8Vec {
    fn trace(&self, _collector: &mut gc::Collector) {}
}

impl Len for PossiblyUtf8Vec {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl Concat for PossiblyUtf8Vec {
    fn concat(&mut self, other: &Self) {
        self.0.extend(&other.0);
    }
}

impl AsRef<[u8]> for PossiblyUtf8Vec {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl From<Vec<u8>> for PossiblyUtf8Vec {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl From<String> for PossiblyUtf8Vec {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl<'a> From<&'a str> for PossiblyUtf8Vec {
    fn from(value: &'a str) -> Self {
        value.to_string().into()
    }
}

impl TryFrom<OsString> for PossiblyUtf8Vec {
    type Error = InvalidUtf8Error;

    fn try_from(value: OsString) -> Result<Self, Self::Error> {
        value
            .into_string()
            .map(|s| Self(s.into()))
            .map_err(|_| InvalidUtf8Error)
    }
}

impl TryFrom<PathBuf> for PossiblyUtf8Vec {
    type Error = InvalidUtf8Error;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        value.into_os_string().try_into()
    }
}

impl From<PossiblyUtf8Vec> for Vec<u8> {
    fn from(value: PossiblyUtf8Vec) -> Self {
        value.0
    }
}

impl TryFrom<PossiblyUtf8Vec> for String {
    type Error = std::string::FromUtf8Error;

    fn try_from(value: PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        String::from_utf8(value.0)
    }
}

impl TryFrom<PossiblyUtf8Vec> for OsString {
    type Error = <String as TryFrom<PossiblyUtf8Vec>>::Error;

    fn try_from(value: PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        let s: String = value.try_into()?;
        Ok(s.into())
    }
}

impl TryFrom<PossiblyUtf8Vec> for PathBuf {
    type Error = <String as TryFrom<PossiblyUtf8Vec>>::Error;

    fn try_from(value: PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        let s: String = value.try_into()?;
        Ok(s.into())
    }
}

impl<'a> From<&'a PossiblyUtf8Vec> for Vec<u8> {
    fn from(value: &'a PossiblyUtf8Vec) -> Self {
        value.0.clone()
    }
}

impl<'a> TryFrom<&'a PossiblyUtf8Vec> for String {
    type Error = std::string::FromUtf8Error;

    fn try_from(value: &'a PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        String::from_utf8(value.0.clone())
    }
}

impl<'a> TryFrom<&'a PossiblyUtf8Vec> for OsString {
    type Error = <String as TryFrom<PossiblyUtf8Vec>>::Error;

    fn try_from(value: &'a PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        let s: String = value.try_into()?;
        Ok(s.into())
    }
}

impl<'a> TryFrom<&'a PossiblyUtf8Vec> for PathBuf {
    type Error = <String as TryFrom<PossiblyUtf8Vec>>::Error;

    fn try_from(value: &'a PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        let s: String = value.try_into()?;
        Ok(s.into())
    }
}

impl Debug for PossiblyUtf8Vec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match std::str::from_utf8(&self.0) {
            Ok(s) => write!(f, "{s:?}"),
            Err(_) => write!(f, "{:?}", &self.0),
        }
    }
}

impl Display for PossiblyUtf8Vec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(s) = std::str::from_utf8(&self.0) {
            write!(f, "{s}")
        } else {
            write!(f, "<binary payload>")
        }
    }
}

#[derive(Debug)]
pub struct InvalidUtf8Error;

impl Display for InvalidUtf8Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "string does not contain valid utf8")
    }
}

impl Error for InvalidUtf8Error {}
