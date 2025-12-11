use std::borrow::Cow;
use std::error::Error;
use std::ffi::OsString;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

use gc::Trace;

use super::{Concat, Int, Len};
use crate::ffi::arg_parser::ParseFrom;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash, Trace)]
pub struct PossiblyUtf8Vec(pub Vec<u8>);

impl Len for PossiblyUtf8Vec {
    fn len(&self) -> Int {
        let len = self.0.len();
        Int(len.try_into().unwrap())
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

impl From<&[u8]> for PossiblyUtf8Vec {
    fn from(value: &[u8]) -> Self {
        Self(value.to_vec())
    }
}

impl From<String> for PossiblyUtf8Vec {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<&str> for PossiblyUtf8Vec {
    fn from(value: &str) -> Self {
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

impl ParseFrom<PossiblyUtf8Vec> for Vec<u8> {
    type Error = std::convert::Infallible;

    fn parse(value: &PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        Ok(value.into())
    }
}

impl ParseFrom<PossiblyUtf8Vec> for String {
    type Error = std::string::FromUtf8Error;

    fn parse(value: &PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl ParseFrom<PossiblyUtf8Vec> for OsString {
    type Error = std::string::FromUtf8Error;

    fn parse(value: &PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        value.try_into()
    }
}

impl ParseFrom<PossiblyUtf8Vec> for PathBuf {
    type Error = std::string::FromUtf8Error;

    fn parse(value: &PossiblyUtf8Vec) -> Result<Self, Self::Error> {
        value.try_into()
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

pub trait AsEncoding {
    /// View raw byte content of a Lua string.
    fn as_bytes(&self) -> Option<&[u8]>;

    /// View Lua string content as utf8-encoded string.
    fn as_str(&self) -> Option<&str>;
}

pub trait IntoEncoding: AsEncoding {
    /// Convert Lua string into byte array.
    ///
    /// Notice that this function is *infallible*.
    /// Lua inherently expects that it can always treat strings as bag-of-bytes.
    ///
    /// The return type is [`Cow`] because a lot of the time Rust side doesn't require ownership in order to perform work,
    /// so if your type already keeps the data in contiguous memory you may simply return a `&[u8]` pointing to it
    /// and let Rust decide whether it needs to take ownership.
    fn to_bytes(&self) -> Cow<'_, [u8]>;

    /// Convert Lua string into utf8-encoded string.
    ///
    /// This function should return `None` when value cannot be interpreted as text.
    ///
    /// This function is an important interaction point between Lua-specific type and Rust ecosystem.
    /// Lua states that it is encoding-agnostic, that is its string type can have arbitrary encoding.
    /// This becomes a point of contention when interacting with Rust ecosystem:
    /// Rust functions that work with strings expect it to be utf8-encoded, e.g. `&str` or `String`.
    /// This method provides you with a way to convert from one to another.
    ///
    /// The return type is [`Cow`] because a lot of the time Rust side doesn't require ownership in order to perform work,
    /// so if your type already contains text encoded in utf8 you may simply return a `&str` pointing into it.
    fn to_str(&self) -> Option<Cow<'_, str>>;
}

pub trait FromEncoding:
    for<'a> From<&'a [u8]> + From<Vec<u8>> + for<'a> From<&'a str> + From<String>
{
}

impl<T> FromEncoding for T where
    T: for<'a> From<&'a [u8]> + From<Vec<u8>> + for<'a> From<&'a str> + From<String>
{
}

impl AsEncoding for Vec<u8> {
    fn as_bytes(&self) -> Option<&[u8]> {
        Some(self)
    }

    fn as_str(&self) -> Option<&str> {
        None
    }
}

impl AsEncoding for String {
    fn as_bytes(&self) -> Option<&[u8]> {
        Some(self.as_bytes())
    }

    fn as_str(&self) -> Option<&str> {
        Some(self)
    }
}

impl AsEncoding for PossiblyUtf8Vec {
    fn as_bytes(&self) -> Option<&[u8]> {
        Some(self.as_ref())
    }

    fn as_str(&self) -> Option<&str> {
        std::str::from_utf8(self.as_ref()).ok()
    }
}

impl<T> AsEncoding for Interned<T>
where
    T: AsEncoding,
{
    fn as_bytes(&self) -> Option<&[u8]> {
        self.as_inner().as_bytes()
    }

    fn as_str(&self) -> Option<&str> {
        self.as_inner().as_str()
    }
}

impl IntoEncoding for Vec<u8> {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        self.as_slice().into()
    }

    fn to_str(&self) -> Option<Cow<'_, str>> {
        None
    }
}

impl IntoEncoding for String {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        self.as_bytes().into()
    }

    fn to_str(&self) -> Option<Cow<'_, str>> {
        Some(self.into())
    }
}

impl IntoEncoding for PossiblyUtf8Vec {
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        self.0.as_slice().into()
    }

    fn to_str(&self) -> Option<Cow<'_, str>> {
        let s = std::str::from_utf8(&self.0).ok()?;
        Some(s.into())
    }
}

impl<T> IntoEncoding for Interned<T>
where
    T: IntoEncoding,
{
    fn to_bytes(&self) -> Cow<'_, [u8]> {
        self.as_inner().to_bytes()
    }

    fn to_str(&self) -> Option<Cow<'_, str>> {
        self.as_inner().to_str()
    }
}

use gc::userdata::Params;
use gc::{Gc, Interned, Root};

use crate::error::{AlreadyDroppedOr, NotTextError};

pub fn try_gc_to_str<T, M, P>(
    ptr: Gc<Interned<T>>,
    heap: &gc::Heap<M, P>,
) -> Result<Cow<'_, str>, AlreadyDroppedOr<NotTextError<Interned<T>>>>
where
    T: IntoEncoding + 'static,
    P: Params,
{
    use crate::gc::TryGet;

    let value = heap.try_get(ptr)?;
    match value.to_str() {
        Some(r) => Ok(r),
        None => {
            let value = heap.try_upgrade(ptr)?;
            Err(AlreadyDroppedOr::Other(NotTextError(value)))
        }
    }
}

pub fn try_root_to_str<'h, T, M, P>(
    ptr: &Root<Interned<T>>,
    heap: &'h gc::Heap<M, P>,
) -> Result<Cow<'h, str>, NotTextError<Interned<T>>>
where
    T: IntoEncoding + 'static,
    P: Params,
{
    let value = heap.get_root(ptr);
    match value.to_str() {
        Some(r) => Ok(r),
        None => Err(NotTextError(ptr.clone())),
    }
}
