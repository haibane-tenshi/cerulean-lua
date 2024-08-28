use std::borrow::{Cow, ToOwned};
use std::cmp::{Ordering, Reverse};
use std::collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque};
use std::convert::Infallible;
use std::ffi::{CStr, CString, OsStr, OsString};
use std::fs::{File, FileType, Metadata};
use std::net::{
    IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV4, SocketAddrV6, TcpListener, TcpStream,
    UdpSocket,
};
use std::num::{
    FpCategory, NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroU128,
    NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, Saturating, Wrapping,
};
use std::ops::{ControlFlow, Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::rc::{Rc, Weak};
use std::time::{Duration, Instant, SystemTime};

use super::Collector;
use crate::index::{Access, GcPtr};

/// Track transitive reference dependencies.
///
/// This trait is *safe*.
///
/// Correct implementor should recursively trace all of its fields
/// to ensure that garbage collector can observe weak references owned
/// by the type.
///
/// Note that an incorrect implementation does not lead to memory unsafety:
/// garbage collector uses no unsafe code.
/// However, failure to provide correct implementation will likely cause
/// some objects to be collected earlier than expected,
/// leaving *dangling* [`GcCell`]/[`Gc`] *references* behind.
/// Any attempt to dereference such reference is *safe*
/// but will fail returning `None`.
///
/// The trait is implemented for most useful types in `std`.
/// Still there is a number of types that intentionally don't provide an implementation:
///
/// * [`RootCell<T>`](crate::RootCell)/[`Root<T>`](crate::Root) -
///     comitting strong references to garbage collector allows creation of circular references
///     that leak memory similarly to [`Rc`] cycles.
/// * Raw pointers - it isn't clear if and when those should be followed through.
/// * Cells - correctly handling internal mutability requires additional knoweledge about how the value is used.
///
/// In case you want to allocate a type that cannot/should not implement `Trace`
/// consider wrapping it in [`Untrace`].
/// [`Untrace`] provides trivial trait implementation that doesn't propagate tracing to wrapped value.
pub trait Trace: 'static {
    fn trace(&self, collector: &mut Collector);
}

impl<T, A> Trace for GcPtr<T, A>
where
    T: ?Sized + 'static,
    A: Access + 'static,
{
    fn trace(&self, collector: &mut Collector) {
        collector.mark(*self)
    }
}

impl Trace for u8 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for u16 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for u32 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for u64 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for u128 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for usize {
    fn trace(&self, _: &mut Collector) {}
}

impl Trace for i8 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for i16 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for i32 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for i64 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for i128 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for isize {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for bool {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for char {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for f32 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for f64 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl<T> Trace for &'static T
where
    T: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        <T as Trace>::trace(self, collector)
    }
}

impl<T> Trace for [T]
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for item in self.iter() {
            item.trace(collector);
        }
    }
}

impl<T, const N: usize> Trace for [T; N]
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for item in self.iter() {
            item.trace(collector);
        }
    }
}

impl<T> Trace for Vec<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for item in self.iter() {
            item.trace(collector);
        }
    }
}

impl Trace for str {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for String {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for OsStr {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for OsString {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for CStr {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for CString {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for Path {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for PathBuf {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for () {
    fn trace(&self, _collector: &mut Collector) {}
}

impl<A> Trace for (A,)
where
    A: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a,) = self;
        a.trace(collector);
    }
}

impl<A, B> Trace for (A, B)
where
    A: Trace,
    B: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b) = self;
        a.trace(collector);
        b.trace(collector);
    }
}

impl<A, B, C> Trace for (A, B, C)
where
    A: Trace,
    B: Trace,
    C: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
    }
}

impl<A, B, C, D> Trace for (A, B, C, D)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
    }
}

impl<A, B, C, D, E> Trace for (A, B, C, D, E)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace,
    E: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d, e) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
        e.trace(collector);
    }
}

impl<A, B, C, D, E, F> Trace for (A, B, C, D, E, F)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace,
    E: Trace,
    F: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d, e, f) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
        e.trace(collector);
        f.trace(collector);
    }
}

impl<A, B, C, D, E, F, G> Trace for (A, B, C, D, E, F, G)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace,
    E: Trace,
    F: Trace,
    G: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d, e, f, g) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
        e.trace(collector);
        f.trace(collector);
        g.trace(collector);
    }
}

impl<A, B, C, D, E, F, G, H> Trace for (A, B, C, D, E, F, G, H)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace,
    E: Trace,
    F: Trace,
    G: Trace,
    H: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d, e, f, g, h) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
        e.trace(collector);
        f.trace(collector);
        g.trace(collector);
        h.trace(collector);
    }
}

impl<A, B, C, D, E, F, G, H, I> Trace for (A, B, C, D, E, F, G, H, I)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace,
    E: Trace,
    F: Trace,
    G: Trace,
    H: Trace,
    I: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d, e, f, g, h, i) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
        e.trace(collector);
        f.trace(collector);
        g.trace(collector);
        h.trace(collector);
        i.trace(collector);
    }
}

impl<A, B, C, D, E, F, G, H, I, J> Trace for (A, B, C, D, E, F, G, H, I, J)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace,
    E: Trace,
    F: Trace,
    G: Trace,
    H: Trace,
    I: Trace,
    J: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d, e, f, g, h, i, j) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
        e.trace(collector);
        f.trace(collector);
        g.trace(collector);
        h.trace(collector);
        i.trace(collector);
        j.trace(collector);
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K> Trace for (A, B, C, D, E, F, G, H, I, J, K)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace,
    E: Trace,
    F: Trace,
    G: Trace,
    H: Trace,
    I: Trace,
    J: Trace,
    K: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d, e, f, g, h, i, j, k) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
        e.trace(collector);
        f.trace(collector);
        g.trace(collector);
        h.trace(collector);
        i.trace(collector);
        j.trace(collector);
        k.trace(collector);
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K, L> Trace for (A, B, C, D, E, F, G, H, I, J, K, L)
where
    A: Trace,
    B: Trace,
    C: Trace,
    D: Trace,
    E: Trace,
    F: Trace,
    G: Trace,
    H: Trace,
    I: Trace,
    J: Trace,
    K: Trace,
    L: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        let (a, b, c, d, e, f, g, h, i, j, k, l) = self;
        a.trace(collector);
        b.trace(collector);
        c.trace(collector);
        d.trace(collector);
        e.trace(collector);
        f.trace(collector);
        g.trace(collector);
        h.trace(collector);
        i.trace(collector);
        j.trace(collector);
        k.trace(collector);
        l.trace(collector);
    }
}

impl<T> Trace for Cow<'static, T>
where
    T: Trace + ToOwned + ?Sized + 'static,
    <T as ToOwned>::Owned: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        match self {
            Cow::Owned(t) => t.trace(collector),
            Cow::Borrowed(t) => t.trace(collector),
        }
    }
}

impl<T> Trace for Box<T>
where
    T: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        self.as_ref().trace(collector)
    }
}

impl<T> Trace for Reverse<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        self.0.trace(collector)
    }
}

impl Trace for Ordering {
    fn trace(&self, _collector: &mut Collector) {}
}

impl<T> Trace for VecDeque<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for item in self.iter() {
            item.trace(collector)
        }
    }
}

impl<T> Trace for BTreeSet<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for item in self.iter() {
            item.trace(collector)
        }
    }
}

impl<T> Trace for HashSet<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for item in self.iter() {
            item.trace(collector)
        }
    }
}

impl<T> Trace for LinkedList<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for item in self.iter() {
            item.trace(collector)
        }
    }
}

impl<T> Trace for BinaryHeap<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for item in self.iter() {
            item.trace(collector)
        }
    }
}

impl<K, V> Trace for BTreeMap<K, V>
where
    K: Trace,
    V: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for (key, value) in self.iter() {
            key.trace(collector);
            value.trace(collector);
        }
    }
}

impl<K, V> Trace for HashMap<K, V>
where
    K: Trace,
    V: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        for (key, value) in self.iter() {
            key.trace(collector);
            value.trace(collector);
        }
    }
}

impl Trace for Infallible {
    fn trace(&self, _collector: &mut Collector) {
        match *self {}
    }
}

impl Trace for File {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for FileType {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for Metadata {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for IpAddr {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for Ipv4Addr {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for Ipv6Addr {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for SocketAddr {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for SocketAddrV4 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for SocketAddrV6 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for TcpListener {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for TcpStream {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for UdpSocket {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroU8 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroU16 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroU32 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroU64 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroU128 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroI8 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroI16 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroI32 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroI64 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for NonZeroI128 {
    fn trace(&self, _collector: &mut Collector) {}
}

impl<T> Trace for Saturating<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        self.0.trace(collector)
    }
}

impl<T> Trace for Wrapping<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        self.0.trace(collector)
    }
}

impl Trace for FpCategory {
    fn trace(&self, _collector: &mut Collector) {}
}

impl<B, C> Trace for ControlFlow<B, C>
where
    B: Trace,
    C: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        match self {
            ControlFlow::Break(t) => t.trace(collector),
            ControlFlow::Continue(t) => t.trace(collector),
        }
    }
}

impl<T> Trace for Option<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        if let Some(value) = self {
            value.trace(collector)
        }
    }
}

impl<P> Trace for Pin<P>
where
    P: Deref + 'static,
    <P as Deref>::Target: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        self.deref().trace(collector)
    }
}

impl<T> Trace for Rc<T>
where
    T: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        self.as_ref().trace(collector)
    }
}

impl<T> Trace for Weak<T>
where
    T: Trace + ?Sized,
{
    fn trace(&self, collector: &mut Collector) {
        if let Some(ptr) = self.upgrade() {
            ptr.trace(collector)
        }
    }
}

impl<T, E> Trace for Result<T, E>
where
    T: Trace,
    E: Trace,
{
    fn trace(&self, collector: &mut Collector) {
        match self {
            Ok(t) => t.trace(collector),
            Err(t) => t.trace(collector),
        }
    }
}

impl Trace for Duration {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for Instant {
    fn trace(&self, _collector: &mut Collector) {}
}

impl Trace for SystemTime {
    fn trace(&self, _collector: &mut Collector) {}
}

/// Prevent tracing into wrapped value.
///
/// This struct is useful when you want to put into garbage collector a value that *doesn't* implement [`Trace`].
/// While in most situations it is advisable that you provide an implementation
/// sometimes that can be difficult to achieve.
///
/// One example is trait objects: most traits don't have [`Trace`] as supertrait
/// (which is a perfectly understandable sentiment).
/// Something like `Box<dyn Write>` cannot implement [`Trace`] (as `Box` delegates implementation to inner value),
/// so you can wrap it in `Untrace<Box<dyn Write>>` or `Box<Untrace<dyn Write>>`
/// to indicate that there is no need to trace references through this value.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Default, Hash)]
pub struct Untrace<T: ?Sized>(pub T);

impl<T> Untrace<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Trace for Untrace<T>
where
    T: ?Sized + 'static,
{
    fn trace(&self, _collector: &mut Collector) {}
}

impl<T> Deref for Untrace<T>
where
    T: ?Sized,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Untrace<T>
where
    T: ?Sized,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> AsRef<T> for Untrace<T>
where
    T: ?Sized,
{
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> AsMut<T> for Untrace<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }
}
