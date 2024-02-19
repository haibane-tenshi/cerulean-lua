pub mod arg_parser;
pub mod signature;
pub mod tuple;

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::path::Path;

use gc::{Heap, Trace};

use crate::chunk_cache::ChunkId;
use crate::error::RuntimeError;
use crate::gc::StringRef;
use crate::runtime::{RuntimeView, StackView};
use crate::value::{CoreTypes, NilOr, RootValue, Strong, Value};

use arg_parser::{FormatReturns, ParseArgs};
use signature::{Signature, SignatureWithFirst};
use tuple::Tuple;

pub trait LuaFfiOnce<Ty>
where
    Ty: CoreTypes,
{
    fn call_once(self, _: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>>;
    fn debug_info(&self) -> DebugInfo;
}

pub trait LuaFfiMut<Ty>: LuaFfiOnce<Ty>
where
    Ty: CoreTypes,
{
    fn call_mut(&mut self, _: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>>;
}

pub trait LuaFfi<Ty>: LuaFfiMut<Ty>
where
    Ty: CoreTypes,
{
    fn call(&self, _: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>>;
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct DebugInfo {
    pub name: String,
}

impl<Ty, F> LuaFfiOnce<Ty> for F
where
    Ty: CoreTypes,
    F: for<'rt> FnOnce(RuntimeView<'rt, Ty>) -> Result<(), RuntimeError<Ty>>,
{
    fn call_once(self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>> {
        (self)(rt)
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: std::any::type_name::<F>().to_string(),
        }
    }
}

impl<Ty, F> LuaFfiMut<Ty> for F
where
    Ty: CoreTypes,
    F: for<'rt> FnMut(RuntimeView<'rt, Ty>) -> Result<(), RuntimeError<Ty>>,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>> {
        (self)(rt)
    }
}

impl<Ty, F> LuaFfi<Ty> for F
where
    Ty: CoreTypes,
    F: for<'rt> Fn(RuntimeView<'rt, Ty>) -> Result<(), RuntimeError<Ty>>,
{
    fn call(&self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>> {
        (self)(rt)
    }
}

pub fn invoke<'rt, Ty, F, Args>(mut rt: RuntimeView<'rt, Ty>, f: F) -> Result<(), RuntimeError<Ty>>
where
    Ty: CoreTypes,
    F: Signature<Args>,
    for<'a> &'a [RootValue<Ty>]: ParseArgs<Args, Heap>,
    for<'a> StackView<'a, RootValue<Ty>>:
        FormatReturns<Strong<Ty>, Heap, <F as Signature<Args>>::Output>,
{
    let heap = &mut rt.core.gc;

    let args = rt.stack.raw.parse(heap).map_err(|err| {
        let msg = StringRef::new(err.to_string().into());
        Value::String(msg)
    })?;

    rt.stack.clear();

    let ret = f.call(args);

    rt.stack.format(heap, ret);

    Ok(())
}

pub fn try_invoke<'rt, Ty, F, Args, R>(
    mut rt: RuntimeView<'rt, Ty>,
    f: F,
) -> Result<(), RuntimeError<Ty>>
where
    Ty: CoreTypes,
    F: Signature<Args, Output = Result<R, RuntimeError<Ty>>>,
    for<'a> &'a [RootValue<Ty>]: ParseArgs<Args, Heap>,
    for<'a> StackView<'a, RootValue<Ty>>: FormatReturns<Strong<Ty>, Heap, R>,
{
    let heap = &mut rt.core.gc;

    let args = rt.stack.raw.parse(heap).map_err(|err| {
        let msg = StringRef::new(err.to_string().into());
        Value::String(msg)
    })?;

    rt.stack.clear();

    let ret = f.call(args)?;

    rt.stack.format(heap, ret);

    Ok(())
}

pub fn invoke_with_rt<'rt, Ty, F, Args, R>(
    mut rt: RuntimeView<'rt, Ty>,
    f: F,
) -> Result<(), RuntimeError<Ty>>
where
    Ty: CoreTypes,
    // Value<Ty>: Display + Debug,
    for<'a> F: SignatureWithFirst<RuntimeView<'a, Ty>, Args, Output = R>,
    for<'a> &'a [RootValue<Ty>]: ParseArgs<Args, Heap>,
    for<'a> StackView<'a, RootValue<Ty>>: FormatReturns<
        Strong<Ty>,
        Heap,
        <F as SignatureWithFirst<RuntimeView<'a, Ty>, Args>>::Output,
    >,
{
    let heap = &mut rt.core.gc;

    let args = rt.stack.raw.parse(heap).map_err(|err| {
        let msg = StringRef::new(err.to_string().into());
        Value::String(msg)
    })?;

    rt.stack.clear();

    let ret = f.call(rt.view_full(), args);

    rt.stack.format(&mut rt.core.gc, ret);

    Ok(())
}

pub fn try_invoke_with_rt<'rt, Ty, F, Args, R>(
    mut rt: RuntimeView<'rt, Ty>,
    f: F,
) -> Result<(), RuntimeError<Ty>>
where
    Ty: CoreTypes,
    // Value<Ty>: Debug + Display,
    for<'a> F: SignatureWithFirst<RuntimeView<'a, Ty>, Args, Output = Result<R, RuntimeError<Ty>>>,
    for<'a> &'a [RootValue<Ty>]: ParseArgs<Args, Heap>,
    for<'a> StackView<'a, RootValue<Ty>>: FormatReturns<Strong<Ty>, Heap, R>,
{
    let heap = &mut rt.core.gc;

    let args = rt.stack.raw.parse(heap).map_err(|err| {
        let msg = StringRef::new(err.to_string().into());
        Value::String(msg)
    })?;

    rt.stack.clear();

    let ret = f.call(rt.view_full(), args)?;

    rt.stack.format(&mut rt.core.gc, ret);

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Maybe<T> {
    #[default]
    None,
    Some(T),
}

impl<T> Maybe<T> {
    pub fn into_option(self) -> Option<T> {
        self.into()
    }

    pub fn take(&mut self) -> Self {
        std::mem::take(self)
    }
}

impl<T> Maybe<NilOr<T>> {
    pub fn flatten_into_option(self) -> Option<T> {
        self.into_option().and_then(NilOr::into_option)
    }
}

impl<T> From<Option<T>> for Maybe<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(t) => Maybe::Some(t),
            None => Maybe::None,
        }
    }
}

impl<T> From<Maybe<T>> for Option<T> {
    fn from(value: Maybe<T>) -> Self {
        match value {
            Maybe::None => None,
            Maybe::Some(t) => Some(t),
        }
    }
}

pub trait WithName<Gc>: Sized {
    fn with_name<N>(self, name: N) -> DebugInfoWrap<Self, N>;
}

impl<F, Ty> WithName<Ty> for F
where
    Ty: CoreTypes,
    F: LuaFfiOnce<Ty>,
{
    fn with_name<N>(self, name: N) -> DebugInfoWrap<Self, N> {
        DebugInfoWrap { func: self, name }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DebugInfoWrap<F, N> {
    func: F,
    name: N,
}

impl<Ty, F, N> LuaFfiOnce<Ty> for DebugInfoWrap<F, N>
where
    Ty: CoreTypes,
    F: LuaFfiOnce<Ty>,
    N: AsRef<str>,
{
    fn call_once(self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>> {
        self.func.call_once(rt)
    }

    fn debug_info(&self) -> DebugInfo {
        DebugInfo {
            name: self.name.as_ref().to_string(),
        }
    }
}

impl<Ty, F, N> LuaFfiMut<Ty> for DebugInfoWrap<F, N>
where
    Ty: CoreTypes,
    F: LuaFfiMut<Ty>,
    N: AsRef<str>,
{
    fn call_mut(&mut self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>> {
        self.func.call_mut(rt)
    }
}

impl<Ty, F, N> LuaFfi<Ty> for DebugInfoWrap<F, N>
where
    Ty: CoreTypes,
    F: LuaFfi<Ty>,
    N: AsRef<str>,
{
    fn call(&self, rt: RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>> {
        self.func.call(rt)
    }
}

pub type LuaFfiFnPtr<Ty> =
    DebugInfoWrap<fn(RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>>, &'static str>;

impl<Ty> LuaFfiFnPtr<Ty>
where
    Ty: CoreTypes,
{
    pub fn new(
        fn_ptr: fn(RuntimeView<'_, Ty>) -> Result<(), RuntimeError<Ty>>,
        name: &'static str,
    ) -> Self {
        LuaFfiFnPtr { func: fn_ptr, name }
    }
}

impl<Ty> Trace for LuaFfiFnPtr<Ty>
where
    Ty: CoreTypes + 'static,
{
    fn trace(&self, _collector: &mut gc::Collector) {}
}

pub fn call_chunk<Ty>(chunk_id: ChunkId) -> impl LuaFfi<Ty> + Copy + Send + Sync
where
    Ty: CoreTypes,
    Ty::RustClosure: LuaFfi<Ty>,
    RootValue<Ty>: Display,
{
    let f = move |mut rt: RuntimeView<'_, Ty>| {
        use crate::runtime::FunctionPtr;
        use repr::index::{FunctionId, StackSlot};

        let ptr = FunctionPtr {
            chunk_id,
            function_id: FunctionId(0),
        };

        let closure = rt.construct_closure(ptr, [rt.core.global_env.clone()])?;
        let closure = rt.core.gc.alloc(closure);

        rt.enter(closure, StackSlot(0))
    };

    f.with_name("rt::ffi::call_chunk")
}

pub fn call_file<Ty>(script: impl AsRef<Path>) -> impl LuaFfi<Ty>
where
    Ty: CoreTypes,
    Ty::RustClosure: LuaFfi<Ty>,
    RootValue<Ty>: Display,
{
    let f = move |mut rt: RuntimeView<Ty>| {
        let script = script.as_ref();
        let chunk_id = rt.load_from_file(script)?;

        rt.invoke(call_chunk(chunk_id))
    };

    f.with_name("rt::ffi::call_file")
}

/// Split [`Opts<T>`] into tuple of options.
///
/// Trait provides a convenience method to convert Lua optional arguments
/// `Opts<(T0, T1,...)>` into more convenient Rustic form of
/// `(Option<T0>, Option<T1>,...)`.
pub trait Split: sealed::Sealed {
    type Output;

    /// Convert [`Opts<(T0, T1,...)>`](Opts) into `(Option<T0>, Option<T1>,...)`.
    fn split(self) -> Self::Output;
}

impl Split for Opts<()> {
    type Output = ();

    fn split(self) -> Self::Output {}
}

impl<A> Split for Maybe<NilOr<A>> {
    type Output = (Option<A>,);

    fn split(self) -> Self::Output {
        (self.flatten_into_option(),)
    }
}

impl<A, B> Split for Maybe<(NilOr<A>, Maybe<NilOr<B>>)> {
    type Output = (Option<A>, Option<B>);

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b,) = rest.split();
                (a.into_option(), b)
            }
        }
    }
}

impl<A, B, C> Split for Maybe<(NilOr<A>, Maybe<(NilOr<B>, Maybe<NilOr<C>>)>)> {
    type Output = (Option<A>, Option<B>, Option<C>);

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c) = rest.split();
                (a.into_option(), b, c)
            }
        }
    }
}

impl<A, B, C, D> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(NilOr<B>, Maybe<(NilOr<C>, Maybe<NilOr<D>>)>)>,
    )>
{
    type Output = (Option<A>, Option<B>, Option<C>, Option<D>);

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d) = rest.split();
                (a.into_option(), b, c, d)
            }
        }
    }
}

impl<A, B, C, D, E> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(NilOr<C>, Maybe<(NilOr<D>, Maybe<NilOr<E>>)>)>,
        )>,
    )>
{
    type Output = (Option<A>, Option<B>, Option<C>, Option<D>, Option<E>);

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e) = rest.split();
                (a.into_option(), b, c, d, e)
            }
        }
    }
}

impl<A, B, C, D, E, F> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(NilOr<D>, Maybe<(NilOr<E>, Maybe<NilOr<F>>)>)>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f) = rest.split();
                (a.into_option(), b, c, d, e, f)
            }
        }
    }
}

impl<A, B, C, D, E, F, G> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(NilOr<E>, Maybe<(NilOr<F>, Maybe<NilOr<G>>)>)>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g) = rest.split();
                (a.into_option(), b, c, d, e, f, g)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(NilOr<F>, Maybe<(NilOr<G>, Maybe<NilOr<H>>)>)>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H, I> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(
                            NilOr<F>,
                            Maybe<(NilOr<G>, Maybe<(NilOr<H>, Maybe<NilOr<I>>)>)>,
                        )>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
        Option<I>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h, i) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h, i)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H, I, J> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(
                            NilOr<F>,
                            Maybe<(
                                NilOr<G>,
                                Maybe<(NilOr<H>, Maybe<(NilOr<I>, Maybe<NilOr<J>>)>)>,
                            )>,
                        )>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
        Option<I>,
        Option<J>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h, i, j) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h, i, j)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(
                            NilOr<F>,
                            Maybe<(
                                NilOr<G>,
                                Maybe<(
                                    NilOr<H>,
                                    Maybe<(NilOr<I>, Maybe<(NilOr<J>, Maybe<NilOr<K>>)>)>,
                                )>,
                            )>,
                        )>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
        Option<I>,
        Option<J>,
        Option<K>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h, i, j, k) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h, i, j, k)
            }
        }
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K, L> Split
    for Maybe<(
        NilOr<A>,
        Maybe<(
            NilOr<B>,
            Maybe<(
                NilOr<C>,
                Maybe<(
                    NilOr<D>,
                    Maybe<(
                        NilOr<E>,
                        Maybe<(
                            NilOr<F>,
                            Maybe<(
                                NilOr<G>,
                                Maybe<(
                                    NilOr<H>,
                                    Maybe<(
                                        NilOr<I>,
                                        Maybe<(NilOr<J>, Maybe<(NilOr<K>, Maybe<NilOr<L>>)>)>,
                                    )>,
                                )>,
                            )>,
                        )>,
                    )>,
                )>,
            )>,
        )>,
    )>
{
    type Output = (
        Option<A>,
        Option<B>,
        Option<C>,
        Option<D>,
        Option<E>,
        Option<F>,
        Option<G>,
        Option<H>,
        Option<I>,
        Option<J>,
        Option<K>,
        Option<L>,
    );

    fn split(self) -> Self::Output {
        match self {
            Maybe::None => Default::default(),
            Maybe::Some((a, rest)) => {
                let (b, c, d, e, f, g, h, i, j, k, l) = rest.split();
                (a.into_option(), b, c, d, e, f, g, h, i, j, k, l)
            }
        }
    }
}

mod sealed {
    use super::{Maybe, NilOr};

    pub trait Sealed {}

    impl Sealed for () {}

    impl<T> Sealed for Maybe<NilOr<T>> {}

    impl<T, U> Sealed for Maybe<(NilOr<T>, U)> where U: Sealed {}
}

/// Expand tuple into optional Lua arguments.
///
/// This typedef provides a shorthand for constructing
/// [correct parser](arg_parser#handling-optional-arguments) for a sequence of optional Lua arguments.
///
/// For example, the following expands into
/// * `Opts<()>` -> `()`
/// * `Opts<(A,)>` -> `Maybe<NilOr<A>>`
/// * `Opts<(A, B)>` -> `Maybe<(NilOr<A>, Maybe<NilOr<B>>)>`
/// * `Opts<(A, B, C)>` -> `Maybe<(NilOr<A>, Maybe<(NilOr<B>, Maybe<NilOr<C>>)>)>`
/// * etc.
///
/// See [`Split`] trait to quickly expand those into tuple of `Option`s.
pub type Opts<T> = <T as OptsImpl>::Output;

#[doc(hidden)]
pub trait OptsImpl: Tuple {
    type Output;
}

impl OptsImpl for () {
    type Output = ();
}

impl<A> OptsImpl for (A,) {
    type Output = Maybe<NilOr<A>>;
}

impl<A, B> OptsImpl for (A, B) {
    type Output = Maybe<(NilOr<A>, Opts<(B,)>)>;
}

impl<A, B, C> OptsImpl for (A, B, C) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C)>)>;
}

impl<A, B, C, D> OptsImpl for (A, B, C, D) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D)>)>;
}

impl<A, B, C, D, E> OptsImpl for (A, B, C, D, E) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E)>)>;
}

impl<A, B, C, D, E, F> OptsImpl for (A, B, C, D, E, F) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F)>)>;
}

impl<A, B, C, D, E, F, G> OptsImpl for (A, B, C, D, E, F, G) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G)>)>;
}

impl<A, B, C, D, E, F, G, H> OptsImpl for (A, B, C, D, E, F, G, H) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H)>)>;
}

impl<A, B, C, D, E, F, G, H, I> OptsImpl for (A, B, C, D, E, F, G, H, I) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H, I)>)>;
}

impl<A, B, C, D, E, F, G, H, I, J> OptsImpl for (A, B, C, D, E, F, G, H, I, J) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H, I, J)>)>;
}

impl<A, B, C, D, E, F, G, H, I, J, K> OptsImpl for (A, B, C, D, E, F, G, H, I, J, K) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H, I, J, K)>)>;
}

impl<A, B, C, D, E, F, G, H, I, J, K, L> OptsImpl for (A, B, C, D, E, F, G, H, I, J, K, L) {
    type Output = Maybe<(NilOr<A>, Opts<(B, C, D, E, F, G, H, I, J, K, L)>)>;
}
