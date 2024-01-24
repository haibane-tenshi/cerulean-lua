//! Traits to help extract function signature information.
//!
//! This module provides [`Signature`] and [`SignatureWithFirst`] traits
//! which allow to acquire both arguments (as a tuple) and return type of a function.
//! We use them to power a layer of automatic conversion to/from Lua ABI
//! for some Rust functions.

/// Helper trait allowing to extract function arguments as a tuple.
///
/// The trait design closely mimics currently unstable [`FnOnce`] trait.
///
/// It is implemented for all functions with up to 12 arguments.
/// The only generic parameter is a tuple of all arguments function receives.
pub trait Signature<Args> {
    type Output;

    fn call(self, args: Args) -> Self::Output;
}

impl<Fun, Ret> Signature<()> for Fun
where
    Fun: FnOnce() -> Ret,
{
    type Output = Ret;

    fn call(self, (): ()) -> Self::Output {
        (self)()
    }
}
impl<Fun, Ret, A> Signature<(A,)> for Fun
where
    Fun: FnOnce(A) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A,)) -> Self::Output {
        let (a,) = args;
        (self)(a)
    }
}

impl<Fun, Ret, A, B> Signature<(A, B)> for Fun
where
    Fun: FnOnce(A, B) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B)) -> Self::Output {
        let (a, b) = args;
        (self)(a, b)
    }
}

impl<Fun, Ret, A, B, C> Signature<(A, B, C)> for Fun
where
    Fun: FnOnce(A, B, C) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C)) -> Self::Output {
        let (a, b, c) = args;
        (self)(a, b, c)
    }
}

impl<Fun, Ret, A, B, C, D> Signature<(A, B, C, D)> for Fun
where
    Fun: FnOnce(A, B, C, D) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D)) -> Self::Output {
        let (a, b, c, d) = args;
        (self)(a, b, c, d)
    }
}

impl<Fun, Ret, A, B, C, D, E> Signature<(A, B, C, D, E)> for Fun
where
    Fun: FnOnce(A, B, C, D, E) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D, E)) -> Self::Output {
        let (a, b, c, d, e) = args;
        (self)(a, b, c, d, e)
    }
}

impl<Fun, Ret, A, B, C, D, E, F> Signature<(A, B, C, D, E, F)> for Fun
where
    Fun: FnOnce(A, B, C, D, E, F) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D, E, F)) -> Self::Output {
        let (a, b, c, d, e, f) = args;
        (self)(a, b, c, d, e, f)
    }
}

impl<Fun, Ret, A, B, C, D, E, F, G> Signature<(A, B, C, D, E, F, G)> for Fun
where
    Fun: FnOnce(A, B, C, D, E, F, G) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D, E, F, G)) -> Self::Output {
        let (a, b, c, d, e, f, g) = args;
        (self)(a, b, c, d, e, f, g)
    }
}

impl<Fun, Ret, A, B, C, D, E, F, G, H> Signature<(A, B, C, D, E, F, G, H)> for Fun
where
    Fun: FnOnce(A, B, C, D, E, F, G, H) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D, E, F, G, H)) -> Self::Output {
        let (a, b, c, d, e, f, g, h) = args;
        (self)(a, b, c, d, e, f, g, h)
    }
}

impl<Fun, Ret, A, B, C, D, E, F, G, H, I> Signature<(A, B, C, D, E, F, G, H, I)> for Fun
where
    Fun: FnOnce(A, B, C, D, E, F, G, H, I) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D, E, F, G, H, I)) -> Self::Output {
        let (a, b, c, d, e, f, g, h, i) = args;
        (self)(a, b, c, d, e, f, g, h, i)
    }
}

impl<Fun, Ret, A, B, C, D, E, F, G, H, I, J> Signature<(A, B, C, D, E, F, G, H, I, J)> for Fun
where
    Fun: FnOnce(A, B, C, D, E, F, G, H, I, J) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D, E, F, G, H, I, J)) -> Self::Output {
        let (a, b, c, d, e, f, g, h, i, j) = args;
        (self)(a, b, c, d, e, f, g, h, i, j)
    }
}

impl<Fun, Ret, A, B, C, D, E, F, G, H, I, J, K> Signature<(A, B, C, D, E, F, G, H, I, J, K)> for Fun
where
    Fun: FnOnce(A, B, C, D, E, F, G, H, I, J, K) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D, E, F, G, H, I, J, K)) -> Self::Output {
        let (a, b, c, d, e, f, g, h, i, j, k) = args;
        (self)(a, b, c, d, e, f, g, h, i, j, k)
    }
}

impl<Fun, Ret, A, B, C, D, E, F, G, H, I, J, K, L> Signature<(A, B, C, D, E, F, G, H, I, J, K, L)>
    for Fun
where
    Fun: FnOnce(A, B, C, D, E, F, G, H, I, J, K, L) -> Ret,
{
    type Output = Ret;

    fn call(self, args: (A, B, C, D, E, F, G, H, I, J, K, L)) -> Self::Output {
        let (a, b, c, d, e, f, g, h, i, j, k, l) = args;
        (self)(a, b, c, d, e, f, g, h, i, j, k, l)
    }
}

/// Helper trait allowing to separately extract first function argument and rest as a tuple.
///
/// The trait design closely mimics currently unstable [`FnOnce`] trait.
///
/// It is implemented for all functions with up to 13 arguments (including first arg).
/// The second generic parameter is a tuple of tailing arguments function receives.
pub trait SignatureWithFirst<First, Args> {
    type Output;

    fn call(self, rt: First, args: Args) -> Self::Output;
}

impl<Fun, First, Ret> SignatureWithFirst<First, ()> for Fun
where
    Fun: FnOnce(First) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, (): ()) -> Self::Output {
        (self)(rt)
    }
}

impl<Fun, First, Ret, A> SignatureWithFirst<First, (A,)> for Fun
where
    Fun: FnOnce(First, A) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A,)) -> Self::Output {
        let (a,) = args;
        (self)(rt, a)
    }
}

impl<Fun, First, Ret, A, B> SignatureWithFirst<First, (A, B)> for Fun
where
    Fun: FnOnce(First, A, B) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B)) -> Self::Output {
        let (a, b) = args;
        (self)(rt, a, b)
    }
}

impl<Fun, First, Ret, A, B, C> SignatureWithFirst<First, (A, B, C)> for Fun
where
    Fun: FnOnce(First, A, B, C) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C)) -> Self::Output {
        let (a, b, c) = args;
        (self)(rt, a, b, c)
    }
}

impl<Fun, First, Ret, A, B, C, D> SignatureWithFirst<First, (A, B, C, D)> for Fun
where
    Fun: FnOnce(First, A, B, C, D) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D)) -> Self::Output {
        let (a, b, c, d) = args;
        (self)(rt, a, b, c, d)
    }
}

impl<Fun, First, Ret, A, B, C, D, E> SignatureWithFirst<First, (A, B, C, D, E)> for Fun
where
    Fun: FnOnce(First, A, B, C, D, E) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D, E)) -> Self::Output {
        let (a, b, c, d, e) = args;
        (self)(rt, a, b, c, d, e)
    }
}

impl<Fun, First, Ret, A, B, C, D, E, F> SignatureWithFirst<First, (A, B, C, D, E, F)> for Fun
where
    Fun: FnOnce(First, A, B, C, D, E, F) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D, E, F)) -> Self::Output {
        let (a, b, c, d, e, f) = args;
        (self)(rt, a, b, c, d, e, f)
    }
}

impl<Fun, First, Ret, A, B, C, D, E, F, G> SignatureWithFirst<First, (A, B, C, D, E, F, G)> for Fun
where
    Fun: FnOnce(First, A, B, C, D, E, F, G) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D, E, F, G)) -> Self::Output {
        let (a, b, c, d, e, f, g) = args;
        (self)(rt, a, b, c, d, e, f, g)
    }
}

impl<Fun, First, Ret, A, B, C, D, E, F, G, H> SignatureWithFirst<First, (A, B, C, D, E, F, G, H)>
    for Fun
where
    Fun: FnOnce(First, A, B, C, D, E, F, G, H) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D, E, F, G, H)) -> Self::Output {
        let (a, b, c, d, e, f, g, h) = args;
        (self)(rt, a, b, c, d, e, f, g, h)
    }
}

impl<Fun, First, Ret, A, B, C, D, E, F, G, H, I>
    SignatureWithFirst<First, (A, B, C, D, E, F, G, H, I)> for Fun
where
    Fun: FnOnce(First, A, B, C, D, E, F, G, H, I) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D, E, F, G, H, I)) -> Self::Output {
        let (a, b, c, d, e, f, g, h, i) = args;
        (self)(rt, a, b, c, d, e, f, g, h, i)
    }
}

impl<Fun, First, Ret, A, B, C, D, E, F, G, H, I, J>
    SignatureWithFirst<First, (A, B, C, D, E, F, G, H, I, J)> for Fun
where
    Fun: FnOnce(First, A, B, C, D, E, F, G, H, I, J) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D, E, F, G, H, I, J)) -> Self::Output {
        let (a, b, c, d, e, f, g, h, i, j) = args;
        (self)(rt, a, b, c, d, e, f, g, h, i, j)
    }
}

impl<Fun, First, Ret, A, B, C, D, E, F, G, H, I, J, K>
    SignatureWithFirst<First, (A, B, C, D, E, F, G, H, I, J, K)> for Fun
where
    Fun: FnOnce(First, A, B, C, D, E, F, G, H, I, J, K) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D, E, F, G, H, I, J, K)) -> Self::Output {
        let (a, b, c, d, e, f, g, h, i, j, k) = args;
        (self)(rt, a, b, c, d, e, f, g, h, i, j, k)
    }
}

impl<Fun, First, Ret, A, B, C, D, E, F, G, H, I, J, K, L>
    SignatureWithFirst<First, (A, B, C, D, E, F, G, H, I, J, K, L)> for Fun
where
    Fun: FnOnce(First, A, B, C, D, E, F, G, H, I, J, K, L) -> Ret,
{
    type Output = Ret;

    fn call(self, rt: First, args: (A, B, C, D, E, F, G, H, I, J, K, L)) -> Self::Output {
        let (a, b, c, d, e, f, g, h, i, j, k, l) = args;
        (self)(rt, a, b, c, d, e, f, g, h, i, j, k, l)
    }
}
