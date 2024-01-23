pub(super) trait Signature<Args> {
    type Output;

    fn call(self, _: Args) -> Self::Output;
}

impl<F, R> Signature<()> for F
where
    F: FnOnce() -> R,
{
    type Output = R;

    fn call(self, (): ()) -> Self::Output {
        (self)()
    }
}
impl<F, R, A> Signature<(A,)> for F
where
    F: FnOnce(A) -> R,
{
    type Output = R;

    fn call(self, (a,): (A,)) -> Self::Output {
        (self)(a)
    }
}

impl<F, R, A, B> Signature<(A, B)> for F
where
    F: FnOnce(A, B) -> R,
{
    type Output = R;

    fn call(self, (a, b): (A, B)) -> Self::Output {
        (self)(a, b)
    }
}

pub(super) trait SignatureWithRt<Rt, Args> {
    type Output;

    fn call(&self, rt: Rt, _: Args) -> Self::Output;
}

impl<F, Rt, R> SignatureWithRt<Rt, ()> for F
where
    F: Fn(Rt) -> R,
{
    type Output = R;

    fn call(&self, rt: Rt, (): ()) -> Self::Output {
        (self)(rt)
    }
}

impl<F, Rt, R, A> SignatureWithRt<Rt, (A,)> for F
where
    F: Fn(Rt, A) -> R,
{
    type Output = R;

    fn call(&self, rt: Rt, (a,): (A,)) -> Self::Output {
        (self)(rt, a)
    }
}

impl<F, Rt, R, A, B> SignatureWithRt<Rt, (A, B)> for F
where
    F: Fn(Rt, A, B) -> R,
{
    type Output = R;

    fn call(&self, rt: Rt, (a, b): (A, B)) -> Self::Output {
        (self)(rt, a, b)
    }
}
