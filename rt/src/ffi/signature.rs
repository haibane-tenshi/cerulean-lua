pub(super) trait Signature<In> {
    type Output;

    fn call(self, _: In) -> Self::Output;
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
