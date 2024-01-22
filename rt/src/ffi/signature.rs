pub(super) trait SignatureOnce<In> {
    type Output;

    fn call_once(self, _: In) -> Self::Output;
}

pub(super) trait SignatureMut<In>: SignatureOnce<In> {
    fn call_mut(&mut self, _: In) -> Self::Output;
}

pub(super) trait Signature<In>: SignatureMut<In> {
    fn call(&self, _: In) -> Self::Output;
}

impl<F, R> SignatureOnce<()> for F
where
    F: FnOnce() -> R,
{
    type Output = R;

    fn call_once(self, (): ()) -> Self::Output {
        (self)()
    }
}
impl<F, R, A> SignatureOnce<(A,)> for F
where
    F: FnOnce(A) -> R,
{
    type Output = R;

    fn call_once(self, (a,): (A,)) -> Self::Output {
        (self)(a)
    }
}

impl<F, R, A, B> SignatureOnce<(A, B)> for F
where
    F: FnOnce(A, B) -> R,
{
    type Output = R;

    fn call_once(self, (a, b): (A, B)) -> Self::Output {
        (self)(a, b)
    }
}

impl<F, R> SignatureMut<()> for F
where
    F: FnMut() -> R,
{
    fn call_mut(&mut self, (): ()) -> Self::Output {
        (self)()
    }
}

impl<F, R, A> SignatureMut<(A,)> for F
where
    F: FnMut(A) -> R,
{
    fn call_mut(&mut self, (a,): (A,)) -> Self::Output {
        (self)(a)
    }
}

impl<F, R, A, B> SignatureMut<(A, B)> for F
where
    F: FnMut(A, B) -> R,
{
    fn call_mut(&mut self, (a, b): (A, B)) -> Self::Output {
        (self)(a, b)
    }
}

impl<F, R> Signature<()> for F
where
    F: Fn() -> R,
{
    fn call(&self, (): ()) -> Self::Output {
        (self)()
    }
}

impl<F, R, A> Signature<(A,)> for F
where
    F: Fn(A) -> R,
{
    fn call(&self, (a,): (A,)) -> Self::Output {
        (self)(a)
    }
}

impl<F, R, A, B> Signature<(A, B)> for F
where
    F: Fn(A, B) -> R,
{
    fn call(&self, (a, b): (A, B)) -> Self::Output {
        (self)(a, b)
    }
}
