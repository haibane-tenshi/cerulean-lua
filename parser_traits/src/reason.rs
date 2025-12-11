pub trait Compress<Rhs> {
    type Output;

    fn compress(self, rhs: Rhs) -> Self::Output;
}

pub type Compressed<Lhs, Rhs> = <Lhs as Compress<Rhs>>::Output;

#[derive(Debug, Clone, Copy)]
pub enum Never {}

impl<T> Compress<T> for Never {
    type Output = Never;

    fn compress(self, _: T) -> Self::Output {
        match self {}
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Complete;

impl<T> Compress<T> for Complete {
    type Output = T;

    fn compress(self, rhs: T) -> Self::Output {
        rhs
    }
}
