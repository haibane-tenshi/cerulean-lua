pub type TupleHead<T> = <T as NonEmptyTuple>::Head;
pub type TupleTail<T> = <T as NonEmptyTuple>::Tail;

pub trait Tuple: sealed::Sealed {}

impl Tuple for () {}

impl<T> Tuple for T where T: NonEmptyTuple {}

pub trait NonEmptyTuple: sealed::Sealed {
    type Head;
    type Tail: Tuple;
}

impl<A> NonEmptyTuple for (A,) {
    type Head = A;
    type Tail = ();
}

impl<A, B> NonEmptyTuple for (A, B) {
    type Head = A;
    type Tail = (B,);
}

impl<A, B, C> NonEmptyTuple for (A, B, C) {
    type Head = A;
    type Tail = (B, C);
}

impl<A, B, C, D> NonEmptyTuple for (A, B, C, D) {
    type Head = A;
    type Tail = (B, C, D);
}

impl<A, B, C, D, E> NonEmptyTuple for (A, B, C, D, E) {
    type Head = A;
    type Tail = (B, C, D, E);
}

impl<A, B, C, D, E, F> NonEmptyTuple for (A, B, C, D, E, F) {
    type Head = A;
    type Tail = (B, C, D, E, F);
}

impl<A, B, C, D, E, F, G> NonEmptyTuple for (A, B, C, D, E, F, G) {
    type Head = A;
    type Tail = (B, C, D, E, F, G);
}

impl<A, B, C, D, E, F, G, H> NonEmptyTuple for (A, B, C, D, E, F, G, H) {
    type Head = A;
    type Tail = (B, C, D, E, F, G, H);
}

impl<A, B, C, D, E, F, G, H, I> NonEmptyTuple for (A, B, C, D, E, F, G, H, I) {
    type Head = A;
    type Tail = (B, C, D, E, F, G, H, I);
}

impl<A, B, C, D, E, F, G, H, I, J> NonEmptyTuple for (A, B, C, D, E, F, G, H, I, J) {
    type Head = A;
    type Tail = (B, C, D, E, F, G, H, I, J);
}

impl<A, B, C, D, E, F, G, H, I, J, K> NonEmptyTuple for (A, B, C, D, E, F, G, H, I, J, K) {
    type Head = A;
    type Tail = (B, C, D, E, F, G, H, I, J, K);
}

impl<A, B, C, D, E, F, G, H, I, J, K, L> NonEmptyTuple for (A, B, C, D, E, F, G, H, I, J, K, L) {
    type Head = A;
    type Tail = (B, C, D, E, F, G, H, I, J, K, L);
}

mod sealed {
    pub trait Sealed {}

    impl Sealed for () {}

    impl<A> Sealed for (A,) {}

    impl<A, B> Sealed for (A, B) {}

    impl<A, B, C> Sealed for (A, B, C) {}

    impl<A, B, C, D> Sealed for (A, B, C, D) {}

    impl<A, B, C, D, E> Sealed for (A, B, C, D, E) {}

    impl<A, B, C, D, E, F> Sealed for (A, B, C, D, E, F) {}

    impl<A, B, C, D, E, F, G> Sealed for (A, B, C, D, E, F, G) {}

    impl<A, B, C, D, E, F, G, H> Sealed for (A, B, C, D, E, F, G, H) {}

    impl<A, B, C, D, E, F, G, H, I> Sealed for (A, B, C, D, E, F, G, H, I) {}

    impl<A, B, C, D, E, F, G, H, I, J> Sealed for (A, B, C, D, E, F, G, H, I, J) {}

    impl<A, B, C, D, E, F, G, H, I, J, K> Sealed for (A, B, C, D, E, F, G, H, I, J, K) {}

    impl<A, B, C, D, E, F, G, H, I, J, K, L> Sealed for (A, B, C, D, E, F, G, H, I, J, K, L) {}
}
