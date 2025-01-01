#![allow(dead_code)]

use gc_derive::Trace;

#[derive(Trace)]
struct Foo1<T>(T)
where
    T: Clone;

#[derive(Trace)]
struct Foo2<A, B>
where
    A: Ord,
    B: Eq,
{
    a: A,
    b: B
}

fn main() {}
