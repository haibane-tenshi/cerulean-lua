#![allow(dead_code)]

use gc_derive::Trace;

#[derive(Trace)]
struct Foo1<T: Clone>(T);

#[derive(Trace)]
struct Foo2<A, B: Eq> {
    a: A,
    b: B
}

fn main() {}
