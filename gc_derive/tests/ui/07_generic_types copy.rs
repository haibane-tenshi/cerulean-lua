#![allow(dead_code)]

use gc_derive::Trace;

#[derive(Trace)]
struct Foo1<T>(T);

#[derive(Trace)]
struct Foo2<A, B> {
    a: A,
    b: B
}

fn main() {}
