#![allow(dead_code)]

use gc_derive::Trace;

#[derive(Trace)]
struct Foo1<T: Clone>(T)
where
    T: Eq;

fn main() {}
