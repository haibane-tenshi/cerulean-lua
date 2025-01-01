#![allow(dead_code)]

use gc_derive::Trace;

#[derive(Trace)]
struct Foo0 {}

#[derive(Trace)]
struct Foo1 {
    a: u32
}

#[derive(Trace)]
struct Foo2 {
    a: u32,
    b: u32,
}

#[derive(Trace)]
struct FooCollector {
    collector: u32,
    _collector: u32,
}

fn main() {}
