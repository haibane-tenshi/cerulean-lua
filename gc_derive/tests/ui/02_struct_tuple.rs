#![allow(dead_code)]

use gc_derive::Trace;

#[derive(Trace)]
struct Foo0();

#[derive(Trace)]
struct Foo1(u32);

#[derive(Trace)]
struct Foo2(u32, u32);

#[derive(Trace)]
struct Foo6(u32, u32, u32, u32, u32, u32);

#[derive(Trace)]
struct FooTrailing(u32,);

fn main() {}
