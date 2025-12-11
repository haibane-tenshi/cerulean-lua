#![allow(dead_code)]

use gc_derive::Trace;

mod gc {}

#[derive(Trace)]
struct Foo;

fn main() {}
