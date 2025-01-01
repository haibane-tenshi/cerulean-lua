#![allow(dead_code)]

use gc_derive::Trace;

#[derive(Trace)]
struct Foo<const SIZE: u32>;

fn main() {}
