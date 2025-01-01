#![allow(dead_code)]

use gc_derive::Trace;

#[derive(Trace)]
enum Foo1 {
    A
}

#[derive(Trace)]
enum Foo4 {
    A,
    B0(),
    B1(u32,),
    B2(u32, u32),
    C0 {},
    C1 {a: u32},
    C2 {a: u32, b: u32,},
    D {collector: u32, _collector: u32},
}

fn main() {}
