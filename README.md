A fully featured pure-Rust configurable Lua interpreter.

# Goal of the project

Originally, this project was started with educational purpose.
My aim was to create *a functional Lua interpreter* as well as explore and learn various ideas related to this space.

Some of the additional optional goals are:
* Modularity, allowing custom combinations of existing components.
* Configurability, specifically controlling questionable or dangerous Lua behavior.

Explicitly, the following are **non-goals**:

* Be *🚀 blazing 🚀 fast 🚀*. 
  No it won't be just because it is written in Rust.
  
* Be fully compliant to Lua spec.

  In this regard Lua suffers both from over- and underspecification, especially in `std`.
  There are plenty of places where behaviours are unspecified, 
  even though it is clear that many users will assume certain constraints;
  and there are also plenty of places where details pertaining to their specific implementation are leaked and exist as API guarantees.
  This makes it difficult to replicate *exact* semantics of the language.
  
  Now, it doesn't mean this project intentionally seeks breakage.
  It will try its best to stay compatible, however it reserves the right to diverge from the spec if deemed necessary.
  
  You can find points of (intentional) incompatibility in dedicated section.
  
* Be a drop-in replacement to vanilla Lua CLI.

* Be a production-ready.
  This is a side project which is updated on basis of desire and free time and likely to stay as such.

Still if this doesn't discourage you, feel free to try it (and report the bugs).

# State of the project: WIP(alpha)

**W**ork **I**n **P**rogress is exactly that - you should consider it completely unstable.
This includes public APIs and project structure (crates).

At the moment there are major refactors planned, so be warned.

Unfortunately due to an oversight, all crates are currently versioned as `0.1` (thanks to default Cargo configuration), although it is more correct to consider them `0.0.1`.
Actual `0.1` version will come into effect on specific commit/tag pair (which is yet to happen).

As of progress towards the goal, `main` branch should contain a functional interpreter capable of running Lua scripts, with many features already implemented (see below).
It should be able to run simple scripts.

# Features

This project targets Lua 5.4.

On interpreter side:

* **F**oreign **F**unction **I**nterface to support calling Rust (as host language) functions
    from inside Lua.
    * Rust functions of appropriate shape automatically implement FFI traits,
        although it requires extra packaging to convert them into Lua-usable values.
    * There is an ability to autogenerate conversion layer between Lua ABI and
        more convenient rusty arguments/returns in most cases.

* Incredibly painful but nevertheless present userdata support.
    
    Unfortunately the *painful* part is due to Rust type system limitations
    and workarounds it requires.
    
    * The setup is modular and offers something akin to compile-time mixins.
        You only need to implement method dispatcher once per type or trait.
    * Later a selection of dispatchers can be provided when constructing userdata.
        This part of the process cannot be fully automated and 
        will requires handwriting some boileplate.
    * On the bright side the mechanism behind method dispatchers is very generic.
        While its primary purpose is to faithfully expose Rust APIs to Lua
        it isn't limited to that.
        
        In particular you can expose function with any name and assign an arbitrary Rust-side implementation to it.
        This allows to construct new APIs that might be a bit more Lua-friendly.
        
* Garbage collection is implemented.
* Threads are WIP.
* Full implementation of syntactic test suite.
* Partial implementation of sematic test suite.

On language side:

* Support for all built-in types except
  * `thread`, blocked on interpreter.
  * light `userdata`, implementable but low priority.
* All operators are supported.
* Type coercions are supported and configurable.
* All control flow constructs are supported.
* Closures and closure captures (upvalues) are supported.
* Metatables and metamethod resolution is supported.
* Local declarations (`const`, `close`) are unsupported.

`std` is partially implemented:
* Following APIs are fully present: `os`, `math`, `table`, `utf8`.
* Base APIs are all present and implemented, except
  * `xpcall` defers to `pcall`, blocked on handler support.
* String APIs are implementable, but missing due to regexes, matches and such.
* Coroutine APIs are missing, blocked on interpreter.
* Module APIs are WIP.
* IO is implementable but partially blocked on particulars of userdata implementation.
* Debug is in purgatory, low priority.

On documentation side:

* Much of documentation is sorely missing.
  This is in part due to many ongoing API changes and rewrites.
* `std` implementation should be reasonably (although not comprehensively) documented, including implementation-specific behavior.

# Known incompatibilities and divergent behavior

## Runtime implementation details

TODO

## Intentional breakage

There are some Lua std APIs which have intentionally divergent behavior.

Lua standard library forwards a good number of functions directly from C standard library.
And if C std is known for something than it is certainly not for being consistent, unsurprising and well-designed.
Unfortunately, Lua inherits many of those shortcomings.

Now, considering that this implementation is written in Rust it gives us a perfect opportunity to fix some of those issues.
As a bonus it lets us surprise users with quirky behavior in new and exciting ways.

Known affected APIs:
* `os::setlocale`
* Utf-8 library function do not support `lax` mode.

## String encodings

Lua claims to be encoding-agnostic, more specifically every Lua string is just a sequence of bytes.
The runtime itself upholds this promise, there are no additional requirements imposed on strings.
However this claim is more of a curse in disguise.

Consider the following situation:
you attempt to open a file in a platform-independent way, so you pass a string with file name to OS API.
In order to make that happen the implementation needs to convert the file name
which is in some unknown beforehand opaque encoding into another opaque encoding that OS expects.
It is completely puzzling as to what is expected happen here.
Vanilla Lua implementation does the only reasonable thing: it simply shoves the bytes down the OS call and prays that it works.

Concerningly enough in our utf8-centric world it *does* end up working more often than it should.
However, as soon as you try to run it using any remotely different configuration (either on OS or Lua side) you end up swamped with errors (in best case!) or silently faulty behavior.

Now, in principle, this is Lua programmer's fault.
They should have ensured to sanitize and properly reencode string inputs/outputs.

Since Rust's strings operate on utf-8, this projects opts for slightly stricter rules:

*   Original Lua spec expects programs to use ASCII encoding.
    This implementation extends inputs to utf8 (using Rust's `&str`).
    
    This doesn't extend lexical conventions, so *identifiers* still may contain only ASCII characters.
    However, this permits string literals to embed utf8-encoded text directly in the source.

*   Short string literals containing certain escape sequences are considered invalid.

    As mentioned in the previous point, all string literals are inherently valid utf8.
    However, after unescaping they should become Lua strings which don't have fixed encoding.
    
    Currently, implementation recognizes two kinds of short string literals:
    
    *   *Utf8 strings*, which must resolve into valid utf8 sequence after unescaping.
        In particular
        
        * unicode escapes (`\u{FFFF}`) must denote a valid codepoint, which is divergent from Lua spec
        * byte escapes (hexadecimal `\xFF` or decimal `\255`) can still be embedded but must form a valid utf8 sequence
    
    *   *Binary strings*, which must contain only byte escapes (no other character or escape sequences are allowed).
        
        Binary strings are resolved into binary payload that will contain specified bytes.
    
    (This may change in the future.)
    
* `std` implementation assumes utf-8 strings for anything that can be passed to OS such as paths.
  This is an easy decision, since from Rust side this is the only way to do so in cross-platform way.
  This generally removes the need to reencode strings on Lua side.

# Licence

This project is distributed under MIT licence.
