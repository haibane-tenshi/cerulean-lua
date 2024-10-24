mod closure;
mod dialect;
mod interner;
mod orchestrator;
mod thread;

use std::fmt::Debug;
use std::path::Path;

use enumoid::EnumMap;
use gc::{Root, RootCell};
use repr::literal::Literal;

use crate::backtrace::Location;
use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::diagnostic::Diagnostic;
use crate::error::{AlreadyDroppedError, RtError, RuntimeError};
use crate::ffi::DLuaFfi;
use crate::gc::Heap;
use crate::value::{
    Callable, CoreTypes, KeyValue, Meta, Strong, StrongValue, TypeWithoutMetatable, Value, Weak,
    WeakValue,
};
use orchestrator::Orchestrator;
use thread::frame::Event;

pub use closure::{Closure, FunctionPtr};
pub use dialect::{CoerceArgs, DialectBuilder};
pub use interner::{Interned, Interner};
pub use orchestrator::ThreadId;
pub use thread::stack::{StackFrame, StackGuard, TransientStackFrame};

pub struct Core<Ty>
where
    Ty: CoreTypes,
{
    pub global_env: StrongValue<Ty>,
    pub primitive_metatables: EnumMap<TypeWithoutMetatable, Option<RootCell<Ty::Table>>>,
    pub dialect: DialectBuilder,
    pub gc: Heap<Ty>,
    pub string_interner: Interner<Ty::String>,
}

impl<Ty> Core<Ty>
where
    Ty: CoreTypes,
{
    pub fn alloc_string(&mut self, s: Ty::String) -> Root<Interned<Ty::String>> {
        self.string_interner.insert(s, &mut self.gc)
    }

    pub fn alloc_literal(&mut self, literal: Literal) -> StrongValue<Ty> {
        match literal {
            Literal::Nil => Value::Nil,
            Literal::Bool(t) => Value::Bool(t),
            Literal::Int(t) => Value::Int(t),
            Literal::Float(t) => Value::Float(t),
            Literal::String(s) => {
                use crate::gc::LuaPtr;

                let s = self.alloc_string(s.into());
                Value::String(LuaPtr(s))
            }
        }
    }

    pub fn alloc_error_msg(&mut self, msg: impl Into<Ty::String>) -> RtError<Ty> {
        use crate::error::ValueError;
        use crate::gc::LuaPtr;

        let msg = self.alloc_string(msg.into());
        ValueError(Value::String(LuaPtr(msg))).into()
    }
}

impl<Ty> Core<Ty>
where
    Ty: CoreTypes,
{
    fn lookup_event(&self, event: Event) -> KeyValue<Weak, Ty> {
        use crate::gc::LuaPtr;

        let s = self.string_interner.event(event.to_metamethod());
        KeyValue::String(LuaPtr(s))
    }
}

impl<Ty> Core<Ty>
where
    Ty: CoreTypes,
{
    pub fn metatable_of(
        &self,
        value: &WeakValue<Ty>,
    ) -> Result<Option<Meta<Ty>>, AlreadyDroppedError> {
        use crate::gc::LuaPtr;
        use crate::value::Metatable;
        use TypeWithoutMetatable::*;

        let Core {
            primitive_metatables,
            gc: heap,
            ..
        } = self;

        let r = match value {
            Value::Nil => primitive_metatables[Nil].as_ref().map(|t| t.downgrade()),
            Value::Bool(_) => primitive_metatables[Bool].as_ref().map(|t| t.downgrade()),
            Value::Int(_) => primitive_metatables[Int].as_ref().map(|t| t.downgrade()),
            Value::Float(_) => primitive_metatables[Float].as_ref().map(|t| t.downgrade()),
            Value::String(_) => primitive_metatables[String].as_ref().map(|t| t.downgrade()),
            Value::Function(_) => primitive_metatables[Function]
                .as_ref()
                .map(|t| t.downgrade()),
            Value::Table(LuaPtr(t)) => heap
                .get(*t)
                .ok_or(AlreadyDroppedError)?
                .metatable()
                .copied(),
            Value::Userdata(LuaPtr(t)) => heap
                .get(*t)
                .ok_or(AlreadyDroppedError)?
                .metatable()
                .copied(),
        };

        Ok(r)
    }
}

impl<Ty> Debug for Core<Ty>
where
    Ty: CoreTypes,
    StrongValue<Ty>: Debug,
    Ty::Table: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Core")
            .field("global_env", &self.global_env)
            .field("primitive_metatables", &self.primitive_metatables)
            .field("dialect", &self.dialect)
            .field("gc", &self.gc)
            .field("string_interner", &self.string_interner)
            .finish()
    }
}

pub struct Runtime<Ty, C>
where
    Ty: CoreTypes,
{
    pub core: Core<Ty>,
    pub chunk_cache: C,
    orchestrator: Orchestrator<Ty>,
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: CoreTypes,
    C: Debug,
{
    pub fn new(chunk_cache: C, mut core: Core<Ty>) -> Self {
        tracing::trace!(?chunk_cache, "constructed runtime");

        let orchestrator = Orchestrator::new(&mut core.gc);
        Runtime {
            core,
            chunk_cache,
            orchestrator,
        }
    }
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: CoreTypes,
    C: ChunkCache,
{
    fn context(&mut self) -> (orchestrator::Context<Ty>, &mut Orchestrator<Ty>) {
        use orchestrator::Context;

        let Runtime {
            core,
            chunk_cache,
            orchestrator,
        } = self;

        let ctx = Context { core, chunk_cache };

        (ctx, orchestrator)
    }
}

impl<Ty, C> Runtime<Ty, C>
where
    Ty: CoreTypes<LuaClosure = Closure<Ty>>,
    Ty::RustClosure: DLuaFfi<Ty>,
    C: ChunkCache,
{
    pub fn enter(&mut self, callable: Callable<Strong, Ty>) -> Result<(), RtError<Ty>> {
        let (mut ctx, orchestrator) = self.context();
        let id = orchestrator.new_thread(ctx.reborrow(), callable);
        orchestrator.push(id);

        orchestrator.enter(ctx)
    }
}

pub struct RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
{
    pub core: &'rt mut Core<Ty>,
    pub chunk_cache: &'rt mut dyn ChunkCache,
    pub stack: StackGuard<'rt, Ty>,
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
{
    pub fn reborrow(&mut self) -> RuntimeView<'_, Ty> {
        let RuntimeView {
            core,
            chunk_cache,
            stack,
        } = self;

        RuntimeView {
            core: *core,
            chunk_cache: *chunk_cache,
            stack: stack.reborrow(),
        }
    }
}

// impl<'rt, Ty> RuntimeView<'rt, Ty>
// where
//     Ty: CoreTypes,
// {
/// Return runtime into consistent state.
///
/// This function is useful in case you caught Lua panic
/// (one of the methods returned `RuntimeError`)
/// and you want to continue executing code inside this runtime.
/// Lua panic may interrupt execution at arbitrary point
/// potentially leaving internal structures in inconsistent state.
/// Resuming execution on such runtime results *Lua undefined behavior*.
///
/// Invoking `reset` will purge stack and bring internals into consistent state
/// making it safe to execute Lua code once again.
/// As such you should collect any useful information about error (e.g. backtrace)
/// before resetting in case you need it.
///
/// Note that the "consistency" part applies only to runtime itself but not to Lua constructs!
/// It is entirely possible for Lua to panic while modifying some state
/// and since most things (tables, closures, etc.) are shared through references,
/// this corrupted state may be observed by outside code.
/// If that code doesn't expect to find malformed data it may lead to weird and/or buggy behavior.
/// There is nothing runtime can do to help you.
/// In case this presents an issue,
/// the best course of action is to discard the runtime and construct a fresh one.
// pub fn reset(&mut self) {
//     self.stack.lua_frame().clear();
//     self.soft_reset();
// }

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
{
    pub fn construct_closure(
        &mut self,
        fn_ptr: FunctionPtr,
        upvalues: impl IntoIterator<Item = WeakValue<Ty>>,
    ) -> Result<RootCell<Closure<Ty>>, RtError<Ty>> {
        Closure::new(self, fn_ptr, upvalues)
    }

    pub fn chunk_cache(&self) -> &dyn ChunkCache {
        self.chunk_cache
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
{
    pub fn load(
        &mut self,
        source: String,
        location: Option<Location>,
    ) -> Result<ChunkId, LoadError> {
        use codespan_reporting::files::SimpleFile;
        use logos::Logos;
        use parser::lex::Token;

        let lexer = Token::lexer(&source);
        let chunk = match parser::parser::chunk(lexer) {
            Ok(chunk) => chunk,
            Err(err) => {
                let name = location
                    .map(|loc| loc.file.clone())
                    .unwrap_or_else(|| "<unnamed>".to_string());
                let diag = Diagnostic {
                    files: SimpleFile::new(name, source),
                    message: err.into_diagnostic(),
                };

                return Err(diag.into());
            }
        };

        let chunk_id = self.chunk_cache.insert(chunk, Some(source), location)?;

        Ok(chunk_id)
    }

    pub fn load_from_file(&mut self, path: impl AsRef<Path>) -> Result<ChunkId, RtError<Ty>>
    where
        Ty::String: From<String>,
    {
        let path = path.as_ref();

        let source = std::fs::read_to_string(path).map_err(|err| {
            self.core.alloc_error_msg(format!(
                "failed to load file {}: {err}",
                path.to_string_lossy()
            ))
        })?;
        let location = Location {
            file: path.to_string_lossy().to_string(),
            line: 0,
            column: 0,
        };

        self.load(source, Some(location)).map_err(Into::into)
    }
}

impl<'rt, Ty> RuntimeView<'rt, Ty>
where
    Ty: CoreTypes,
{
    // pub fn backtrace(&self) -> Backtrace {
    //     use rust_backtrace_stack::RustFrame;

    //     let mut start = self.frames.boundary();
    //     let mut frames = Vec::new();

    //     for rust_frame in self.rust_backtrace_stack.iter() {
    //         let RustFrame {
    //             position,
    //             backtrace,
    //         } = rust_frame;

    //         frames.extend(
    //             self.frames
    //                 .range(start..*position)
    //                 .unwrap_or_default()
    //                 .iter()
    //                 .map(|frame| frame.backtrace(&self.core.gc, self.chunk_cache)),
    //         );
    //         frames.push(backtrace.clone());
    //         start = *position
    //     }

    //     frames.extend(
    //         self.frames
    //             .range(start..)
    //             .unwrap_or_default()
    //             .iter()
    //             .map(|frame| frame.backtrace(&self.core.gc, self.chunk_cache)),
    //     );

    //     Backtrace { frames }
    // }
}

#[derive(Debug)]
pub enum LoadError {
    Immutable(crate::chunk_cache::ImmutableCacheError),
    CompilationFailure(Box<Diagnostic>),
}

impl From<crate::chunk_cache::ImmutableCacheError> for LoadError {
    fn from(value: crate::chunk_cache::ImmutableCacheError) -> Self {
        LoadError::Immutable(value)
    }
}

impl From<Diagnostic> for LoadError {
    fn from(value: Diagnostic) -> Self {
        LoadError::CompilationFailure(Box::new(value))
    }
}

impl<Value> From<LoadError> for RuntimeError<Value> {
    fn from(value: LoadError) -> Self {
        match value {
            LoadError::Immutable(err) => RuntimeError::Immutable(err),
            LoadError::CompilationFailure(diag) => RuntimeError::Diagnostic(diag),
        }
    }
}
