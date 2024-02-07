use repr::chunk::{Chunk, ChunkExtension, ClosureRecipe, Function};
use repr::index::{ConstId, FunctionId, RecipeId};
use repr::literal::Literal;
use std::fmt::Display;
use std::ops::Range;

use crate::chunk_cache::ChunkId;
use crate::error::RuntimeError;
use crate::runtime::{FunctionPtr, RuntimeView};
use crate::value::{TypeProvider, Value};

#[derive(Debug, Clone)]
pub struct ChunkRange {
    pub chunk_id: ChunkId,
    pub function_ids: Range<FunctionId>,
    pub constant_ids: Range<ConstId>,
    pub recipe_ids: Range<RecipeId>,
}

impl ChunkRange {
    pub fn make_ptr(&self, fn_id: FunctionId) -> Option<FunctionPtr> {
        let function_id = self.function_id(fn_id)?;

        let r = FunctionPtr {
            chunk_id: self.chunk_id,
            function_id,
        };

        Some(r)
    }

    pub fn function_id(&self, fn_id: FunctionId) -> Option<FunctionId> {
        let function_id = FunctionId(fn_id.0 + self.function_ids.start.0);

        self.function_ids
            .contains(&function_id)
            .then_some(function_id)
    }

    pub fn constant_id(&self, const_id: ConstId) -> Option<ConstId> {
        let const_id = ConstId(const_id.0 + self.constant_ids.start.0);

        self.constant_ids.contains(&const_id).then_some(const_id)
    }

    pub fn recipe_id(&self, recipe_id: RecipeId) -> Option<RecipeId> {
        let recipe_id = RecipeId(recipe_id.0 + self.recipe_ids.start.0);

        self.recipe_ids.contains(&recipe_id).then_some(recipe_id)
    }
}

pub struct Part<Fun, Con, Rec, F> {
    pub chunk_ext: ChunkExtension<Fun, Con, Rec>,
    pub builder: F,
}

pub fn builder<Gc: TypeProvider, T>() -> ValueBuilder<
    impl for<'rt, 'a> FnOnce(RuntimeView<'rt, Gc>, ChunkId, T) -> Result<T, RuntimeError<Gc>>,
> {
    fn builder<Gc: TypeProvider, T>(
        _: RuntimeView<Gc>,
        _: ChunkId,
        value: T,
    ) -> Result<T, RuntimeError<Gc>> {
        Ok(value)
    }

    ValueBuilder {
        chunk: Default::default(),
        builder,
    }
}

pub struct ValueBuilder<F> {
    chunk: Chunk,
    builder: F,
}

impl<P> ValueBuilder<P> {
    // pub fn new<Fun, Con, Rec>(part: ChunkPart<Fun, Con, Rec, P>) -> Self
    // where
    //     Fun: IntoIterator<Item = Function>,
    //     Con: IntoIterator<Item = Literal>,
    //     Rec: IntoIterator<Item = ClosureRecipe>,
    // {
    //     let ChunkPart { chunk_ext, builder } = part;

    //     let chunk = chunk_ext.into();

    //     ChunkBuilder { chunk, builder }
    // }

    pub fn include<Fun, Con, Rec, Gc, F, T, U, V>(
        self,
        chunk_part: Part<Fun, Con, Rec, F>,
    ) -> ValueBuilder<impl FnOnce(RuntimeView<Gc>, ChunkId, T) -> Result<V, RuntimeError<Gc>>>
    where
        Gc: TypeProvider,
        Value<Gc>: Display,
        P: FnOnce(RuntimeView<Gc>, ChunkId, T) -> Result<U, RuntimeError<Gc>>,
        Fun: IntoIterator<Item = Function>,
        Con: IntoIterator<Item = Literal>,
        Rec: IntoIterator<Item = ClosureRecipe>,
        F: FnOnce(RuntimeView<Gc>, ChunkRange, U) -> Result<V, RuntimeError<Gc>>,
    {
        let Part {
            chunk_ext,
            builder: part_builder,
        } = chunk_part;

        let ValueBuilder { mut chunk, builder } = self;

        let function_start = chunk.functions.next_key();
        let constant_start = chunk.constants.next_key();
        let recipe_start = chunk.closure_recipes.next_key();

        chunk.extend(chunk_ext);

        let function_end = chunk.functions.next_key();
        let constant_end = chunk.constants.next_key();
        let recipe_end = chunk.closure_recipes.next_key();

        let function_ids = function_start..function_end;
        let constant_ids = constant_start..constant_end;
        let recipe_ids = recipe_start..recipe_end;

        let builder = move |mut rt: RuntimeView<Gc>, chunk_id: ChunkId, value: T| {
            let value = builder(rt.view_full(), chunk_id, value)?;

            let chunk_part = ChunkRange {
                chunk_id,
                function_ids,
                constant_ids,
                recipe_ids,
            };

            let value = part_builder(rt, chunk_part, value)?;

            Ok(value)
        };

        ValueBuilder { chunk, builder }
    }

    pub fn finish<Gc, T, U>(
        self,
    ) -> (
        Chunk,
        impl FnOnce(RuntimeView<Gc>, ChunkId, T) -> Result<U, RuntimeError<Gc>>,
    )
    where
        Gc: TypeProvider,
        P: FnOnce(RuntimeView<Gc>, ChunkId, T) -> Result<U, RuntimeError<Gc>>,
    {
        let ValueBuilder { chunk, builder } = self;

        (chunk, builder)
    }
}
