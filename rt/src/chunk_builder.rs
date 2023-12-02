use repr::chunk::{Chunk, ChunkExtension, ClosureRecipe, Function};
use repr::index::{ConstId, FunctionId, RecipeId};
use repr::literal::Literal;
use std::ops::Range;

use crate::chunk_cache::{ChunkCache, ChunkId};
use crate::error::RuntimeError;
use crate::runtime::{FunctionPtr, RuntimeView};
use crate::value::Value;

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

pub struct ChunkPart<Fun, Con, Rec, F> {
    pub chunk_ext: ChunkExtension<Fun, Con, Rec>,
    pub builder: F,
}

pub fn builder<C>() -> ChunkBuilder<
    impl for<'rt, 'a> FnOnce(RuntimeView<'rt, C>, ChunkId, &'a mut Value<C>) -> Result<(), RuntimeError>,
> {
    fn builder<C>(_: RuntimeView<C>, _: ChunkId, _: &mut Value<C>) -> Result<(), RuntimeError> {
        Ok(())
    }

    ChunkBuilder {
        chunk: Default::default(),
        builder,
    }
}

pub struct ChunkBuilder<F> {
    pub(crate) chunk: Chunk,
    pub(crate) builder: F,
}

impl<P> ChunkBuilder<P> {
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

    pub fn add<Fun, Con, Rec, Cache, F>(
        self,
        chunk_part: ChunkPart<Fun, Con, Rec, F>,
    ) -> ChunkBuilder<
        impl FnOnce(RuntimeView<Cache>, ChunkId, &mut Value<Cache>) -> Result<(), RuntimeError>,
    >
    where
        P: FnOnce(RuntimeView<Cache>, ChunkId, &mut Value<Cache>) -> Result<(), RuntimeError>,
        Fun: IntoIterator<Item = Function>,
        Con: IntoIterator<Item = Literal>,
        Rec: IntoIterator<Item = ClosureRecipe>,
        F: FnOnce(RuntimeView<Cache>, ChunkRange, &mut Value<Cache>) -> Result<(), RuntimeError>,
    {
        let ChunkPart {
            chunk_ext,
            builder: part_builder,
        } = chunk_part;

        let ChunkBuilder { mut chunk, builder } = self;

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

        let builder =
            move |mut rt: RuntimeView<Cache>, chunk_id: ChunkId, value: &mut Value<Cache>| {
                use repr::index::StackSlot;

                builder(rt.view(StackSlot(0)).unwrap(), chunk_id, value)?;

                let chunk_part = ChunkRange {
                    chunk_id,
                    function_ids,
                    constant_ids,
                    recipe_ids,
                };

                part_builder(rt, chunk_part, value)?;

                Ok(())
            };

        ChunkBuilder { chunk, builder }
    }

    pub fn finish<C>(
        self,
    ) -> (
        Chunk,
        impl FnOnce(RuntimeView<C>, ChunkId) -> Result<Value<C>, RuntimeError>,
    )
    where
        P: FnOnce(RuntimeView<C>, ChunkId, &mut Value<C>) -> Result<(), RuntimeError>,
        C: ChunkCache<ChunkId>,
    {
        let ChunkBuilder { chunk, builder } = self;

        let f = |rt: RuntimeView<'_, C>, chunk_id: ChunkId| {
            let mut value = Value::Nil;
            builder(rt, chunk_id, &mut value)?;

            Ok(value)
        };

        (chunk, f)
    }
}
