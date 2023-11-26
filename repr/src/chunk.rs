use std::fmt::Display;

use rle_vec::RleVec;

use crate::index::{ConstId, FunctionId, InstrId, RecipeId, StackSlot, UpvalueSlot};
use crate::literal::Literal;
use crate::opcode::OpCode;
use crate::tivec::TiVec;

#[derive(Debug, Clone)]
pub struct Chunk {
    pub functions: TiVec<FunctionId, Function>,
    pub constants: TiVec<ConstId, Literal>,
    pub closure_recipes: TiVec<RecipeId, ClosureRecipe>,
}

impl Chunk {
    pub fn get_constant(&self, index: ConstId) -> Option<&Literal> {
        self.constants.get(index)
    }

    pub fn get_function(&self, index: FunctionId) -> Option<&Function> {
        self.functions.get(index)
    }

    pub fn get_recipe(&self, index: RecipeId) -> Option<&ClosureRecipe> {
        self.closure_recipes.get(index)
    }

    pub fn extend<F, C, R>(&mut self, extension: ChunkExtension<F, C, R>)
    where
        F: IntoIterator<Item = Function>,
        C: IntoIterator<Item = Literal>,
        R: IntoIterator<Item = ClosureRecipe>,
    {
        let function_offset = self.functions.len();
        let constant_offset = self.constants.len();
        let recipe_offset = self.closure_recipes.len();

        let ChunkExtension {
            functions,
            constants,
            closure_recipes,
        } = extension;

        self.functions.extend(functions);
        self.constants.extend(constants);
        self.closure_recipes.extend(closure_recipes);

        for opcode in self.functions[FunctionId(function_offset)..]
            .iter_mut()
            .flat_map(|fun| fun.codes.iter_mut())
        {
            match opcode {
                OpCode::LoadConstant(ConstId(id)) => *id += constant_offset,
                OpCode::MakeClosure(RecipeId(id)) => *id += recipe_offset,
                _ => (),
            }
        }

        for FunctionId(fn_id) in self.closure_recipes[RecipeId(recipe_offset)..]
            .iter_mut()
            .map(|recipe| &mut recipe.function_id)
        {
            *fn_id += function_offset;
        }
    }
}

impl<F, C, R> From<ChunkExtension<F, C, R>> for Chunk
where
    F: IntoIterator<Item = Function>,
    C: IntoIterator<Item = Literal>,
    R: IntoIterator<Item = ClosureRecipe>,
{
    fn from(value: ChunkExtension<F, C, R>) -> Self {
        let ChunkExtension {
            functions,
            constants,
            closure_recipes,
        } = value;

        let functions = functions.into_iter().collect();
        let constants = constants.into_iter().collect();
        let closure_recipes = closure_recipes.into_iter().collect();

        Chunk {
            functions,
            constants,
            closure_recipes,
        }
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "== chunk ==")?;

        writeln!(f, "== constant table ==")?;
        for (i, literal) in self.constants.iter().enumerate() {
            writeln!(f, "[{i:03}] {literal}")?;
        }

        writeln!(f)?;

        writeln!(f, "== function table ==")?;
        for (i, fun) in self.functions.iter().enumerate() {
            writeln!(f, ":: function id {i:3} ::")?;
            writeln!(f, "{fun}")?;
        }

        writeln!(f, "== recipe table ==")?;
        for (i, recipe) in self.closure_recipes.iter().enumerate() {
            writeln!(f, ":: recipe id {i:3} ::")?;
            writeln!(f, "{recipe}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Function {
    pub codes: TiVec<InstrId, OpCode>,
    pub lines: RleVec<u32>,
    pub signature: Signature,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[derive(Copy, Clone)]
        enum LineNumber {
            Explicit(u32),
            Repeat,
        }

        writeln!(f, "# signature")?;
        let arg_count = self.signature.height.checked_sub(1).unwrap_or_default();
        writeln!(
            f,
            "  height:   {} ({arg_count} real args)",
            self.signature.height
        )?;
        writeln!(f, "  variadic: {}", self.signature.is_variadic)?;
        writeln!(f, "  upvalues: {}", self.signature.upvalue_count)?;

        writeln!(f)?;
        writeln!(f, "# body")?;

        let iter = self.codes.iter().copied().enumerate().zip(
            self.lines
                .runs()
                .flat_map(|run| {
                    std::iter::once(LineNumber::Explicit(*run.value))
                        .chain(std::iter::repeat(LineNumber::Repeat).take(run.len - 1))
                })
                .map(Some)
                .chain(std::iter::repeat(None)),
        );

        for ((i, code), line) in iter {
            let line = match line {
                Some(LineNumber::Explicit(n)) => n.to_string(),
                Some(LineNumber::Repeat) => "|".to_string(),
                None => "?".to_string(),
            };

            writeln!(f, "  {i:04} {line:>3} {code}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub struct Signature {
    pub upvalue_count: usize,
    pub height: usize,
    pub is_variadic: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ClosureRecipe {
    pub function_id: FunctionId,
    pub upvalues: TiVec<UpvalueSlot, UpvalueSource>,
}

impl Display for ClosureRecipe {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "function_id: {}", self.function_id)?;
        if self.upvalues.is_empty() {
            writeln!(f, "upvalues: []")?;
        } else {
            writeln!(f, "upvalues: [")?;
            for upvalue in self.upvalues.iter() {
                writeln!(f, "  {upvalue:?},")?
            }
            writeln!(f, "]")?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum UpvalueSource {
    Temporary(StackSlot),
    Upvalue(UpvalueSlot),
}

#[derive(Debug, Copy, Clone)]
pub struct ChunkExtension<F, C, R> {
    pub functions: F,
    pub constants: C,
    pub closure_recipes: R,
}

impl From<Chunk>
    for ChunkExtension<
        TiVec<FunctionId, Function>,
        TiVec<ConstId, Literal>,
        TiVec<RecipeId, ClosureRecipe>,
    >
{
    fn from(value: Chunk) -> Self {
        let Chunk {
            functions,
            constants,
            closure_recipes,
        } = value;

        ChunkExtension {
            functions,
            constants,
            closure_recipes,
        }
    }
}
