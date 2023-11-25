use repr::chunk::ClosureRecipe;
use repr::index::RecipeId;
use repr::tivec::TiVec;

#[derive(Debug, Default)]
pub struct RecipeTable {
    recipes: TiVec<RecipeId, ClosureRecipe>,
}

impl RecipeTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn view(&mut self) -> RecipeTableView {
        RecipeTableView::new(self)
    }

    pub fn resolve(self) -> TiVec<RecipeId, ClosureRecipe> {
        self.recipes
    }

    fn inner_state(&self) -> InnerState {
        InnerState {
            recipes: self.recipes.next_key(),
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState { recipes } = state;

        self.recipes.truncate(recipes.into());
    }
}

#[derive(Debug, Clone, Copy)]
struct InnerState {
    recipes: RecipeId,
}

#[derive(Debug)]
pub struct RecipeTableView<'a> {
    recipe_table: &'a mut RecipeTable,
    inner_state: InnerState,
}

impl<'a> RecipeTableView<'a> {
    pub fn new(recipe_table: &'a mut RecipeTable) -> Self {
        let inner_state = recipe_table.inner_state();

        RecipeTableView {
            recipe_table,
            inner_state,
        }
    }

    pub fn borrow(&mut self) -> &mut RecipeTable {
        self.recipe_table
    }

    pub fn push(&mut self, func: ClosureRecipe) -> RecipeId {
        self.recipe_table.recipes.push_and_get_key(func)
    }

    pub fn commit(self) {
        std::mem::forget(self)
    }
}

impl<'a> Drop for RecipeTableView<'a> {
    fn drop(&mut self) {
        let RecipeTableView {
            recipe_table,
            inner_state,
        } = self;

        recipe_table.apply(*inner_state)
    }
}
