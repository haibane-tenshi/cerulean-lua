use std::collections::HashMap;

use repr::index::{ConstCapacityError, ConstId};
use repr::index_vec::IndexVec;
use repr::literal::Literal;

#[derive(Debug, Default)]
pub struct ConstTable {
    constants: IndexVec<ConstId, Literal>,
    backlinks: HashMap<Literal, ConstId>,
}

impl ConstTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn view(&mut self) -> ConstTableView {
        ConstTableView::new(self)
    }

    pub fn insert(&mut self, value: Literal) -> Result<ConstId, ConstCapacityError> {
        use std::collections::hash_map::Entry;

        let id = match self.backlinks.entry(value) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let value = entry.key().clone();
                let id = self.constants.push(value)?;
                entry.insert(id);

                id
            }
        };

        Ok(id)
    }

    pub fn resolve(self) -> IndexVec<ConstId, Literal> {
        self.constants
    }

    fn inner_state(&self) -> InnerState {
        InnerState {
            constants: self.constants.len(),
        }
    }

    fn apply_inner_state(&mut self, state: InnerState) {
        let InnerState { constants } = state;

        for literal in self.constants.drain(constants..) {
            self.backlinks.remove(&literal);
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct InnerState {
    constants: ConstId,
}

#[derive(Debug)]
pub struct ConstTableView<'a> {
    constants: &'a mut ConstTable,
    inner_state: InnerState,
}

impl<'a> ConstTableView<'a> {
    pub fn new(constants: &'a mut ConstTable) -> Self {
        let inner_state = constants.inner_state();

        ConstTableView {
            constants,
            inner_state,
        }
    }

    pub fn new_view(&mut self) -> ConstTableView<'_> {
        ConstTableView::new(self.constants)
    }

    pub fn try_insert(&mut self, value: Literal) -> Result<ConstId, ConstCapacityError> {
        self.constants.insert(value)
    }

    pub fn insert(&mut self, value: Literal) -> ConstId {
        self.try_insert(value).unwrap()
    }

    pub fn commit(self) {
        std::mem::forget(self);
    }
}

impl<'a> Drop for ConstTableView<'a> {
    fn drop(&mut self) {
        self.constants.apply_inner_state(self.inner_state)
    }
}
