use std::collections::HashMap;

use repr::index::ConstId;
use repr::literal::Literal;
use repr::tivec::TiVec;

#[derive(Debug, Default)]
pub struct ConstTable {
    constants: TiVec<ConstId, Literal>,
    backlinks: HashMap<Literal, ConstId>,
}

impl ConstTable {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn view(&mut self) -> ConstTableView {
        ConstTableView::new(self)
    }

    pub fn insert(&mut self, value: Literal) -> ConstId {
        use std::collections::hash_map::Entry;

        match self.backlinks.entry(value) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let value = entry.key().clone();
                let id = self.constants.push_and_get_key(value);
                entry.insert(id);

                id
            }
        }
    }

    pub fn resolve(self) -> TiVec<ConstId, Literal> {
        self.constants
    }

    fn inner_state(&self) -> InnerState {
        InnerState {
            constants: self.constants.next_key(),
        }
    }

    fn apply(&mut self, state: InnerState) {
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

    pub fn borrow(&mut self) -> &mut ConstTable {
        self.constants
    }

    pub fn insert(&mut self, value: Literal) -> ConstId {
        self.constants.insert(value)
    }

    pub fn commit(self) {
        std::mem::forget(self);
    }
}

impl<'a> Drop for ConstTableView<'a> {
    fn drop(&mut self) {
        self.constants.apply(self.inner_state)
    }
}
