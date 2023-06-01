use std::collections::HashMap;

use repr::index::{ConstCapacityError, ConstId};
use repr::index_vec::IndexVec;
use repr::literal::Literal;

#[derive(Debug, Default)]
pub struct ConstTracker {
    constants: IndexVec<ConstId, Literal>,
    backlinks: HashMap<Literal, ConstId>,
}

impl ConstTracker {
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
}
