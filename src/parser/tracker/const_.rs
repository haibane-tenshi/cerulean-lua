use std::collections::HashMap;

use thiserror::Error;

use crate::index_vec::IndexVec;
use crate::opcode::ConstId;
use crate::value::Literal;

#[derive(Debug, Error)]
#[error("exceeded indexing capacity of constant ids")]
pub struct ExceededConstIdError;

#[derive(Debug, Default)]
pub struct ConstTracker {
    constants: IndexVec<ConstId, Literal>,
    backlinks: HashMap<Literal, ConstId>,
}

impl ConstTracker {
    pub fn insert(&mut self, value: Literal) -> Result<ConstId, ExceededConstIdError> {
        use std::collections::hash_map::Entry;

        let id = match self.backlinks.entry(value) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let value = entry.key().clone();
                let id = self
                    .constants
                    .push(value)
                    .map_err(|_| ExceededConstIdError)?;
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
