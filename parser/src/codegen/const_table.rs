use std::collections::HashMap;

use ordered_float::NotNan;

use repr::index::ConstId;
use repr::literal::Literal;
use repr::tivec::TiVec;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum HashLiteral {
    Nil,
    Bool(bool),
    Int(i64),
    Float(NotNan<f64>),
    String(String),
}

impl HashLiteral {
    fn new(literal: Literal) -> Option<Self> {
        let r = match literal {
            Literal::Nil => HashLiteral::Nil,
            Literal::Bool(t) => HashLiteral::Bool(t),
            Literal::Int(t) => HashLiteral::Int(t),
            Literal::Float(t) => HashLiteral::Float(NotNan::new(t).ok()?),
            Literal::String(t) => HashLiteral::String(t),
        };

        Some(r)
    }
}

#[derive(Debug, Default)]
pub struct ConstTable {
    constants: TiVec<ConstId, Literal>,
    backlinks: HashMap<HashLiteral, ConstId>,
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

        if let Some(key) = HashLiteral::new(value.clone()) {
            match self.backlinks.entry(key) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let id = self.constants.push_and_get_key(value);
                    entry.insert(id);

                    id
                }
            }
        } else {
            self.constants.push_and_get_key(value)
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
            if let Some(key) = HashLiteral::new(literal) {
                self.backlinks.remove(&key);
            }
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
