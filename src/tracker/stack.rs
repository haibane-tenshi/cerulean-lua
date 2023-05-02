use std::collections::HashMap;

use thiserror::Error;

use crate::opcode::StackSlot;

#[derive(Debug, Error)]
pub enum StackStateError {
    #[error("attempted to modify stack behind protected boundary")]
    BoundaryViolation,

    #[error("temporary doesn't exist")]
    MissingTemporary,

    #[error("block doesn't exist")]
    MissingBlock,

    #[error("frame doesn't exist")]
    MissingFrame,

    #[error("there is no statically known upper bound on the stack")]
    VariadicStack,

    #[error("temporary already have a name")]
    NameAlias,
}

#[derive(Debug, Copy, Clone)]
pub struct BlockId(usize);

#[derive(Debug, Default)]
pub struct StackTracker<'s> {
    /// Implies there is an unknowable number of local variables residing on top of known stack.
    variadic: bool,
    stack: Vec<Option<&'s str>>,
    backlinks: Backlinks<'s>,
    blocks: Vec<usize>,
}

impl<'s> StackTracker<'s> {
    fn block_base(&self) -> usize {
        self.blocks.last().copied().unwrap_or_default()
    }

    fn slot_to_index(&self, slot: StackSlot) -> usize {
        slot.0.try_into().unwrap()
    }

    fn index_to_slot(&self, index: usize) -> Option<StackSlot> {
        let slot = index.try_into().ok()?;

        Some(StackSlot(slot))
    }

    fn adjust_to_height(&mut self, height: usize) -> Result<bool, StackStateError> {
        use std::cmp::Ordering;

        if height < self.block_base() {
            return Err(StackStateError::BoundaryViolation);
        }

        let adjusted = match height.cmp(&self.stack.len()) {
            Ordering::Equal => false,
            Ordering::Less => {
                let names = self.stack.drain(height..).flatten();

                for name in names {
                    self.backlinks.pop(name);
                }

                true
            }
            Ordering::Greater => {
                self.stack
                    .extend(std::iter::repeat(None).take(height - self.stack.len()));

                true
            }
        };

        let adjusted = adjusted || self.variadic;
        self.variadic = false;

        Ok(adjusted)
    }

    pub fn adjust_to(&mut self, slot: StackSlot) -> Result<bool, StackStateError> {
        let height = self.slot_to_index(slot);
        self.adjust_to_height(height)
    }

    pub fn make_variadic(&mut self) {
        self.variadic = true;
    }

    pub fn push(&mut self, name: Option<&'s str>) -> Result<StackSlot, StackStateError> {
        let slot = self.top()?;
        let index = self.stack.len();
        self.stack.push(name);

        if let Some(name) = name {
            self.backlinks.add(name, index);
        }

        Ok(slot)
    }

    pub fn pop(&mut self) -> Result<(), StackStateError> {
        if self.variadic {
            return Err(StackStateError::VariadicStack);
        }

        if let Some(name) = self.stack.pop().ok_or(StackStateError::MissingTemporary)? {
            self.backlinks.pop(name);
        }

        Ok(())
    }

    pub fn top(&self) -> Result<StackSlot, StackStateError> {
        if self.variadic {
            return Err(StackStateError::VariadicStack);
        }

        let slot = self.index_to_slot(self.stack.len()).unwrap();
        Ok(slot)
    }

    pub fn lookup_local(&self, name: &str) -> Option<StackSlot> {
        let index = self.backlinks.get(name)?;
        self.index_to_slot(index)
    }

    pub fn start_block(&mut self) -> Result<BlockId, StackStateError> {
        if self.variadic {
            return Err(StackStateError::VariadicStack);
        }

        let id = BlockId(self.blocks.len());
        self.blocks.push(self.stack.len());

        Ok(id)
    }

    pub fn finish_block(&mut self, id: BlockId) -> Result<StackSlot, StackStateError> {
        let height = *self.blocks.get(id.0).ok_or(StackStateError::MissingBlock)?;
        self.adjust_to_height(height)?;
        self.blocks.truncate(id.0);

        let slot = self.index_to_slot(height).unwrap();

        Ok(slot)
    }

    pub fn name_local(&mut self, slot: StackSlot, name: &'s str) -> Result<(), StackStateError> {
        let index = self.slot_to_index(slot);
        let place = self
            .stack
            .get_mut(index)
            .ok_or(StackStateError::MissingTemporary)?;

        if place.is_some() {
            return Err(StackStateError::NameAlias);
        }

        *place = Some(name);
        self.backlinks.add(name, index);

        Ok(())
    }
}

#[derive(Debug, Default)]
struct Backlinks<'s>(HashMap<&'s str, Vec<usize>>);

impl<'s> Backlinks<'s> {
    fn add(&mut self, name: &'s str, index: usize) {
        self.0.entry(name).or_default().push(index);
    }

    fn pop(&mut self, name: &'s str) {
        let Some(backlink) = self.0.get_mut(&name) else {
            return
        };

        backlink.pop();

        if backlink.is_empty() {
            self.0.remove(&name);
        }
    }

    fn get(&self, name: &'s str) -> Option<usize> {
        self.0.get(name)?.last().copied()
    }
}
