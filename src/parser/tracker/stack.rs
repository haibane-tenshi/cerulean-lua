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
}

#[derive(Debug, Default)]
pub struct StackTracker<'s> {
    stack: Vec<Option<&'s str>>,
    backlinks: Backlinks<'s>,
    blocks: Vec<usize>,
    frames: Vec<usize>,
}

impl<'s> StackTracker<'s> {
    fn frame_base(&self) -> usize {
        self.frames.last().copied().unwrap_or_default()
    }

    fn block_base(&self) -> usize {
        self.blocks.last().copied().unwrap_or_default()
    }

    fn slot_to_index(&self, slot: StackSlot) -> usize {
        let offset: usize = slot.0.try_into().unwrap();
        self.frame_base() + offset
    }

    fn index_to_slot(&self, index: usize) -> Option<StackSlot> {
        let offset = index.checked_sub(self.frame_base())?;
        let slot = offset.try_into().unwrap();

        Some(StackSlot(slot))
    }

    fn adjust_to_height(&mut self, height: usize) -> Result<(), StackStateError> {
        if height < self.block_base() {
            return Err(StackStateError::BoundaryViolation);
        }

        let names = self.stack.drain(height..).flatten();

        for name in names {
            self.backlinks.pop(name);
        }

        Ok(())
    }

    pub fn adjust_to(&mut self, slot: StackSlot) -> Result<(), StackStateError> {
        let height = self.slot_to_index(slot);
        self.adjust_to_height(height)
    }

    pub fn push(&mut self, name: Option<&'s str>) -> StackSlot {
        let index = self.stack.len();
        self.stack.push(name);

        if let Some(name) = name {
            self.backlinks.add(name, index);
        }

        self.index_to_slot(index).unwrap()
    }

    pub fn pop(&mut self) -> Result<(), StackStateError> {
        if self.stack.len() < self.frame_base() {
            return Err(StackStateError::BoundaryViolation);
        }

        if let Some(name) = self.stack.pop().ok_or(StackStateError::MissingTemporary)? {
            self.backlinks.pop(name);
        }

        Ok(())
    }

    pub fn top(&self) -> Option<StackSlot> {
        if self.stack.len() < self.frame_base() {
            return None;
        }

        let index = self.stack.len().checked_sub(1)?;
        self.index_to_slot(index)
    }

    pub fn lookup_local(&self, name: &str) -> Option<StackSlot> {
        let index = self.backlinks.get(name)?;
        self.index_to_slot(index)
    }

    pub fn push_block(&mut self) {
        self.blocks.push(self.stack.len());
    }

    pub fn pop_block(&mut self) -> Result<u32, StackStateError> {
        // Blocks outside current frame need to be protected.
        if let Some(&block) = self.blocks.last() {
            if block < self.frame_base() {
                return Err(StackStateError::BoundaryViolation);
            }
        }

        let index = self.blocks.pop().ok_or(StackStateError::MissingBlock)?;
        let count = self.stack.len().checked_sub(index).unwrap_or_default();

        self.adjust_to_height(index)?;

        Ok(count.try_into().unwrap())
    }

    pub fn pop_ghost_block(&mut self) -> Result<u32, StackStateError> {
        let &block = self.blocks.last().ok_or(StackStateError::MissingBlock)?;

        // Blocks outside current frame need to be protected.
        if block < self.frame_base() {
            return Err(StackStateError::BoundaryViolation);
        }

        let count = self
            .stack
            .len()
            .checked_sub(block)
            .unwrap_or_default()
            .try_into()
            .unwrap();

        Ok(count)
    }

    pub fn push_frame(&mut self) {
        self.frames.push(self.stack.len());
        self.push_block();
    }

    pub fn pop_frame(&mut self) -> Result<u32, StackStateError> {
        let mut count = 0;
        while let Ok(block) = self.pop_block() {
            count += block;
        }

        self.frames.pop().ok_or(StackStateError::MissingFrame)?;

        Ok(count)
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
