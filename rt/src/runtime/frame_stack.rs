use repr::tivec::TiVec;

use std::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct RawFrameId(usize);

impl From<usize> for RawFrameId {
    fn from(value: usize) -> Self {
        RawFrameId(value)
    }
}

impl From<RawFrameId> for usize {
    fn from(value: RawFrameId) -> Self {
        value.0
    }
}

#[derive(Debug)]
pub(crate) struct FrameStack<Frame> {
    stack: TiVec<RawFrameId, Frame>,
}

impl<Frame> FrameStack<Frame> {
    pub(crate) fn push(&mut self, frame: Frame) {
        self.stack.push(frame)
    }

    pub(crate) fn pop(&mut self) -> Option<Frame> {
        self.stack.pop()
    }

    pub(crate) fn truncate(&mut self, id: RawFrameId) {
        self.stack.truncate(id.0)
    }
}

impl<C> Default for FrameStack<C> {
    fn default() -> Self {
        Self {
            stack: Default::default(),
        }
    }
}
