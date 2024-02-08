use repr::tivec::TiVec;
use std::ops::RangeBounds;

use std::fmt::Debug;

use super::frame::Frame;
use super::MapBound;

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

#[derive(Debug, Clone, Copy)]
pub(crate) struct FrameId(usize);

#[derive(Debug)]
pub(crate) struct FrameStack<Value> {
    stack: TiVec<RawFrameId, Frame<Value>>,
}

impl<Value> FrameStack<Value> {
    pub(crate) fn view(&mut self) -> FrameStackView<Value> {
        FrameStackView::new(self)
    }

    fn next_id(&self) -> RawFrameId {
        self.stack.next_key()
    }

    fn push(&mut self, frame: Frame<Value>) {
        self.stack.push(frame)
    }

    fn pop(&mut self) -> Option<Frame<Value>> {
        self.stack.pop()
    }

    fn truncate(&mut self, id: RawFrameId) {
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

pub(crate) struct FrameStackView<'a, Value> {
    stack: &'a mut FrameStack<Value>,
    boundary: RawFrameId,
}

impl<'a, Value> FrameStackView<'a, Value> {
    pub(crate) fn new(frames: &'a mut FrameStack<Value>) -> Self {
        FrameStackView {
            stack: frames,
            boundary: RawFrameId(0),
        }
    }

    pub(crate) fn view(&mut self) -> FrameStackView<Value> {
        let boundary = self.stack.next_id();

        FrameStackView {
            stack: self.stack,
            boundary,
        }
    }

    pub(crate) fn last(&self) -> Option<&Frame<Value>> {
        self.stack.stack.last()
    }

    pub(crate) fn next_raw_id(&self) -> RawFrameId {
        self.stack.next_id()
    }

    pub(crate) fn pop(&mut self) -> Option<Frame<Value>> {
        if self.stack.next_id() <= self.boundary {
            return None;
        }

        self.stack.pop()
    }

    pub(crate) fn push(&mut self, frame: Frame<Value>) {
        self.stack.push(frame)
    }

    pub(crate) fn clear(&mut self) {
        self.stack.truncate(self.boundary)
    }

    pub(crate) fn boundary(&self) -> RawFrameId {
        self.boundary
    }

    pub(crate) fn range(&self, range: impl RangeBounds<RawFrameId>) -> Option<&[Frame<Value>]> {
        let start = range.start_bound().mapb(|id| id.0);
        let end = range.end_bound().mapb(|id| id.0);

        self.stack.stack.raw.get((start, end))
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<'_, Frame<Value>> {
        self.stack.stack.iter()
    }
}
