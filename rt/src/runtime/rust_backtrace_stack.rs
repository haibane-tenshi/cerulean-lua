use repr::tivec::TiVec;

use super::frame_stack::RawFrameId;
use crate::backtrace::BacktraceFrame;

#[derive(Debug, Clone, Copy)]
struct RawId(usize);

impl From<usize> for RawId {
    fn from(value: usize) -> Self {
        RawId(value)
    }
}

impl From<RawId> for usize {
    fn from(value: RawId) -> Self {
        value.0
    }
}

#[derive(Debug)]
pub(crate) struct RustFrame {
    pub(crate) position: RawFrameId,
    pub(crate) backtrace: BacktraceFrame,
}

#[derive(Debug, Default)]
pub(crate) struct RustBacktraceStack {
    stack: TiVec<RawId, RustFrame>,
}

impl RustBacktraceStack {
    pub(crate) fn view(&mut self) -> RustBacktraceStackView {
        RustBacktraceStackView::new(self)
    }

    fn push(&mut self, frame: RustFrame) {
        self.stack.push(frame)
    }

    fn truncate(&mut self, len: RawId) {
        self.stack.truncate(len.0)
    }

    fn next_id(&self) -> RawId {
        self.stack.next_key()
    }
}

pub(crate) struct RustBacktraceStackView<'rt> {
    stack: &'rt mut RustBacktraceStack,
    boundary: RawId,
}

impl<'rt> RustBacktraceStackView<'rt> {
    pub(crate) fn new(stack: &'rt mut RustBacktraceStack) -> Self {
        RustBacktraceStackView {
            stack,
            boundary: RawId(0),
        }
    }

    pub(crate) fn view_over(&mut self) -> RustBacktraceStackView {
        let boundary = self.stack.next_id();

        RustBacktraceStackView {
            stack: self.stack,
            boundary,
        }
    }

    pub(crate) fn push(&mut self, frame: RustFrame) {
        self.stack.push(frame)
    }

    pub(crate) fn clear(&mut self) {
        self.stack.truncate(self.boundary)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &RustFrame> {
        self.stack.stack[self.boundary..].iter()
    }
}
