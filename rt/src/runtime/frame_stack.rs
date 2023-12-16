use super::frame::Frame;

pub(crate) struct FrameStackView<'a, C> {
    frames: &'a mut Vec<Frame<C>>,
    protected_size: usize,
}

impl<'a, C> FrameStackView<'a, C> {
    pub(crate) fn new(frames: &'a mut Vec<Frame<C>>) -> Self {
        FrameStackView {
            frames,
            protected_size: 0,
        }
    }

    pub(crate) fn view(&mut self) -> FrameStackView<C> {
        let protected_size = self.frames.len();

        FrameStackView {
            frames: self.frames,
            protected_size,
        }
    }

    pub(crate) fn pop(&mut self) -> Option<Frame<C>> {
        if self.frames.len() <= self.protected_size {
            return None;
        }

        self.frames.pop()
    }

    pub(crate) fn push(&mut self, frame: Frame<C>) {
        self.frames.push(frame)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Frame<C>> {
        self.frames[self.protected_size..].iter()
    }

    pub(crate) fn clear(&mut self) {
        self.frames.truncate(self.protected_size)
    }
}
