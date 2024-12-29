use repr::index::StackSlot;

#[derive(Debug, Default)]
pub struct PendingAdjustStack(Option<StackSlot>);

impl PendingAdjustStack {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn view(&mut self) -> PendingAdjustStackView<'_> {
        PendingAdjustStackView::new(self)
    }

    fn inner_state(&self) -> InnerState {
        InnerState { stack_slot: self.0 }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState { stack_slot } = state;

        self.0 = stack_slot;
    }
}

#[derive(Debug, Clone, Copy)]
struct InnerState {
    stack_slot: Option<StackSlot>,
}

#[derive(Debug)]
pub struct PendingAdjustStackView<'a> {
    store: &'a mut PendingAdjustStack,
    prev_state: InnerState,
}

impl<'a> PendingAdjustStackView<'a> {
    fn new(store: &'a mut PendingAdjustStack) -> Self {
        let prev_state = store.inner_state();

        PendingAdjustStackView { store, prev_state }
    }

    pub fn borrow(&mut self) -> &mut PendingAdjustStack {
        self.store
    }

    pub fn push(&mut self, slot: StackSlot) {
        let slot = match self.take() {
            Some(prev) => prev.min(slot),
            None => slot,
        };

        self.store.0 = Some(slot);
    }

    pub fn take(&mut self) -> Option<StackSlot> {
        self.store.0.take()
    }

    pub fn commit(self) {
        std::mem::forget(self)
    }
}

impl Drop for PendingAdjustStackView<'_> {
    fn drop(&mut self) {
        self.store.apply(self.prev_state)
    }
}
