use repr::index::UpvalueSlot;
use repr::tivec::TiVec;

#[derive(Debug, Default)]
pub struct Upvalues<'s> {
    store: TiVec<UpvalueSlot, &'s str>,
}

impl<'s> Upvalues<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn view(&mut self) -> UpvaluesView<'s, '_> {
        UpvaluesView::new(self)
    }

    fn get(&self, ident: &'s str) -> Option<UpvalueSlot> {
        // Perform linear search.
        // In general we expect the number of upvalues to be reasonably small:
        // somewhere in single digits.
        // Even for outliers more upvalues than 100 is incredibly unlikely
        // (unless one tries to stress-test, but that's a different story).
        // In this situation not only linear search will perform just fine
        // but we also get rid of headache of properly handling backlinks.
        self.store
            .iter_enumerated()
            .find_map(|(slot, &upvalue_name)| (upvalue_name == ident).then_some(slot))
    }

    fn register(&mut self, ident: &'s str) -> UpvalueSlot {
        if let Some(slot) = self.get(ident) {
            return slot;
        }

        self.store.push_and_get_key(ident)
    }

    fn inner_state(&self) -> InnerState {
        InnerState {
            store_len: self.store.next_key(),
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState { store_len } = state;

        self.store.truncate(store_len.into());
    }

    pub fn resolve(self) -> TiVec<UpvalueSlot, &'s str> {
        self.store
    }
}

#[derive(Debug, Clone, Copy)]
struct InnerState {
    store_len: UpvalueSlot,
}

#[derive(Debug)]
pub struct UpvaluesView<'s, 'origin> {
    upvalues: &'origin mut Upvalues<'s>,
    prev_state: InnerState,
}

impl<'s, 'origin> UpvaluesView<'s, 'origin> {
    pub fn new(upvalues: &'origin mut Upvalues<'s>) -> Self {
        let prev_state = upvalues.inner_state();

        UpvaluesView {
            upvalues,
            prev_state,
        }
    }

    pub fn borrow(&mut self) -> &mut Upvalues<'s> {
        self.upvalues
    }

    pub fn register(&mut self, ident: &'s str) -> UpvalueSlot {
        self.upvalues.register(ident)
    }

    pub fn commit(self) {
        std::mem::forget(self)
    }
}

impl<'s, 'origin> Drop for UpvaluesView<'s, 'origin> {
    fn drop(&mut self) {
        let state = self.prev_state;
        self.upvalues.apply(state);
    }
}
