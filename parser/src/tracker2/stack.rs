use repr::index_vec::{Index, IndexVec};
use repr::opcode::StackSlot;
use std::collections::HashMap;
use std::ops::{Add, AddAssign, BitOr, Sub, SubAssign};
use thiserror::Error;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
struct GlobalStackSlot(pub u32);

impl GlobalStackSlot {
    fn checked_sub(self, rhs: Self) -> Option<StackOffset> {
        let inner = self.0.checked_sub(rhs.0)?;
        Some(StackOffset(inner))
    }
}

#[derive(Debug, Error)]
#[error("number of temporaries on the stack exceeded the limit")]
pub struct StackOverflowError;

impl Index for GlobalStackSlot {
    type Error = StackOverflowError;
    const MAX: Self = GlobalStackSlot(u32::MAX);

    fn try_from(val: usize) -> Result<Self, Self::Error> {
        let inner = val.try_into().map_err(|_| StackOverflowError)?;
        Ok(GlobalStackSlot(inner))
    }

    fn into(self) -> usize {
        self.0.try_into().expect("u32 should fit into usize")
    }
}

impl AddAssign<StackOffset> for GlobalStackSlot {
    fn add_assign(&mut self, rhs: StackOffset) {
        self.0 += rhs.0;
    }
}

impl Add<StackOffset> for GlobalStackSlot {
    type Output = Self;

    fn add(mut self, rhs: StackOffset) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<StackOffset> for GlobalStackSlot {
    fn sub_assign(&mut self, rhs: StackOffset) {
        self.0 -= rhs.0;
    }
}

impl Sub<StackOffset> for GlobalStackSlot {
    type Output = Self;

    fn sub(mut self, rhs: StackOffset) -> Self::Output {
        self -= rhs;
        self
    }
}

impl Sub for GlobalStackSlot {
    type Output = StackOffset;

    fn sub(self, rhs: Self) -> Self::Output {
        let inner = self.0 - rhs.0;
        StackOffset(inner)
    }
}

pub(crate) use repr::opcode::StackOffset;

#[derive(Debug, Copy, Clone)]
pub enum StackState {
    Variadic,
    Finite(StackSlot),
}

impl BitOr for StackState {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackState::Finite(lhs), StackState::Finite(rhs)) if lhs == rhs => {
                StackState::Finite(lhs)
            }
            _ => StackState::Variadic,
        }
    }
}

#[derive(Debug, Default)]
pub struct Stack<'s> {
    temporaries: IndexVec<GlobalStackSlot, Option<&'s str>>,
    variadic: bool,
    backlinks: Backlinks<'s>,
}

impl<'s> Stack<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    fn len(&self) -> GlobalStackSlot {
        self.temporaries.len()
    }

    fn push(&mut self) -> Result<GlobalStackSlot, StackOverflowError> {
        self.temporaries.push(None)
    }

    fn pop(&mut self) -> Option<()> {
        let name = self.temporaries.pop()?;

        if let Some(name) = name {
            self.backlinks.pop(name)
        }

        Some(())
    }

    fn give_name(&mut self, slot: GlobalStackSlot, name: &'s str) -> Result<(), GiveNameError> {
        let Some(place) = self.temporaries.get_mut(slot) else {
            return Err(GiveNameError::MissingTemporary)
        };

        if place.is_some() {
            return Err(GiveNameError::NameAlias);
        }

        *place = Some(name);
        self.backlinks.add(slot, name);

        Ok(())
    }

    fn lookup(&self, name: &'s str) -> Option<GlobalStackSlot> {
        self.backlinks.get(name)
    }

    fn adjust_to(&mut self, slot: GlobalStackSlot) -> bool {
        use std::cmp::Ordering;

        self.variadic = false;

        match Ord::cmp(&self.len(), &slot) {
            Ordering::Equal => false,
            Ordering::Less => {
                let count = Index::into(slot) - Index::into(self.len());
                let iter = std::iter::repeat(None).take(count);
                self.temporaries.extend(iter).unwrap();

                true
            }
            Ordering::Greater => {
                for name in self.temporaries.drain(slot..).flatten().rev() {
                    self.backlinks.pop(name);
                }

                true
            }
        }
    }
}

#[derive(Debug, Default)]
struct Backlinks<'s>(HashMap<&'s str, Vec<GlobalStackSlot>>);

impl<'s> Backlinks<'s> {
    fn add(&mut self, slot: GlobalStackSlot, name: &'s str) {
        self.0.entry(name).or_default().push(slot);
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

    fn get(&self, name: &'s str) -> Option<GlobalStackSlot> {
        self.0.get(name)?.last().copied()
    }
}

#[derive(Debug)]
pub enum NameLookup {
    Local(StackSlot),
    Upvalue,
    Global,
}

#[derive(Debug)]
pub struct StackView<'s, 'origin> {
    stack: &'origin mut Stack<'s>,
    variadic: bool,
    boundary: GlobalStackSlot,
    frame_base: GlobalStackSlot,
}

impl<'s, 'origin> StackView<'s, 'origin> {
    pub fn new(stack: &'origin mut Stack<'s>) -> Self {
        let variadic = stack.variadic;
        let boundary = stack.len();
        let frame_base = stack.len();

        StackView {
            stack,
            variadic,
            boundary,
            frame_base,
        }
    }

    pub fn new_block(&mut self) -> StackView<'s, '_> {
        let variadic = self.stack.variadic;
        let boundary = self.stack.len();
        let frame_base = self.frame_base;

        StackView {
            stack: self.stack,
            variadic,
            boundary,
            frame_base,
        }
    }

    pub fn new_frame(&mut self) -> StackView<'s, '_> {
        let boundary = self.stack.len();
        let frame_base = boundary;

        StackView {
            stack: self.stack,
            variadic: false,
            boundary,
            frame_base,
        }
    }

    fn len(&self) -> StackOffset {
        self.stack.len() - self.frame_base
    }

    fn slot_to_global(&self, slot: StackSlot) -> GlobalStackSlot {
        self.frame_base + (slot - StackSlot::default())
    }

    fn slot_to_frame(&self, slot: GlobalStackSlot) -> Option<StackSlot> {
        let offset = self.frame_base.checked_sub(slot)?;
        let r = StackSlot::default() + offset;

        Some(r)
    }

    pub(super) fn raw_top(&self) -> StackSlot {
        StackSlot::default() + self.len()
    }

    pub fn state(&self) -> StackState {
        if self.stack.variadic {
            StackState::Variadic
        } else {
            StackState::Finite(self.raw_top())
        }
    }

    pub fn apply(&mut self, state: StackState) -> Result<(), BoundaryViolationError> {
        match state {
            StackState::Variadic => {
                self.make_variadic();
            }
            StackState::Finite(height) => {
                self.adjust_to(height)?;
            }
        }

        Ok(())
    }

    pub fn top(&self) -> Result<StackSlot, VariadicStackError> {
        match self.state() {
            StackState::Finite(top) => Ok(top),
            StackState::Variadic => Err(VariadicStackError),
        }
    }

    pub fn boundary(&self) -> StackSlot {
        self.slot_to_frame(self.boundary).unwrap()
    }

    pub fn push(&mut self) -> Result<StackSlot, PushError> {
        if self.variadic {
            return Err(VariadicStackError.into());
        }

        let slot = self.stack.push()?;
        let r = StackSlot::default() + (slot - self.frame_base);

        Ok(r)
    }

    pub fn pop(&mut self) -> Result<(), PopError> {
        if self.variadic {
            return Err(VariadicStackError.into());
        }

        if self.stack.len() == self.boundary {
            return Err(BoundaryViolationError.into());
        }

        self.stack
            .pop()
            .expect("there should be boundary violation error for empty stack");

        Ok(())
    }

    pub fn make_variadic(&mut self) {
        self.stack.variadic = true;
    }

    pub fn adjust_to(&mut self, height: StackSlot) -> Result<bool, BoundaryViolationError> {
        let height = self.slot_to_global(height);
        if height < self.boundary {
            return Err(BoundaryViolationError);
        };

        let r = self.stack.adjust_to(height);

        Ok(r)
    }

    pub fn give_name(&mut self, slot: StackSlot, name: &'s str) -> Result<(), GiveNameError> {
        let slot = self.slot_to_global(slot);
        if slot < self.boundary {
            return Err(BoundaryViolationError.into());
        };

        self.stack.give_name(slot, name)
    }

    pub fn lookup(&self, name: &'s str) -> NameLookup {
        match self.stack.lookup(name) {
            Some(slot) => match self.slot_to_frame(slot) {
                Some(slot) => NameLookup::Local(slot),
                None => NameLookup::Upvalue,
            },
            None => NameLookup::Global,
        }
    }

    pub fn commit(self, preserve_idents: bool) {
        if !preserve_idents {
            for slot in self.stack.temporaries.range_mut(self.boundary..).iter_mut() {
                if let Some(ident) = slot.take() {
                    self.stack.backlinks.pop(ident);
                }
            }
        }

        std::mem::forget(self)
    }
}

impl<'s, 'origin> Drop for StackView<'s, 'origin> {
    fn drop(&mut self) {
        self.stack.adjust_to(self.boundary);
        self.stack.variadic = self.variadic;
    }
}

#[derive(Debug, Error)]
#[error("cannot manipulate stack when number of temporaries is statically unknown")]
pub struct VariadicStackError;

#[derive(Debug, Error)]
#[error("attempt to modify stack beyond protected boundary")]
pub struct BoundaryViolationError;

#[derive(Debug, Error)]
pub enum PushError {
    #[error("attempt to push value onto variadic stack")]
    Variadic(#[from] VariadicStackError),

    #[error("too many temporaries on the stack")]
    Overflow(#[from] StackOverflowError),
}

#[derive(Debug, Error)]
pub enum PopError {
    #[error("attempt to pop value from variadic stack")]
    Variadic(#[from] VariadicStackError),

    #[error("locally accessible stack is empty")]
    Boundary(#[from] BoundaryViolationError),
}

#[derive(Debug, Error)]
pub enum GiveNameError {
    #[error("cannot attach names to temporaries beyond local stack")]
    Boundary(#[from] BoundaryViolationError),

    #[error("stack slot is unoccupied")]
    MissingTemporary,

    #[error("temporary already have a name")]
    NameAlias,
}
