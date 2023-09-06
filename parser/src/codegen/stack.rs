use super::fragment::EmitError;
use repr::index::StackSlot;
use repr::index_vec::{Index, IndexVec};
use repr::opcode::OpCode;
use std::collections::HashMap;
use std::ops::{Add, AddAssign, BitOr, Sub, SubAssign};
use thiserror::Error;

pub(crate) use repr::index::StackOffset;

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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
pub struct FragmentStackSlot(pub(crate) u32);

// impl FragmentStackSlot {
//     pub fn checked_sub(self, other: Self) -> Option<StackOffset> {
//         let inner = self.0.checked_sub(other.0)?;
//         Some(StackOffset(inner))
//     }
// }

impl AddAssign<StackOffset> for FragmentStackSlot {
    fn add_assign(&mut self, rhs: StackOffset) {
        self.0 += rhs.0
    }
}

impl Add<StackOffset> for FragmentStackSlot {
    type Output = Self;

    fn add(mut self, rhs: StackOffset) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign<u32> for FragmentStackSlot {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl Add<u32> for FragmentStackSlot {
    type Output = Self;

    fn add(mut self, rhs: u32) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign<StackOffset> for FragmentStackSlot {
    fn sub_assign(&mut self, rhs: StackOffset) {
        self.0 -= rhs.0;
    }
}

impl Sub<StackOffset> for FragmentStackSlot {
    type Output = Self;

    fn sub(mut self, rhs: StackOffset) -> Self::Output {
        self -= rhs;
        self
    }
}

impl Sub for FragmentStackSlot {
    type Output = StackOffset;

    fn sub(self, rhs: Self) -> Self::Output {
        StackOffset(self.0 - rhs.0)
    }
}

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
struct UnqualifiedStack<'s> {
    temporaries: IndexVec<GlobalStackSlot, Option<&'s str>>,
    backlinks: Backlinks<'s>,
    variadic: bool,
}

impl<'s> UnqualifiedStack<'s> {
    fn len(&self) -> GlobalStackSlot {
        self.temporaries.len()
    }

    fn push(&mut self, name: Option<&'s str>) -> Result<GlobalStackSlot, PushError> {
        if self.variadic {
            return Err(VariadicStackError.into());
        }

        let slot = self.temporaries.push(name)?;
        if let Some(name) = name {
            self.backlinks.add(slot, name);
        }

        Ok(slot)
    }

    fn pop(&mut self) -> Result<(), PopError> {
        if self.variadic {
            return Err(VariadicStackError.into());
        }

        if let Some(name) = self.temporaries.pop().ok_or(BoundaryViolationError)? {
            self.backlinks.pop(name)
        }

        Ok(())
    }

    fn make_variadic(&mut self) {
        self.variadic = true;
    }

    fn is_variadic(&self) -> bool {
        self.variadic
    }

    fn lookup(&self, name: &'s str) -> Option<GlobalStackSlot> {
        self.backlinks.get(name)
    }

    fn need_adjustment_to(&self, slot: GlobalStackSlot) -> bool {
        self.len() != slot || self.variadic
    }

    fn adjust_to(&mut self, slot: GlobalStackSlot) -> bool {
        use std::cmp::Ordering;

        let r = match Ord::cmp(&self.len(), &slot) {
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
        };

        let r = self.variadic || r;
        self.variadic = false;

        r
    }
}

#[derive(Debug, Default)]
struct StackFrame<'s> {
    stack: UnqualifiedStack<'s>,
    frame_base: GlobalStackSlot,
}

impl<'s> StackFrame<'s> {
    fn frame_to_global(&self, slot: StackSlot) -> GlobalStackSlot {
        self.frame_base + (slot - StackSlot::default())
    }

    fn global_to_frame(&self, slot: GlobalStackSlot) -> Option<StackSlot> {
        let offset = slot.checked_sub(self.frame_base)?;
        let r = StackSlot::default() + offset;

        Some(r)
    }

    fn len(&self) -> StackSlot {
        self.global_to_frame(self.stack.len()).unwrap()
    }

    fn push(&mut self, name: Option<&'s str>) -> Result<StackSlot, PushError> {
        self.stack
            .push(name)
            .map(|slot| self.global_to_frame(slot).unwrap())
    }

    fn pop(&mut self) -> Result<(), PopError> {
        if self.stack.len() <= self.frame_base {
            return Err(BoundaryViolationError.into());
        }

        self.stack.pop()
    }

    fn make_variadic(&mut self) {
        self.stack.make_variadic()
    }

    fn is_variadic(&self) -> bool {
        self.stack.is_variadic()
    }

    fn lookup(&self, name: &str) -> NameLookup {
        match self.stack.lookup(name) {
            Some(slot) => match self.global_to_frame(slot) {
                Some(slot) => NameLookup::Local(slot),
                None => NameLookup::Upvalue,
            },
            None => NameLookup::Global,
        }
    }

    fn need_adjustment_to(&self, slot: StackSlot) -> bool {
        self.stack.need_adjustment_to(self.frame_to_global(slot))
    }

    fn adjust_to(&mut self, slot: StackSlot) -> bool {
        self.stack.adjust_to(self.frame_to_global(slot))
    }
}

#[derive(Debug, Default)]
pub struct Stack<'s> {
    stack: StackFrame<'s>,
    boundary: StackSlot,
}

impl<'s> Stack<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn view(&mut self) -> StackView<'s, '_> {
        StackView::new_block(self)
    }

    pub fn view_at(&mut self, slot: FragmentStackSlot) -> StackView<'s, '_> {
        StackView::new_block_at(self, slot)
    }

    pub fn frame(&mut self) -> StackView<'s, '_> {
        StackView::new_frame(self)
    }

    fn fragment_to_frame(&self, slot: FragmentStackSlot) -> StackSlot {
        self.boundary + (slot - FragmentStackSlot::default())
    }

    fn frame_to_fragment(&self, slot: StackSlot) -> Option<FragmentStackSlot> {
        let offset = slot.checked_sub(self.boundary)?;
        let r = FragmentStackSlot::default() + offset;

        Some(r)
    }

    fn len(&self) -> FragmentStackSlot {
        self.frame_to_fragment(self.stack.len()).unwrap()
    }

    fn push(&mut self, name: Option<&'s str>) -> Result<FragmentStackSlot, PushError> {
        self.stack
            .push(name)
            .map(|slot| self.frame_to_fragment(slot).unwrap())
    }

    fn pop(&mut self) -> Result<(), PopError> {
        if self.stack.len() <= self.boundary {
            return Err(BoundaryViolationError.into());
        }

        self.stack.pop()
    }

    fn make_variadic(&mut self) {
        self.stack.make_variadic()
    }

    fn is_variadic(&self) -> bool {
        self.stack.is_variadic()
    }

    fn lookup(&self, name: &'s str) -> NameLookup {
        self.stack.lookup(name)
    }

    fn need_adjustment_to(&self, slot: FragmentStackSlot) -> bool {
        self.stack.need_adjustment_to(self.fragment_to_frame(slot))
    }

    fn adjust_to(&mut self, slot: FragmentStackSlot) -> bool {
        self.stack.adjust_to(self.fragment_to_frame(slot))
    }

    fn iter(&self) -> impl Iterator<Item = &Option<&'s str>> {
        let start = self.stack.frame_to_global(self.boundary);

        self.stack.stack.temporaries.range(start..).iter()
    }

    fn remove_idents(&mut self) {
        let start = self.stack.frame_to_global(self.boundary);

        for slot in self.stack.stack.temporaries.range_mut(start..).iter_mut() {
            if let Some(name) = slot.take() {
                self.stack.stack.backlinks.pop(name)
            }
        }
    }

    fn inner_state(&self) -> InnerState {
        InnerState {
            frame_base: self.stack.frame_base,
            variadic: self.stack.stack.variadic,
            boundary: self.boundary,
            top: self.len(),
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState {
            frame_base,
            variadic,
            boundary,
            top,
        } = state;

        // Reset any names that were assigned as part of current block.
        self.remove_idents();

        // Order matters: adjust_to resets stack's variadic flag.
        self.stack.frame_base = frame_base;
        self.boundary = boundary;
        self.adjust_to(top);
        self.stack.stack.variadic = variadic;
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

#[derive(Debug, Copy, Clone)]
struct InnerState {
    variadic: bool,
    frame_base: GlobalStackSlot,
    boundary: StackSlot,
    top: FragmentStackSlot,
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
    inner_state: InnerState,
}

impl<'s, 'origin> StackView<'s, 'origin> {
    pub fn new_block<'a>(stack: &'a mut Stack<'s>) -> StackView<'s, 'a> {
        StackView::new_block_at(stack, stack.len())
    }

    pub fn new_block_at<'a>(
        stack: &'a mut Stack<'s>,
        slot: FragmentStackSlot,
    ) -> StackView<'s, 'a> {
        let inner_state = stack.inner_state();

        stack.boundary = stack.fragment_to_frame(slot);

        assert_eq!(
            stack.iter().find(|t| t.is_some()),
            None,
            "transferred portion of stack cannot contain named temporaries"
        );

        StackView { stack, inner_state }
    }

    pub fn new_frame<'a>(stack: &'a mut Stack<'s>) -> StackView<'s, 'a> {
        let inner_state = stack.inner_state();
        let frame_base = stack.stack.stack.temporaries.len();

        stack.stack.stack.variadic = false;
        stack.stack.frame_base = frame_base;
        stack.boundary = StackSlot(0);

        StackView { stack, inner_state }
    }

    // pub fn new_view(&mut self) -> StackView<'s, '_> {
    //     StackView::new_block(self.borrow())
    // }

    pub fn borrow(&mut self) -> &mut Stack<'s> {
        self.stack
    }

    pub fn fragment_to_frame(&self, slot: FragmentStackSlot) -> StackSlot {
        self.stack.fragment_to_frame(slot)
    }

    // pub fn is_variadic(&self) -> bool {
    //     self.stack.is_variadic()
    // }

    pub fn state(&self) -> StackState {
        if self.stack.is_variadic() {
            StackState::Variadic
        } else {
            StackState::Finite(self.stack.fragment_to_frame(self.stack.len()))
        }
    }

    pub fn try_apply(&mut self, state: StackState) -> Result<(), BoundaryViolationError> {
        match state {
            StackState::Variadic => {
                self.make_variadic();
            }
            StackState::Finite(height) => {
                let height = self
                    .stack
                    .frame_to_fragment(height)
                    .ok_or(BoundaryViolationError)?;
                self.adjust_to(height);
            }
        }

        Ok(())
    }

    pub fn apply(&mut self, state: StackState) {
        self.try_apply(state).unwrap()
    }

    pub fn len(&self) -> FragmentStackSlot {
        self.stack.len()
    }

    pub fn try_push(&mut self, name: Option<&'s str>) -> Result<FragmentStackSlot, PushError> {
        self.stack.push(name)
    }

    pub fn push(&mut self, name: Option<&'s str>) -> FragmentStackSlot {
        self.try_push(name).unwrap()
    }

    pub fn try_pop(&mut self) -> Result<(), PopError> {
        self.stack.pop()
    }

    pub fn pop(&mut self) {
        self.try_pop().unwrap();
    }

    pub fn emit(&mut self, opcode: &OpCode) -> Result<(), EmitError> {
        match opcode {
            // This opcode never returns, so stack manipulation is irrelevant.
            // Any opcodes after this one are either unreachable,
            // or in case we get there through jumps,
            // presumably this instruction should be the end of the fragment.
            // Anyway, stack space must be manually brought into consistent state.
            OpCode::Return(_) => (),
            // This opcode never returns, however it grabs the top value as panic message.
            OpCode::Panic => {
                self.try_pop()?;
            }
            OpCode::Invoke(slot) => {
                let height = self
                    .stack
                    .frame_to_fragment(*slot)
                    .ok_or(BoundaryViolationError)?;

                if height > self.len() {
                    return Err(EmitError::InvokeOutsideStackBoundary);
                }

                // Stack space at `slot` and above is consumed during invocation.
                // Function returns are always variadic.
                self.adjust_to(height);
                self.make_variadic();
            }
            OpCode::LoadConstant(_) | OpCode::LoadStack(_) | OpCode::TabCreate => {
                self.try_push(None)?;
            }
            OpCode::LoadVariadic => {
                self.make_variadic();
            }
            OpCode::StoreStack(_) => {
                self.try_pop()?;
            }
            OpCode::AdjustStack(slot) => {
                let height = self
                    .stack
                    .frame_to_fragment(*slot)
                    .ok_or(BoundaryViolationError)?;
                self.adjust_to(height);
            }
            OpCode::UnaOp(_) => {
                self.try_pop()?;
                self.try_push(None)?;
            }
            OpCode::BinOp(_) => {
                self.try_pop()?;
                self.try_pop()?;
                self.try_push(None)?;
            }
            OpCode::Jump { .. }
            | OpCode::JumpIf { .. }
            | OpCode::Loop { .. }
            | OpCode::LoopIf { .. } => (),
            OpCode::TabGet => {
                self.try_pop()?;
                self.try_pop()?;
                self.try_push(None)?;
            }
            OpCode::TabSet => {
                self.try_pop()?;
                self.try_pop()?;
                self.try_pop()?;
            }
        }

        Ok(())
    }

    pub fn make_variadic(&mut self) {
        self.stack.make_variadic();
    }

    pub fn need_adjustment_to(&self, slot: FragmentStackSlot) -> bool {
        self.stack.need_adjustment_to(slot)
    }

    pub fn adjust_to(&mut self, height: FragmentStackSlot) -> bool {
        self.stack.adjust_to(height)
    }

    pub fn lookup(&self, name: &'s str) -> NameLookup {
        self.stack.lookup(name)
    }

    pub fn commit(self, kind: CommitKind) {
        match kind {
            CommitKind::Decl => (),
            CommitKind::Expr => {
                self.stack.remove_idents();
            }
            CommitKind::Scope => {
                self.stack.adjust_to(FragmentStackSlot(0));
            }
        }

        let InnerState {
            frame_base,
            boundary,
            ..
        } = self.inner_state;

        self.stack.stack.frame_base = frame_base;
        self.stack.boundary = boundary;

        std::mem::forget(self)
    }
}

impl<'s, 'origin> Drop for StackView<'s, 'origin> {
    fn drop(&mut self) {
        self.stack.apply(self.inner_state)
    }
}

#[derive(Debug)]
pub enum CommitKind {
    Scope,
    Expr,
    Decl,
}

/// There are named temporaries above boundary.
#[derive(Debug, Error)]
#[error("attempt to pass named temporaries into new frame")]
pub struct PassingNamedTemporariesError;

#[derive(Debug, Error)]
#[error("failed to capture part of stack in new frame")]
pub enum NewBlockAtError {
    #[error("tried to capture temporaries below current frame's boundary")]
    Boundary(#[from] BoundaryViolationError),
    #[error("cannot capture named temporaries in the new frame")]
    PassingNamed(#[from] PassingNamedTemporariesError),
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
