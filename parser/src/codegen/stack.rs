use repr::index::StackSlot;
use repr::opcode::OpCode;
use repr::tivec::TiVec;
use std::collections::HashMap;
use std::ops::{Add, AddAssign, BitOr, Sub, SubAssign};

use super::Ident;

pub(crate) use repr::index::StackOffset;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
struct GlobalStackSlot(usize);

impl GlobalStackSlot {
    fn checked_sub(self, rhs: Self) -> Option<StackOffset> {
        let inner = self.0.checked_sub(rhs.0)?;
        Some(StackOffset(inner))
    }
}

impl From<usize> for GlobalStackSlot {
    fn from(value: usize) -> Self {
        GlobalStackSlot(value)
    }
}

impl From<GlobalStackSlot> for usize {
    fn from(value: GlobalStackSlot) -> Self {
        value.0
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
pub struct FragmentStackSlot(pub(crate) usize);

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

impl AddAssign<usize> for FragmentStackSlot {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl Add<usize> for FragmentStackSlot {
    type Output = Self;

    fn add(mut self, rhs: usize) -> Self::Output {
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
    temporaries: TiVec<GlobalStackSlot, Option<Ident<'s>>>,
    backlinks: Backlinks<'s>,
    variadic: bool,
}

impl<'s> UnqualifiedStack<'s> {
    fn len(&self) -> GlobalStackSlot {
        self.temporaries.next_key()
    }

    fn push(&mut self, name: Option<Ident<'s>>) -> GlobalStackSlot {
        assert!(
            !self.variadic,
            "cannot push values onto stack of variadic length"
        );

        let slot = self.temporaries.push_and_get_key(name);
        if let Some(name) = name {
            self.backlinks.add(slot, name);
        }

        slot
    }

    fn pop(&mut self) {
        assert!(
            !self.variadic,
            "cannot pop values from stack of variadic length"
        );

        let maybe_name = self
            .temporaries
            .pop()
            .expect("stack should contain at least one value");

        if let Some(name) = maybe_name {
            self.backlinks.pop(name);
        }
    }

    fn make_variadic(&mut self) {
        self.variadic = true;
    }

    fn is_variadic(&self) -> bool {
        self.variadic
    }

    fn lookup(&self, name: Ident<'s>) -> Option<GlobalStackSlot> {
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
                let count = slot - self.len();
                self.temporaries
                    .extend(std::iter::repeat(None).take(count.into()));

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
        self.frame_base + (slot - StackSlot(0))
    }

    fn global_to_frame(&self, slot: GlobalStackSlot) -> Option<StackSlot> {
        let offset = slot.checked_sub(self.frame_base)?;
        let r = StackSlot(0) + offset;

        Some(r)
    }

    fn len(&self) -> StackSlot {
        self.global_to_frame(self.stack.len()).unwrap()
    }

    fn push(&mut self, name: Option<Ident<'s>>) -> StackSlot {
        let slot = self.stack.push(name);
        self.global_to_frame(slot).unwrap()
    }

    fn pop(&mut self) {
        assert!(
            self.stack.len() > self.frame_base,
            "current frame has no values on stack"
        );

        self.stack.pop()
    }

    fn make_variadic(&mut self) {
        self.stack.make_variadic()
    }

    fn is_variadic(&self) -> bool {
        self.stack.is_variadic()
    }

    fn lookup(&self, name: Ident<'s>) -> NameLookup {
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

    fn push(&mut self, name: Option<Ident<'s>>) -> FragmentStackSlot {
        let slot = self.stack.push(name);
        self.frame_to_fragment(slot).unwrap()
    }

    fn pop(&mut self) {
        assert!(
            self.stack.len() > self.boundary,
            "cannot pop values across fragment boundary"
        );

        self.stack.pop()
    }

    fn make_variadic(&mut self) {
        self.stack.make_variadic()
    }

    fn is_variadic(&self) -> bool {
        self.stack.is_variadic()
    }

    fn lookup(&self, name: Ident<'s>) -> NameLookup {
        self.stack.lookup(name)
    }

    fn need_adjustment_to(&self, slot: FragmentStackSlot) -> bool {
        self.stack.need_adjustment_to(self.fragment_to_frame(slot))
    }

    fn adjust_to(&mut self, slot: FragmentStackSlot) -> bool {
        self.stack.adjust_to(self.fragment_to_frame(slot))
    }

    fn iter(&self) -> impl Iterator<Item = &Option<Ident<'s>>> {
        let start = self.stack.frame_to_global(self.boundary);

        self.stack
            .stack
            .temporaries
            .get(start..)
            .unwrap_or_default()
            .iter()
    }

    fn remove_idents(&mut self) {
        let start = self.stack.frame_to_global(self.boundary);

        for slot in self
            .stack
            .stack
            .temporaries
            .get_mut(start..)
            .unwrap_or_default()
            .iter_mut()
        {
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
struct Backlinks<'s>(HashMap<Ident<'s>, Vec<GlobalStackSlot>>);

impl<'s> Backlinks<'s> {
    fn add(&mut self, slot: GlobalStackSlot, name: Ident<'s>) {
        self.0.entry(name).or_default().push(slot);
    }

    fn pop(&mut self, name: Ident<'s>) {
        let Some(backlink) = self.0.get_mut(&name) else {
            return;
        };

        backlink.pop();

        if backlink.is_empty() {
            self.0.remove(&name);
        }
    }

    fn get(&self, name: Ident<'s>) -> Option<GlobalStackSlot> {
        self.0.get(&name)?.last().copied()
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

impl<'s> StackView<'s, '_> {
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
        let frame_base = stack.stack.stack.temporaries.next_key();

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

    pub fn apply(&mut self, state: StackState) {
        match state {
            StackState::Variadic => {
                self.make_variadic();
            }
            StackState::Finite(height) => {
                let height = self
                    .stack
                    .frame_to_fragment(height)
                    .expect("cannot set stack length to value below fragment boundary");
                self.adjust_to(height);
            }
        }
    }

    pub fn len(&self) -> FragmentStackSlot {
        self.stack.len()
    }

    pub fn push(&mut self, name: Option<Ident<'s>>) -> FragmentStackSlot {
        self.stack.push(name)
    }

    pub fn pop(&mut self) {
        self.stack.pop()
    }

    pub fn emit(&mut self, opcode: &OpCode) {
        match opcode {
            // This opcode never returns, so stack manipulation is irrelevant.
            // Any opcodes after this one are either unreachable,
            // or in case we get there through jumps,
            // presumably this instruction should be the end of the fragment.
            // Anyway, stack space must be manually brought into consistent state.
            OpCode::Return(_) => (),
            // This opcode never returns, however it grabs the top value as panic message.
            OpCode::Panic => {
                // self.pop();
            }
            OpCode::Invoke(slot) => {
                let height = self
                    .stack
                    .frame_to_fragment(*slot)
                    .expect("cannot pass values below fragment boundary as arguments to invoke");

                assert!(height <= self.len(), "cannot invoke past stack length");

                // Stack space at `slot` and above is consumed during invocation.
                // Function returns are always variadic.
                self.adjust_to(height);
                self.make_variadic();
            }
            OpCode::LoadConstant(_)
            | OpCode::LoadStack(_)
            | OpCode::LoadUpvalue(_)
            | OpCode::TabCreate
            | OpCode::MakeClosure(_) => {
                self.push(None);
            }
            OpCode::LoadVariadic => {
                self.make_variadic();
            }
            OpCode::StoreStack(_) | OpCode::StoreUpvalue(_) | OpCode::JumpIf { .. } => {
                self.pop();
            }
            OpCode::AdjustStack(slot) => {
                let height = self
                    .stack
                    .frame_to_fragment(*slot)
                    .expect("cannot adjust stack length to value below fragment boundary");
                self.adjust_to(height);
            }
            OpCode::UnaOp(_) => {
                self.pop();
                self.push(None);
            }
            OpCode::BinOp(_) => {
                self.pop();
                self.pop();
                self.push(None);
            }
            OpCode::Jump { .. } | OpCode::Loop { .. } => (),
            OpCode::GetIndex => {
                self.pop();
                self.pop();
                self.push(None);
            }
            OpCode::SetIndex => {
                self.pop();
                self.pop();
                self.pop();
            }
        }
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

    pub fn lookup(&self, name: Ident<'s>) -> NameLookup {
        self.stack.lookup(name)
    }

    pub fn commit(self, kind: CommitKind) {
        match kind {
            CommitKind::Decl => (),
            CommitKind::Expr | CommitKind::Scope => {
                self.stack.remove_idents();
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

impl Drop for StackView<'_, '_> {
    fn drop(&mut self) {
        self.stack.apply(self.inner_state)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommitKind {
    Scope,
    Expr,
    Decl,
}
