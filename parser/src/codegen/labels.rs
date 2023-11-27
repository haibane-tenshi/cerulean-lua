use repr::index::InstrId;
use repr::opcode::OpCode;
use std::collections::HashMap;
use thiserror::Error;

use crate::codegen::function::FunctionView;
use crate::codegen::stack::CommitKind;
use crate::codegen::Ident;

#[derive(Debug, Clone, Copy)]
pub struct Label<'s> {
    pub name: Ident<'s>,
    pub target: InstrId,
}

#[derive(Debug)]
pub struct Labels<'s> {
    /// Position of known labels.
    up_jumps: Vec<Label<'s>>,

    /// To-be-resolved labels.
    down_jumps: HashMap<Ident<'s>, Vec<InstrId>>,

    /// First instruction marking the start of current lexical scope.
    ///
    /// It is impossible to perform a down-jump *into* inner scope -
    /// label is simply not visible from the jump position.
    /// Hence, any pending jumps that happen prior to the start of current
    /// scope cannot be attributed to newly encountered labels.
    scope: InstrId,

    /// Position of last known binding to a temporary.
    ///
    /// Lua states that goto cannot jump over newly introduced variables.
    ///
    /// With regard to this rule up-jumps are always safe:
    /// they can only remove variables from scope.
    /// However, down-jumps need to be carefully tracked.
    /// Any down-jumps that happen before this mark need to be considered ill-formed.
    last_binding: InstrId,
}

#[derive(Debug, Clone, Copy)]
struct InnerState {
    up_jumps: usize,
    last_down_jump: InstrId,
    scope: InstrId,
    last_binding: InstrId,
}

impl<'s> Labels<'s> {
    pub fn new(scope_start: InstrId) -> Self {
        Labels {
            up_jumps: Default::default(),
            down_jumps: Default::default(),
            scope: scope_start,
            last_binding: scope_start,
        }
    }

    pub fn view_expr<'a>(&'a mut self) -> LabelsView<'s, 'a> {
        let prev_state = self.inner_state();

        LabelsView {
            labels: self,
            prev_state,
        }
    }

    pub fn view_scope<'a>(&'a mut self, scope: InstrId) -> LabelsView<'s, 'a> {
        let prev_state = self.inner_state();

        self.scope = scope;
        LabelsView {
            labels: self,
            prev_state,
        }
    }

    fn inner_state(&self) -> InnerState {
        let up_jumps = self.up_jumps.len();
        let last_down_jump = self
            .down_jumps
            .values()
            .map(|locations| locations.last().copied().unwrap_or_default())
            .max()
            .unwrap_or_default();
        InnerState {
            up_jumps,
            last_down_jump,
            scope: self.scope,
            last_binding: self.last_binding,
        }
    }

    fn apply(&mut self, state: InnerState) {
        let InnerState {
            up_jumps,
            last_down_jump,
            scope,
            last_binding,
        } = state;

        self.up_jumps.truncate(up_jumps);
        self.scope = scope;
        self.last_binding = last_binding;

        for locations in self.down_jumps.values_mut() {
            let to_truncate = locations.partition_point(|location| *location <= last_down_jump);
            locations.truncate(to_truncate);
        }
    }

    pub fn is_resolved(&self) -> bool {
        self.down_jumps.is_empty()
    }
}

#[derive(Debug)]
pub struct LabelsView<'s, 'origin> {
    labels: &'origin mut Labels<'s>,
    prev_state: InnerState,
}

impl<'s, 'origin> LabelsView<'s, 'origin> {
    pub fn borrow<'a>(&'a mut self) -> &'a mut Labels<'s> {
        self.labels
    }

    pub fn get(&mut self, name: Ident<'s>) -> Option<InstrId> {
        self.labels
            .up_jumps
            .iter()
            .find_map(|label| (label.name == name).then_some(label.target))
    }

    pub fn push_last_binding(&mut self, marker: InstrId) {
        self.labels.last_binding = marker;
    }

    pub fn push_label(
        &mut self,
        label: Label<'s>,
        mut fun: FunctionView,
    ) -> Result<(), PushLabelError> {
        if self
            .labels
            .up_jumps
            .iter()
            .any(|other| other.name == label.name)
        {
            return Err(DuplicateLabelError.into());
        }

        self.labels.up_jumps.push(label);

        let Some(pending) = self.labels.down_jumps.get_mut(&label.name) else {
            return Ok(());
        };

        let scope = pending.partition_point(|instr| *instr < self.labels.scope);
        let valid = pending[scope..].partition_point(|instr| *instr < self.labels.last_binding);

        if !(scope..valid).is_empty() {
            return Err(IllFormedGotoError.into());
        }

        for instr_id in pending.drain(valid..) {
            if let Some(OpCode::Jump { offset } | OpCode::JumpIf { offset, .. }) =
                fun.get_mut(instr_id)
            {
                *offset = label.target - instr_id;
            }
        }

        if pending.is_empty() {
            self.labels.down_jumps.remove(&label.name);
        }

        fun.commit();

        Ok(())
    }

    pub fn goto(&mut self, name: Ident<'s>, instr_id: InstrId) -> OpCode {
        if let Some(target) = self.get(name) {
            let offset = instr_id - target;

            OpCode::Loop { offset }
        } else {
            self.labels
                .down_jumps
                .entry(name)
                .or_default()
                .push(instr_id);

            OpCode::Jump {
                offset: Default::default(),
            }
        }
    }

    pub fn commit(mut self, kind: CommitKind) {
        let LabelsView { labels, prev_state } = &mut self;

        if kind == CommitKind::Scope {
            let in_scope = labels
                .up_jumps
                .partition_point(|label| label.target < labels.scope);
            labels.up_jumps.truncate(in_scope);

            labels.last_binding = prev_state.last_binding;
        }

        labels.scope = prev_state.scope;

        std::mem::forget(self)
    }
}

impl<'s, 'origin> Drop for LabelsView<'s, 'origin> {
    fn drop(&mut self) {
        self.labels.apply(self.prev_state)
    }
}

#[derive(Debug, Error)]
#[error("label with the same identifier already exists")]
pub struct DuplicateLabelError;

#[derive(Debug, Error)]
#[error("attempted to goto jump over veriable declaration")]
pub struct IllFormedGotoError;

#[derive(Debug, Error)]
pub enum PushLabelError {
    #[error(transparent)]
    DuplicateLabel(#[from] DuplicateLabelError),
    #[error(transparent)]
    IllFormedGoto(#[from] IllFormedGotoError),
}
