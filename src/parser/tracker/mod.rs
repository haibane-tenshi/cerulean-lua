mod const_;
mod function;
mod opcode;
mod stack;

use thiserror::Error;

use crate::index_vec::IndexVec;
use crate::opcode::{Chunk, ConstId, Function, FunctionId, StackSlot};
use crate::value::Literal;

use const_::{ConstTracker, ExceededConstIdError};
use function::{EmitError, FunctionTracker};
use opcode::{ExceededInstrIdError, OpCodeTracker};
use stack::{StackStateError, StackTracker};

#[derive(Debug, Error)]
#[error("exceeded indexing capacity of function ids")]
pub struct ExceededFnIdError;

#[derive(Debug, Error)]
pub enum FinishFnError {
    #[error("no unresolved functions are present")]
    NoActiveFn,

    #[error("failed to resolve function")]
    FnResolve(#[from] EmitError),

    #[error("failed to generate function index")]
    FnId(#[from] ExceededFnIdError),
}

#[derive(Debug)]
enum FunctionSlot<'s> {
    Resolved(Function),
    InProgress(FunctionTracker<'s>),
}

impl<'s> FunctionSlot<'s> {
    pub fn resolved(self) -> Result<Function, UnresolvedFnError> {
        match self {
            FunctionSlot::Resolved(f) => Ok(f),
            FunctionSlot::InProgress(_) => Err(UnresolvedFnError),
        }
    }

    fn is_in_progress(&self) -> bool {
        matches!(self, FunctionSlot::InProgress(_))
    }

    pub fn resolve(&mut self, height: u32) -> Result<(), EmitError> {
        let mut dummy = FunctionSlot::default();

        std::mem::swap(self, &mut dummy);

        let (mut dummy, r) = match dummy {
            FunctionSlot::InProgress(tracker) => match tracker.resolve(height) {
                Ok(fun) => (FunctionSlot::Resolved(fun), Ok(())),
                Err((e, tracker)) => (FunctionSlot::InProgress(tracker), Err(e)),
            },
            r @ FunctionSlot::Resolved(_) => (r, Ok(())),
        };

        std::mem::swap(self, &mut dummy);

        r
    }
}

impl<'s> Default for FunctionSlot<'s> {
    fn default() -> Self {
        FunctionSlot::InProgress(Default::default())
    }
}

#[derive(Debug, Error)]
#[error("encountered unresolved function in chunk")]
struct UnresolvedFnError;

#[derive(Debug, Default)]
pub(super) struct ChunkTracker<'s> {
    functions: IndexVec<FunctionId, FunctionSlot<'s>>,
    constants: ConstTracker,
    // Invariant: points at the latest in-progress function slot.
    current: Option<FunctionId>,
}

impl<'s> ChunkTracker<'s> {
    pub fn empty() -> Self {
        Default::default()
    }

    pub fn current(&self) -> Option<&FunctionTracker> {
        let r = match self.functions.get(self.current?)? {
            FunctionSlot::InProgress(tracker) => tracker,
            FunctionSlot::Resolved(_) => unreachable!(),
        };

        Some(r)
    }

    pub fn current_mut(&mut self) -> Option<&mut FunctionTracker> {
        let r = match self.functions.get_mut(self.current?)? {
            FunctionSlot::InProgress(tracker) => tracker,
            FunctionSlot::Resolved(_) => unreachable!(),
        };

        Some(r)
    }

    pub fn resolve(self) -> Result<Chunk, UnresolvedFnError> {
        let ChunkTracker {
            functions,
            constants,
            current: _,
        } = self;

        let functions = functions
            .into_iter()
            .map(FunctionSlot::resolved)
            .collect::<Result<_, _>>()?;
        let constants = constants.resolve();

        let r = Chunk {
            functions,
            constants,
        };

        Ok(r)
    }

    pub fn insert_literal(&mut self, value: Literal) -> Result<ConstId, ExceededConstIdError> {
        self.constants.insert(value)
    }

    pub fn lookup_local(&self, ident: &str) -> Option<StackSlot> {
        self.current()?.lookup_local(ident)
    }

    pub fn start_fn(&mut self) -> Result<FunctionId, ExceededFnIdError> {
        self.functions
            .push(Default::default())
            .map_err(|_| ExceededFnIdError)
    }

    pub fn finish_fn(&mut self, height: u32) -> Result<FunctionId, FinishFnError> {
        let fn_id = self.current.ok_or(FinishFnError::NoActiveFn)?;

        let slot = self.functions.get_mut(fn_id).unwrap();
        slot.resolve(height)?;

        // Slot got resolved, make sure current doesn't point to it anymore.
        let _ = self.current.take();

        // Lastly, update function id.
        self.current = self
            .functions
            .indexed_iter()
            .map_err(|_| ExceededFnIdError)?
            .rev()
            .find_map(|(id, slot)| slot.is_in_progress().then_some(id));

        Ok(fn_id)
    }
}
