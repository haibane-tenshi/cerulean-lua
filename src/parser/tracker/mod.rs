mod const_;
mod function;
mod opcode;
mod stack;

use thiserror::Error;

use crate::index_vec::IndexVec;
use crate::opcode::{Chunk, ConstId, Function, FunctionId, StackSlot};
use crate::value::Literal;

use const_::ConstTracker;
pub use const_::ExceededConstIdError;
use function::FunctionTracker;
pub use function::{BackpatchError, EmitError};
pub use opcode::ExceededInstrIdError;
use opcode::OpCodeTracker;
pub use stack::StackStateError;
use stack::StackTracker;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    ConstId(#[from] ExceededConstIdError),

    #[error(transparent)]
    InstrId(#[from] ExceededInstrIdError),

    #[error(transparent)]
    FnId(#[from] ExceededFnIdError),

    #[error(transparent)]
    NoActiveFn(#[from] NoActiveFnError),

    #[error(transparent)]
    Emit(#[from] EmitError),

    #[error(transparent)]
    Backpatch(#[from] BackpatchError),

    #[error(transparent)]
    StackState(#[from] StackStateError),
}

impl From<FinishFnError> for Error {
    fn from(value: FinishFnError) -> Self {
        match value {
            FinishFnError::NoActiveFn(err) => err.into(),
            FinishFnError::FnResolve(err) => err.into(),
            FinishFnError::FnId(err) => err.into(),
        }
    }
}

#[derive(Debug, Error)]
#[error("exceeded indexing capacity of function ids")]
pub struct ExceededFnIdError;

#[derive(Debug, Error)]
#[error("chunk doesn't have any unresolved functions to write to")]
pub struct NoActiveFnError;

#[derive(Debug, Error)]
pub enum FinishFnError {
    #[error(transparent)]
    NoActiveFn(#[from] NoActiveFnError),

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
pub struct UnresolvedFnError;

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

    pub fn current(&self) -> Result<&FunctionTracker<'s>, NoActiveFnError> {
        (|| {
            let r = match self.functions.get(self.current?)? {
                FunctionSlot::InProgress(tracker) => tracker,
                FunctionSlot::Resolved(_) => unreachable!(),
            };

            Some(r)
        })()
        .ok_or(NoActiveFnError)
    }

    pub fn current_mut(&mut self) -> Result<&mut FunctionTracker<'s>, NoActiveFnError> {
        let current = self.current.ok_or(NoActiveFnError)?;

        let r = match self.functions.get_mut(current).unwrap() {
            FunctionSlot::InProgress(tracker) => tracker,
            FunctionSlot::Resolved(_) => unreachable!(),
        };

        Ok(r)
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
        self.current().ok()?.lookup_local(ident)
    }

    pub fn start_fn(&mut self) -> Result<FunctionId, ExceededFnIdError> {
        let id = self
            .functions
            .push(Default::default())
            .map_err(|_| ExceededFnIdError)?;

        self.current = Some(id);

        Ok(id)
    }

    pub fn finish_fn(&mut self, height: u32) -> Result<FunctionId, FinishFnError> {
        let fn_id = self.current.ok_or(NoActiveFnError)?;

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
