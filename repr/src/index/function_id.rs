use thiserror::Error;

use crate::index_vec::Index;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct FunctionId(pub u32);

#[derive(Debug, Error)]
#[error("function count overflowed u32")]
pub struct FunctionCapacityError;

impl Index for FunctionId {
    type Error = FunctionCapacityError;
    const MAX: Self = FunctionId(u32::MAX);

    fn try_from(val: usize) -> Result<Self, Self::Error> {
        let inner = val.try_into().map_err(|_| FunctionCapacityError)?;
        Ok(FunctionId(inner))
    }

    fn into(self) -> usize {
        self.0.try_into().expect("u32 should fit into usize")
    }
}
