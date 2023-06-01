use std::fmt::Display;
use thiserror::Error;

use crate::index_vec::Index;

#[derive(Debug, Copy, Clone)]
pub struct ConstId(pub u32);

#[derive(Debug, Error)]
#[error("constant count overflowed u32")]
pub struct ConstCapacityError;

impl Index for ConstId {
    type Error = ConstCapacityError;
    const MAX: Self = ConstId(u32::MAX);

    fn try_from(val: usize) -> Result<Self, Self::Error> {
        let inner = val.try_into().map_err(|_| ConstCapacityError)?;
        Ok(ConstId(inner))
    }

    fn into(self) -> usize {
        self.0.try_into().expect("u32 should fit into usize")
    }
}

impl Display for ConstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
