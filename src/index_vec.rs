use std::marker::PhantomData;

use thiserror::Error;

pub type VecU32<T> = IndexVec<u32, T>;

#[derive(Debug, Error)]
#[error("index cannot be represented")]
pub struct ExceededIndexError;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IndexVec<Index, T>(Vec<T>, PhantomData<Index>);

impl<Index, T> IndexVec<Index, T> {
    pub fn new() -> Self {
        Default::default()
    }
}

impl<Index, T> IndexVec<Index, T>
where
    Index: TryFrom<usize> + TryInto<usize>,
{
    pub fn push(&mut self, value: T) -> Result<Index, ExceededIndexError> {
        let index = self.len()?;
        self.0.push(value);

        Ok(index)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub fn get(&self, index: Index) -> Option<&T> {
        let index = index.try_into().ok()?;
        self.0.get(index)
    }

    pub fn get_mut(&mut self, index: Index) -> Option<&mut T> {
        let index = index.try_into().ok()?;
        self.0.get_mut(index)
    }

    pub fn len(&self) -> Result<Index, ExceededIndexError> {
        self.0.len().try_into().map_err(|_| ExceededIndexError)
    }
}

impl<Index, T> Default for IndexVec<Index, T> {
    fn default() -> Self {
        IndexVec(Default::default(), PhantomData)
    }
}

impl<Index, T> From<Vec<T>> for IndexVec<Index, T> {
    fn from(value: Vec<T>) -> Self {
        IndexVec(value, PhantomData)
    }
}

impl<Index, T> Into<Vec<T>> for IndexVec<Index, T> {
    fn into(self) -> Vec<T> {
        self.0
    }
}
