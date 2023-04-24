use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use thiserror::Error;

pub type VecU32<T> = IndexVec<u32, T>;

#[derive(Debug, Error)]
#[error("usize value cannot be represented as index")]
pub struct ExceededIndexError;

#[derive(Debug, Error)]
#[error("index value cannot be represented as usize")]
pub struct ExceededUsizeError;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IndexVec<Index, T>(Vec<T>, PhantomData<Index>);

impl<Index, T> IndexVec<Index, T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn as_slice(&self) -> &IndexSlice<Index, T> {
        IndexSlice::from_slice(self.0.as_slice())
    }

    pub fn as_mut_slice(&mut self) -> &mut IndexSlice<Index, T> {
        IndexSlice::from_mut_slice(self.0.as_mut_slice())
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

impl<Index, T> Deref for IndexVec<Index, T> {
    type Target = IndexSlice<Index, T>;

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<Index, T> DerefMut for IndexVec<Index, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct IndexSlice<Index, T>(PhantomData<Index>, [T]);

impl<Index, T> IndexSlice<Index, T> {
    fn from_slice(slice: &[T]) -> &Self {
        // SAFETY: repr(transparent)
        unsafe { std::mem::transmute(slice) }
    }

    fn from_mut_slice(slice: &mut [T]) -> &mut Self {
        // SAFETY: repr(transparent)
        unsafe { std::mem::transmute(slice) }
    }

    pub fn first(&self) -> Option<&T> {
        self.1.first()
    }
}

impl<Index, T> IndexSlice<Index, T>
where
    Index: TryFrom<usize> + TryInto<usize>,
{
    pub fn get(&self, index: Index) -> Option<&T> {
        let index = index.try_into().ok()?;
        self.1.get(index)
    }

    pub fn get_mut(&mut self, index: Index) -> Option<&mut T> {
        let index = index.try_into().ok()?;
        self.1.get_mut(index)
    }

    pub fn len(&self) -> Result<Index, ExceededIndexError> {
        self.1.len().try_into().map_err(|_| ExceededIndexError)
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.1.iter()
    }
}

impl<Index, T> Default for &IndexSlice<Index, T> {
    fn default() -> Self {
        IndexSlice::from_slice(Default::default())
    }
}

impl<Index, T> Default for &mut IndexSlice<Index, T> {
    fn default() -> Self {
        IndexSlice::from_mut_slice(Default::default())
    }
}
