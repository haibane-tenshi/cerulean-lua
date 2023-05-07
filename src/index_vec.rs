use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, RangeBounds};

pub trait Index: Sized {
    type Error;
    const MAX: Self;

    fn try_from(val: usize) -> Result<Self, Self::Error>;
    fn into(self) -> usize;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IndexVec<I, T>(PhantomData<I>, Vec<T>);

impl<I, T> IndexVec<I, T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn as_slice(&self) -> &IndexSlice<I, T> {
        IndexSlice::from_slice(self.1.as_slice())
    }

    pub fn as_mut_slice(&mut self) -> &mut IndexSlice<I, T> {
        IndexSlice::from_mut_slice(self.1.as_mut_slice())
    }

    pub fn pop(&mut self) -> Option<T> {
        self.1.pop()
    }
}

impl<I, T> IndexVec<I, T>
where
    I: Index,
    <I as Index>::Error: Debug,
{
    pub fn push(&mut self, value: T) -> Result<I, <I as Index>::Error> {
        let _ = I::try_from(self.1.len() + 1)?;
        let index = self.len();
        self.1.push(value);

        Ok(index)
    }

    pub fn extend<Iter>(&mut self, iter: Iter) -> Result<(), <I as Index>::Error>
    where
        Iter: IntoIterator<Item = T>,
    {
        self.1.extend(iter);

        match I::try_from(self.1.len()) {
            Err(err) => {
                self.1.truncate(I::MAX.into());
                Err(err)
            }
            Ok(_) => Ok(()),
        }
    }
}

impl<I, T> IndexVec<I, T>
where
    I: Index + Clone,
{
    pub fn drain<R>(&mut self, range: R) -> std::vec::Drain<T>
    where
        R: RangeBounds<I>,
    {
        use std::ops::Bound;

        let map_bound = |bound: Bound<&I>| -> Bound<usize> {
            match bound {
                Bound::Excluded(t) => Bound::Excluded(t.clone().into()),
                Bound::Included(t) => Bound::Included(t.clone().into()),
                Bound::Unbounded => Bound::Unbounded,
            }
        };

        let start = map_bound(range.start_bound());
        let end = map_bound(range.end_bound());

        self.1.drain((start, end))
    }
}

impl<I, T> Default for IndexVec<I, T> {
    fn default() -> Self {
        IndexVec(PhantomData, Default::default())
    }
}

impl<I, T> TryFrom<Vec<T>> for IndexVec<I, T>
where
    I: Index,
{
    type Error = <I as Index>::Error;

    fn try_from(value: Vec<T>) -> Result<Self, Self::Error> {
        let _ = I::try_from(value.len())?;
        Ok(IndexVec(PhantomData, value))
    }
}

impl<I, T> From<IndexVec<I, T>> for Vec<T> {
    fn from(val: IndexVec<I, T>) -> Self {
        val.1
    }
}

impl<I, T> Deref for IndexVec<I, T> {
    type Target = IndexSlice<I, T>;

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<I, T> DerefMut for IndexVec<I, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<I, T> IntoIterator for IndexVec<I, T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.1.into_iter()
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct IndexSlice<I, T>(PhantomData<I>, [T]);

impl<I, T> IndexSlice<I, T> {
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

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.1.iter()
    }
}

impl<I, T> IndexSlice<I, T>
where
    I: Index,
{
    pub fn get(&self, index: I) -> Option<&T> {
        let index = index.into();
        self.1.get(index)
    }

    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        let index = index.into();
        self.1.get_mut(index)
    }
}

impl<I, T> IndexSlice<I, T>
where
    I: Index,
    <I as Index>::Error: Debug,
{
    pub fn len(&self) -> I {
        Index::try_from(self.1.len()).expect("length should fit into index type")
    }

    pub fn indexed_iter(&self) -> impl Iterator<Item = (I, &T)> + DoubleEndedIterator {
        self.iter().enumerate().map(|(i, t)| {
            let index = Index::try_from(i).expect("length should fit into index type");
            (index, t)
        })
    }
}

impl<I, T> Default for &IndexSlice<I, T> {
    fn default() -> Self {
        IndexSlice::from_slice(Default::default())
    }
}

impl<I, T> Default for &mut IndexSlice<I, T> {
    fn default() -> Self {
        IndexSlice::from_mut_slice(Default::default())
    }
}
