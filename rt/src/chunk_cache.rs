use repr::chunk::Chunk;
use std::fmt::Display;
use std::num::NonZeroUsize;

pub struct FailedInsertError;

pub trait ChunkCache<Id> {
    fn chunk(&self, id: Id) -> Option<&Chunk>;
    fn insert(&mut self, chunk: Chunk) -> Result<Id, FailedInsertError>;
}

pub trait KeyedChunkCache<Id: Clone, Q: ?Sized>: ChunkCache<Id> {
    fn lookup(&self, key: &Q) -> Option<Id>;

    fn lookup_with_chunk(&self, key: &Q) -> Option<(Id, &Chunk)> {
        let id = self.lookup(key)?;
        let chunk = self.chunk(id.clone())?;

        Some((id, chunk))
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct NonZeroChunkId(pub NonZeroUsize);

impl NonZeroChunkId {
    pub fn new(chunk_id: ChunkId) -> Option<Self> {
        let value = NonZeroUsize::new(chunk_id.0)?;
        Some(NonZeroChunkId(value))
    }
}

impl TryFrom<usize> for NonZeroChunkId {
    type Error = std::num::TryFromIntError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        let value = value.try_into()?;
        Ok(NonZeroChunkId(value))
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct ChunkId(pub usize);

impl From<usize> for ChunkId {
    fn from(value: usize) -> Self {
        ChunkId(value)
    }
}

impl From<ChunkId> for usize {
    fn from(value: ChunkId) -> Self {
        value.0
    }
}

impl Display for ChunkId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<NonZeroChunkId> for ChunkId {
    fn from(value: NonZeroChunkId) -> Self {
        ChunkId(value.0.into())
    }
}

pub mod single {
    use super::{ChunkCache, ChunkId, FailedInsertError, KeyedChunkCache, NonZeroChunkId};
    use repr::chunk::Chunk;

    #[derive(Debug, Clone, Copy)]
    pub struct Main;

    #[derive(Debug)]
    pub struct SingleChunk {
        chunk: Chunk,
    }

    impl SingleChunk {
        pub fn new(chunk: Chunk) -> Self {
            SingleChunk { chunk }
        }
    }

    impl ChunkCache<ChunkId> for SingleChunk {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            match id {
                ChunkId(0) => Some(&self.chunk),
                _ => None,
            }
        }

        fn insert(&mut self, _: Chunk) -> Result<ChunkId, FailedInsertError> {
            Err(FailedInsertError)
        }
    }

    impl KeyedChunkCache<ChunkId, Main> for SingleChunk {
        fn lookup(&self, _: &Main) -> Option<ChunkId> {
            Some(ChunkId(0))
        }
    }

    impl ChunkCache<NonZeroChunkId> for SingleChunk {
        fn chunk(&self, id: NonZeroChunkId) -> Option<&Chunk> {
            match id.0.get() {
                1 => Some(&self.chunk),
                _ => None,
            }
        }

        fn insert(&mut self, _chunk: Chunk) -> Result<NonZeroChunkId, FailedInsertError> {
            Err(FailedInsertError)
        }
    }

    impl KeyedChunkCache<NonZeroChunkId, Main> for SingleChunk {
        fn lookup(&self, _: &Main) -> Option<NonZeroChunkId> {
            Some(NonZeroChunkId::try_from(1).unwrap())
        }
    }
}

pub mod path {
    use super::{ChunkCache, ChunkId, KeyedChunkCache};
    use repr::chunk::Chunk;

    use repr::tivec::TiVec;
    use std::borrow::Borrow;
    use std::collections::HashMap;
    use std::hash::Hash;
    pub use std::path::Path;
    use std::path::PathBuf;

    pub struct PathCache {
        ids: HashMap<PathBuf, ChunkId>,
        chunks: TiVec<ChunkId, Chunk>,
    }

    impl ChunkCache<ChunkId> for PathCache {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            self.chunks.get(id)
        }

        fn insert(&mut self, chunk: Chunk) -> Result<ChunkId, super::FailedInsertError> {
            Ok(self.chunks.push_and_get_key(chunk))
        }
    }

    impl<Q> KeyedChunkCache<ChunkId, Q> for PathCache
    where
        PathBuf: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        fn lookup(&self, key: &Q) -> Option<ChunkId> {
            self.ids.get(key).copied()
        }
    }
}

pub mod main {
    use super::{ChunkCache, ChunkId, FailedInsertError, KeyedChunkCache, NonZeroChunkId};
    use repr::chunk::Chunk;

    #[derive(Debug)]
    pub struct MainCache<C> {
        special: Chunk,
        cache: C,
    }

    impl<C> MainCache<C> {
        pub fn new(special: Chunk, cache: C) -> Self {
            MainCache { special, cache }
        }
    }

    impl<C> ChunkCache<ChunkId> for MainCache<C>
    where
        C: ChunkCache<NonZeroChunkId>,
    {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            if let Some(id) = NonZeroChunkId::new(id) {
                self.cache.chunk(id)
            } else {
                Some(&self.special)
            }
        }

        fn insert(&mut self, chunk: Chunk) -> Result<ChunkId, FailedInsertError> {
            self.cache.insert(chunk).map(Into::into)
        }
    }

    impl<C, Q> KeyedChunkCache<ChunkId, Q> for MainCache<C>
    where
        Q: ?Sized,
        C: KeyedChunkCache<NonZeroChunkId, Q>,
    {
        fn lookup(&self, key: &Q) -> Option<ChunkId> {
            self.cache.lookup(key).map(Into::into)
        }
    }
}
