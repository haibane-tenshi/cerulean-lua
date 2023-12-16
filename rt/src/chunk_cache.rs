use repr::chunk::Chunk;
use std::fmt::Display;
use std::num::NonZeroUsize;

use crate::backtrace::Location;

pub struct FailedInsertError;

pub trait ChunkCache<Id> {
    fn chunk(&self, id: Id) -> Option<&Chunk>;

    fn location(&self, id: Id) -> Option<Location>;

    fn insert_with_location(
        &mut self,
        chunk: Chunk,
        location: Option<Location>,
    ) -> Result<Id, FailedInsertError>;

    fn insert(&mut self, chunk: Chunk) -> Result<Id, FailedInsertError> {
        self.insert_with_location(chunk, None)
    }
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
    use super::{
        ChunkCache, ChunkId, FailedInsertError, KeyedChunkCache, Location, NonZeroChunkId,
    };
    use repr::chunk::Chunk;
    use std::fmt::Display;

    #[derive(Debug, Clone, Copy)]
    pub struct Main;

    impl Display for Main {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "<main script>")
        }
    }

    #[derive(Debug)]
    pub struct SingleChunk {
        chunk: Chunk,
        location: Option<Location>,
    }

    impl SingleChunk {
        pub fn new(chunk: Chunk, location: Option<Location>) -> Self {
            SingleChunk { chunk, location }
        }
    }

    impl ChunkCache<ChunkId> for SingleChunk {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            match id {
                ChunkId(0) => Some(&self.chunk),
                _ => None,
            }
        }

        fn insert_with_location(
            &mut self,
            _: Chunk,
            _: Option<Location>,
        ) -> Result<ChunkId, FailedInsertError> {
            Err(FailedInsertError)
        }

        fn location(&self, id: ChunkId) -> Option<Location> {
            match id {
                ChunkId(0) => self.location.clone(),
                _ => None,
            }
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

        fn insert_with_location(
            &mut self,
            _chunk: Chunk,
            _: Option<Location>,
        ) -> Result<NonZeroChunkId, FailedInsertError> {
            Err(FailedInsertError)
        }

        fn location(&self, id: NonZeroChunkId) -> Option<Location> {
            match id.0.get() {
                1 => self.location.clone(),
                _ => None,
            }
        }
    }

    impl KeyedChunkCache<NonZeroChunkId, Main> for SingleChunk {
        fn lookup(&self, _: &Main) -> Option<NonZeroChunkId> {
            Some(NonZeroChunkId::try_from(1).unwrap())
        }
    }
}

pub mod path {
    use super::{ChunkCache, ChunkId, KeyedChunkCache, Location};
    use repr::chunk::Chunk;

    use repr::tivec::TiVec;
    use std::borrow::Borrow;
    use std::collections::HashMap;
    use std::hash::Hash;
    pub use std::path::Path;
    use std::path::PathBuf;

    pub struct PathCache {
        ids: HashMap<PathBuf, ChunkId>,
        chunks: TiVec<ChunkId, (Chunk, Option<Location>)>,
    }

    impl ChunkCache<ChunkId> for PathCache {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            self.chunks.get(id).map(|(chunk, _)| chunk)
        }

        fn insert_with_location(
            &mut self,
            chunk: Chunk,
            location: Option<Location>,
        ) -> Result<ChunkId, super::FailedInsertError> {
            Ok(self.chunks.push_and_get_key((chunk, location)))
        }

        fn location(&self, id: ChunkId) -> Option<Location> {
            self.chunks
                .get(id)
                .and_then(|(_, location)| location.clone())
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
    use super::{
        ChunkCache, ChunkId, FailedInsertError, KeyedChunkCache, Location, NonZeroChunkId,
    };
    use repr::chunk::Chunk;

    #[derive(Debug)]
    pub struct MainCache<C> {
        special: (Chunk, Option<Location>),
        cache: C,
    }

    impl<C> MainCache<C> {
        pub fn new(chunk: Chunk, location: Option<Location>, cache: C) -> Self {
            MainCache {
                special: (chunk, location),
                cache,
            }
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
                Some(&self.special.0)
            }
        }

        fn insert_with_location(
            &mut self,
            chunk: Chunk,
            location: Option<Location>,
        ) -> Result<ChunkId, FailedInsertError> {
            self.cache
                .insert_with_location(chunk, location)
                .map(Into::into)
        }

        fn location(&self, id: ChunkId) -> Option<Location> {
            if let Some(id) = NonZeroChunkId::new(id) {
                self.cache.location(id)
            } else {
                self.special.1.clone()
            }
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
