use repr::chunk::Chunk;
use std::fmt::Display;
// use std::num::NonZeroUsize;
use crate::backtrace::Location;

#[derive(Debug)]
pub struct Immutable;

pub trait ChunkCache {
    fn chunk(&self, id: ChunkId) -> Option<&Chunk>;
    fn source(&self, id: ChunkId) -> Option<String>;
    fn location(&self, id: ChunkId) -> Option<Location>;
    fn insert(
        &mut self,
        chunk: Chunk,
        source: Option<String>,
        location: Option<Location>,
    ) -> Result<ChunkId, Immutable>;
}

pub trait KeyedChunkCache<Q: ?Sized> {
    fn bind(&mut self, key: &Q, chunk_id: ChunkId) -> Result<(), Immutable>;
    fn get(&self, key: &Q) -> Option<ChunkId>;
}

// #[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
// pub struct NonZeroChunkId(pub NonZeroUsize);

// impl NonZeroChunkId {
//     pub fn new(chunk_id: ChunkId) -> Option<Self> {
//         let value = NonZeroUsize::new(chunk_id.0)?;
//         Some(NonZeroChunkId(value))
//     }
// }

// impl TryFrom<usize> for NonZeroChunkId {
//     type Error = std::num::TryFromIntError;

//     fn try_from(value: usize) -> Result<Self, Self::Error> {
//         let value = value.try_into()?;
//         Ok(NonZeroChunkId(value))
//     }
// }

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

// impl From<NonZeroChunkId> for ChunkId {
//     fn from(value: NonZeroChunkId) -> Self {
//         ChunkId(value.0.into())
//     }
// }

pub mod single {
    use super::{ChunkCache, ChunkId, Immutable, KeyedChunkCache, Location};
    use repr::chunk::Chunk;

    #[derive(Debug, Clone, Copy)]
    pub struct Main;

    #[derive(Debug)]
    pub struct SingleChunk {
        chunk: Chunk,
        source: Option<String>,
        location: Option<Location>,
    }

    impl SingleChunk {
        pub fn new(chunk: Chunk, source: Option<String>, location: Option<Location>) -> Self {
            SingleChunk {
                chunk,
                source,
                location,
            }
        }
    }

    impl ChunkCache for SingleChunk {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            match id {
                ChunkId(0) => Some(&self.chunk),
                _ => None,
            }
        }

        fn source(&self, id: ChunkId) -> Option<String> {
            match id {
                ChunkId(0) => self.source.clone(),
                _ => None,
            }
        }

        fn location(&self, id: ChunkId) -> Option<Location> {
            match id {
                ChunkId(0) => self.location.clone(),
                _ => None,
            }
        }

        fn insert(
            &mut self,
            _chunk: Chunk,
            _source: Option<String>,
            _location: Option<Location>,
        ) -> Result<ChunkId, Immutable> {
            Err(Immutable)
        }
    }

    impl KeyedChunkCache<Main> for SingleChunk {
        fn get(&self, _key: &Main) -> Option<ChunkId> {
            Some(ChunkId(0))
        }

        fn bind(&mut self, _key: &Main, _chunk_id: ChunkId) -> Result<(), Immutable> {
            Err(Immutable)
        }
    }
}

pub mod main {
    use super::single::SingleChunk;
    use super::{ChunkCache, ChunkId, KeyedChunkCache};
    use repr::chunk::Chunk;

    #[derive(Debug)]
    pub struct MainCache<C> {
        special: SingleChunk,
        cache: C,
    }

    impl<C> MainCache<C> {
        pub fn new(special: SingleChunk, cache: C) -> Self {
            MainCache { special, cache }
        }

        fn to_cache_index(chunk_id: ChunkId) -> Option<ChunkId> {
            let value = chunk_id.0.checked_sub(1)?;
            Some(ChunkId(value))
        }

        fn from_cache_index(chunk_id: ChunkId) -> ChunkId {
            ChunkId(chunk_id.0 + 1)
        }
    }

    impl<C> ChunkCache for MainCache<C>
    where
        C: ChunkCache,
    {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            if let Some(id) = Self::to_cache_index(id) {
                self.cache.chunk(id)
            } else {
                self.special.chunk(id)
            }
        }

        fn source(&self, id: ChunkId) -> Option<String> {
            if let Some(id) = Self::to_cache_index(id) {
                self.cache.source(id)
            } else {
                self.special.source(id)
            }
        }

        fn location(&self, id: ChunkId) -> Option<crate::backtrace::Location> {
            if let Some(id) = Self::to_cache_index(id) {
                self.cache.location(id)
            } else {
                self.special.location(id)
            }
        }

        fn insert(
            &mut self,
            chunk: Chunk,
            source: Option<String>,
            location: Option<crate::backtrace::Location>,
        ) -> Result<ChunkId, super::Immutable> {
            self.cache
                .insert(chunk, source, location)
                .map(Self::from_cache_index)
        }
    }

    impl<C, Q> KeyedChunkCache<Q> for MainCache<C>
    where
        Q: ?Sized,
        C: KeyedChunkCache<Q>,
    {
        fn bind(&mut self, key: &Q, chunk_id: ChunkId) -> Result<(), super::Immutable> {
            if let Some(chunk_id) = Self::to_cache_index(chunk_id) {
                self.cache.bind(key, chunk_id)
            } else {
                Err(super::Immutable)
            }
        }

        fn get(&self, key: &Q) -> Option<ChunkId> {
            self.cache.get(key).map(Self::from_cache_index)
        }
    }
}
