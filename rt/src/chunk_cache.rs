use crate::backtrace::Location;
use gc::Trace;
use repr::chunk::Chunk;
use std::fmt::Display;

pub use main::MainCache;
pub use single::SingleChunk;
pub use vec::VecCache;

#[derive(Debug, Clone, Copy)]
pub struct ImmutableCacheError;

impl ImmutableCacheError {
    pub(crate) fn into_diagnostic<FileId>(
        self,
    ) -> codespan_reporting::diagnostic::Diagnostic<FileId> {
        use codespan_reporting::diagnostic::Diagnostic;

        Diagnostic::error().with_message("runtime does not support loading new chunks")
    }
}

pub trait ChunkCache {
    fn chunk(&self, id: ChunkId) -> Option<&Chunk>;
    fn source(&self, id: ChunkId) -> Option<String>;
    fn location(&self, id: ChunkId) -> Option<Location>;
    fn insert(
        &mut self,
        chunk: Chunk,
        source: Option<String>,
        location: Option<Location>,
    ) -> Result<ChunkId, ImmutableCacheError>;
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, Trace)]
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

pub mod single {
    use super::{ChunkCache, ChunkId, ImmutableCacheError, Location};
    use repr::chunk::Chunk;

    #[derive(Debug, Clone)]
    pub struct SingleChunk {
        pub chunk: Chunk,
        pub source: Option<String>,
        pub location: Option<Location>,
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
        ) -> Result<ChunkId, ImmutableCacheError> {
            Err(ImmutableCacheError)
        }
    }
}

pub mod main {
    use super::single::SingleChunk;
    use super::{ChunkCache, ChunkId};
    use repr::chunk::Chunk;

    #[derive(Debug, Clone)]
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
        ) -> Result<ChunkId, super::ImmutableCacheError> {
            self.cache
                .insert(chunk, source, location)
                .map(Self::from_cache_index)
        }
    }
}

pub mod vec {
    use super::single::SingleChunk;
    use super::{ChunkCache, ChunkId};
    use crate::backtrace::Location;
    use repr::tivec::TiVec;

    #[derive(Debug, Clone, Default)]
    pub struct VecCache(TiVec<ChunkId, SingleChunk>);

    impl VecCache {
        pub fn new() -> Self {
            Default::default()
        }
    }

    impl ChunkCache for VecCache {
        fn chunk(&self, id: ChunkId) -> Option<&repr::chunk::Chunk> {
            self.0.get(id).map(|cache| &cache.chunk)
        }

        fn source(&self, id: ChunkId) -> Option<String> {
            self.0.get(id).and_then(|cache| cache.source.clone())
        }

        fn location(&self, id: ChunkId) -> Option<Location> {
            self.0.get(id).and_then(|cache| cache.location.clone())
        }

        fn insert(
            &mut self,
            chunk: repr::chunk::Chunk,
            source: Option<String>,
            location: Option<crate::backtrace::Location>,
        ) -> Result<super::ChunkId, super::ImmutableCacheError> {
            let value = SingleChunk {
                chunk,
                source,
                location,
            };
            let id = self.0.push_and_get_key(value);
            Ok(id)
        }
    }
}
