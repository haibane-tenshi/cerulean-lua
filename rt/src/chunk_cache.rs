use repr::chunk::Chunk;
use repr::index::FunctionId;

pub trait ChunkCache {
    fn chunk(&self, id: ChunkId) -> Option<&Chunk>;
}

pub trait KeyedChunkCache<Q: ?Sized>: ChunkCache {
    fn lookup(&self, key: &Q) -> Option<ChunkId>;

    fn lookup_with_chunk(&self, key: &Q) -> Option<(ChunkId, &Chunk)> {
        let id = self.lookup(key)?;
        let chunk = self.chunk(id)?;

        Some((id, chunk))
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ChunkId(pub usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct FunctionPtr {
    pub chunk_id: ChunkId,
    pub function_id: FunctionId,
}

pub mod single {
    use super::{ChunkCache, ChunkId, KeyedChunkCache};
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

    impl ChunkCache for SingleChunk {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            match id {
                ChunkId(0) => Some(&self.chunk),
                _ => None,
            }
        }
    }

    impl KeyedChunkCache<Main> for SingleChunk {
        fn lookup(&self, _: &Main) -> Option<ChunkId> {
            Some(ChunkId(0))
        }
    }
}

pub mod path {
    use super::{ChunkCache, ChunkId, KeyedChunkCache};
    use repr::chunk::Chunk;

    use std::borrow::Borrow;
    use std::collections::HashMap;
    use std::hash::Hash;
    pub use std::path::Path;
    use std::path::PathBuf;

    pub struct PathCache {
        ids: HashMap<PathBuf, ChunkId>,
        chunks: Vec<Chunk>,
    }

    impl ChunkCache for PathCache {
        fn chunk(&self, id: ChunkId) -> Option<&Chunk> {
            self.chunks.get(id.0)
        }
    }

    impl<Q> KeyedChunkCache<Q> for PathCache
    where
        PathBuf: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        fn lookup(&self, key: &Q) -> Option<ChunkId> {
            self.ids.get(key).copied()
        }
    }
}
