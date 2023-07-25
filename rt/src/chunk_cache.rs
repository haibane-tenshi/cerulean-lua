use repr::chunk::Chunk;
use repr::index::FunctionId;

pub trait ChunkCache {
    fn chunk(&self, id: ChunkId) -> Option<&Chunk>;
}

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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct ChunkId(pub usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct FunctionPtr {
    pub chunk_id: ChunkId,
    pub function_id: FunctionId,
}
