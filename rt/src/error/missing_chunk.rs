use codespan_reporting::diagnostic::Diagnostic;

use crate::chunk_cache::ChunkId;

#[derive(Debug, Clone, Copy)]
pub struct MissingChunk(pub ChunkId);

impl MissingChunk {
    pub(crate) fn into_diagnostic<FileId>(self) -> Diagnostic<FileId> {
        let MissingChunk(chunk_id) = self;

        Diagnostic::error().with_message(format!(
            "chunk cache does not contain chunk with id={chunk_id}"
        ))
    }
}
