use codespan_reporting::diagnostic::Diagnostic as Message;
use codespan_reporting::files::SimpleFile;

#[derive(Debug)]
pub struct Diagnostic {
    pub files: SimpleFile<String, String>,
    pub message: Message<()>,
}
