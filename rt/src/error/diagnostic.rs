use codespan_reporting::diagnostic::Diagnostic as Message;
use codespan_reporting::files::{Error, SimpleFile};
use codespan_reporting::term::termcolor::WriteColor;
use codespan_reporting::term::Config;

#[derive(Debug)]
pub struct Diagnostic {
    pub files: SimpleFile<String, String>,
    pub message: Message<()>,
}

impl Diagnostic {
    pub fn emit(&self, writer: &mut dyn WriteColor, config: &Config) -> Result<(), Error> {
        use codespan_reporting::term::emit;

        emit(writer, config, &self.files, &self.message)
    }
}
