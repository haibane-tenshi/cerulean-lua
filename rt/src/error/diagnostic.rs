use codespan_reporting::term::termcolor::WriteColor;

pub use codespan_reporting::diagnostic::{Diagnostic as Message, Label};
pub use codespan_reporting::files::{Error, SimpleFile};
pub use codespan_reporting::term::Config;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub files: SimpleFile<String, String>,
    pub message: Message<()>,
}

impl Diagnostic {
    pub fn with_message(message: Message<()>) -> Self {
        Diagnostic {
            files: SimpleFile::new("".into(), "".into()),
            message,
        }
    }

    pub fn emit(&self, writer: &mut dyn WriteColor, config: &Config) -> Result<(), Error> {
        use codespan_reporting::term::emit;

        emit(writer, config, &self.files, &self.message)
    }

    pub fn emit_to_string(&self) -> String {
        use codespan_reporting::term::termcolor::NoColor;

        let mut s = Vec::new();
        let _ = self.emit(&mut NoColor::new(&mut s), &Default::default());
        String::from_utf8_lossy(&s).to_string()
    }
}
