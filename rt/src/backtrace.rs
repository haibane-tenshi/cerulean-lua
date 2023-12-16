use std::io::{Result, Write};

#[derive(Debug, Clone)]
pub struct Backtrace {
    pub frames: Vec<BacktraceFrame>,
}

impl Backtrace {
    pub fn emit(&self, writer: &mut impl Write) -> Result<()> {
        writeln!(writer, "backtrace:")?;

        for (i, frame) in self.frames.iter().enumerate() {
            let BacktraceFrame {
                name,
                source,
                location,
            } = frame;

            let name = name.as_deref().unwrap_or("<no debug info available>");

            writeln!(writer, "{i:>5}: [{:<4}] {name}", source.name())?;
            match (source, location) {
                (_, Some(location)) => writeln!(
                    writer,
                    "       at {}:{}:{}",
                    location.file, location.line, location.column
                )?,
                (FrameSource::Lua, None) => {
                    writeln!(writer, "       at <no debug info available>")?
                }
                (FrameSource::Rust, None) => {
                    writeln!(writer, "       <Rust frames do not provide location info>")?
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct BacktraceFrame {
    pub source: FrameSource,
    pub name: Option<String>,
    pub location: Option<Location>,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn file(file: String) -> Self {
        Location {
            file,
            line: 0,
            column: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FrameSource {
    Lua,
    Rust,
}

impl FrameSource {
    fn name(self) -> &'static str {
        match self {
            FrameSource::Lua => "Lua",
            FrameSource::Rust => "Rust",
        }
    }
}
