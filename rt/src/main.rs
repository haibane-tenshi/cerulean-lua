use std::path::{Path, PathBuf};

use anyhow::Result;
use clap::{Parser, Subcommand};

use repr::chunk::Chunk;

#[derive(Debug, Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Run { path: PathBuf },
    Compile { path: PathBuf },
}

fn load_from_file(path: &Path) -> Result<Chunk> {
    use logos::Logos;
    use parser::lex::Token;

    let data = std::fs::read_to_string(path)?;
    let lexer = Token::lexer(&data);
    let chunk = match parser::parser::chunk(lexer) {
        Ok(t) => t,
        Err(err) => {
            let writer = codespan_reporting::term::termcolor::StandardStream::stdout(
                codespan_reporting::term::termcolor::ColorChoice::Always,
            );
            let config = codespan_reporting::term::Config::default();
            let files = codespan_reporting::files::SimpleFile::new("", &data);
            let diagnostic = err.into_diagnostic();

            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)?;

            return Err(anyhow::Error::msg("failed to parse input"));
        }
    };

    Ok(chunk)
}

fn main() -> Result<()> {
    let logger = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing_subscriber::filter::LevelFilter::TRACE)
        .finish();
    tracing::subscriber::set_global_default(logger)?;

    let Cli { command } = Cli::try_parse()?;

    match command {
        Command::Run { path } => {
            use rt::chunk_cache::single::{Main, SingleChunk};
            use rt::runtime::Runtime;

            let chunk = load_from_file(&path)?;
            let chunk_cache = SingleChunk::new(chunk);

            let mut runtime = Runtime::new(chunk_cache);

            runtime.view().invoke(rt::ffi::call_script(&Main))?;
        }
        Command::Compile { path } => {
            let chunk = load_from_file(&path)?;

            println!("{chunk}");
        }
    }

    Ok(())
}
