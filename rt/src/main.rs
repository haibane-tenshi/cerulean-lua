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

fn load_from_file(path: &Path) -> Result<(Chunk, String)> {
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

    Ok((chunk, data))
}

fn main() -> Result<()> {
    let logger = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing_subscriber::filter::LevelFilter::TRACE)
        .pretty()
        .finish();
    tracing::subscriber::set_global_default(logger)?;

    let Cli { command } = Cli::try_parse()?;

    match command {
        Command::Run { path } => {
            use rt::chunk_cache::main::MainCache;
            use rt::chunk_cache::single::{Main, SingleChunk};
            use rt::chunk_cache::ChunkId;
            use rt::runtime::Runtime;
            // use rt::value::table::TableRef;
            use rt::backtrace::Location;
            use rt::value::Value;

            let (chunk, source) = load_from_file(&path)?;
            let (env_chunk, builder) = rt::global_env::empty()
                .add(rt::global_env::assert())
                .add(rt::global_env::pcall())
                .add(rt::global_env::print())
                .finish();

            let location = Location::file(path.to_string_lossy().to_string());
            let chunk_cache = MainCache::new(
                SingleChunk::new(env_chunk, None, None),
                SingleChunk::new(chunk, Some(source.clone()), Some(location)),
            );
            let mut runtime = Runtime::new(chunk_cache, Value::Nil);

            let run = || {
                let global_env = builder(runtime.view(), ChunkId(0))?;
                runtime.global_env = global_env;

                runtime.view().invoke(rt::ffi::call_precompiled(&Main))
            };

            if let Err(err) = run() {
                use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

                let view = runtime.view();

                let mut writer = StandardStream::stdout(ColorChoice::Always);

                view.backtrace().emit(&mut writer)?;
                view.into_diagnostic(err)
                    .emit(&mut writer, &Default::default())?;

                return Err(anyhow::Error::msg("runtime error"));
            }
        }
        Command::Compile { path } => {
            let (chunk, _) = load_from_file(&path)?;

            println!("{chunk}");
        }
    }

    Ok(())
}
