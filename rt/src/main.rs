use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
struct Cli {
    path: PathBuf,
}

fn main() -> Result<()> {
    use logos::Logos;
    use parser::lex::Token;
    use rt::runtime::Runtime;

    let logger = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing_subscriber::filter::LevelFilter::TRACE)
        .finish();
    tracing::subscriber::set_global_default(logger)?;

    let Cli { path } = Cli::try_parse()?;

    let data = std::fs::read_to_string(path)?;
    let lexer = Token::lexer(&data);
    let chunk = parser::parser::chunk(lexer)?;

    let mut runtime = Runtime::new(&chunk);

    runtime.run()?;

    Ok(())
}
