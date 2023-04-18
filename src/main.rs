use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
struct Cli {
    path: PathBuf,
}

fn main() -> Result<()> {
    use cerulean_lua::lex::Token;
    use cerulean_lua::runtime::Runtime;
    use logos::Logos;

    let logger = tracing_subscriber::FmtSubscriber::builder()
        .with_max_level(tracing_subscriber::filter::LevelFilter::TRACE)
        .finish();
    tracing::subscriber::set_global_default(logger)?;

    let Cli { path } = Cli::try_parse()?;

    let data = std::fs::read_to_string(path)?;
    let tokens: Vec<_> = Token::lexer(&data).collect::<Result<_, _>>()?;
    let chunk = cerulean_lua::parser::chunk(&tokens)?;

    let mut runtime = Runtime::new(chunk);

    runtime.run()?;

    Ok(())
}
