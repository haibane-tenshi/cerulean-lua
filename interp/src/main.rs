use std::path::{Path, PathBuf};

use anyhow::Result;
use clap::{Parser, Subcommand};

use repr::chunk::Chunk;

#[derive(Debug, Parser)]
struct Cli {
    #[arg(long)]
    trace: bool,
    #[clap(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Run { path: PathBuf },
    Compile { path: PathBuf },
}

fn load_from_file(path: &Path) -> Result<(Chunk, String)> {
    use parser::lex::{Logos, Token};

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
    let Cli { trace, command } = Cli::try_parse()?;

    if trace {
        let logger = tracing_subscriber::FmtSubscriber::builder()
            .with_max_level(tracing_subscriber::filter::LevelFilter::TRACE)
            .pretty()
            .finish();
        tracing::subscriber::set_global_default(logger)?;
    }

    match command {
        Command::Run { path } => {
            use lua_std::Std;
            use lua_std::{lib, std};
            use rt::builtins::coerce::CustomPolicy;
            use rt::chunk_cache::VecCache;
            use rt::gc::{Heap, LuaPtr};
            use rt::runtime::{Core, Runtime};
            use rt::value::traits::DefaultTypes;
            use rt::value::{Callable, Value};

            let core = Core {
                global_env: Value::Nil,
                metatable_registry: Default::default(),
                dialect: CustomPolicy::lua_5_4(),
                gc: Heap::new(),
            };

            let chunk_cache = VecCache::new();
            let mut runtime = Runtime::<DefaultTypes, _>::new(chunk_cache, core);

            let env = Std::empty()
                .include(std::assert)
                .include(std::error)
                .include(std::pcall)
                .include(std::print)
                .include(std::tostring)
                .include(std::load)
                .include(std::loadfile)
                .include(std::setmetatable)
                .include(std::getmetatable)
                .include(std::collectgarbage)
                .include(std::dofile)
                .include(std::_G)
                .include(std::_VERSION)
                .include(std::ipairs)
                .include(std::next)
                .include(std::pairs)
                .include(std::select)
                .include(std::tonumber)
                .include(std::type_)
                .include(std::warn)
                .include(lib::Math::full());

            runtime.include(env);

            let callable = runtime
                .core
                .gc
                .alloc_cell(rt::ffi::boxed(rt::ffi::call_file(&path)));
            let main = runtime.new_thread(Callable::Rust(LuaPtr(callable)));

            if let Err(_err) = runtime.resume(main) {
                use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

                let mut writer = StandardStream::stdout(ColorChoice::Always);

                for thread in runtime.panic_stack(main) {
                    thread.backtrace().emit(&mut writer)?;
                    thread
                        .error()
                        .unwrap()
                        .clone()
                        .into_diagnostic(&runtime.core.gc, &runtime.chunk_cache)
                        .emit(&mut writer, &Default::default())?;
                }

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
