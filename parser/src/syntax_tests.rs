use std::error::Error;
use std::path::Path;

fn run_test_pass(path: impl AsRef<Path>) -> Result<(), Box<dyn Error>> {
    use crate::lex::Token;
    use crate::parser::chunk;
    use logos::Logos;

    let source = std::fs::read_to_string(path)?;
    let lexer = Token::lexer(&source);
    let _chunk = chunk(lexer)?;

    Ok(())
}

#[test]
fn comment() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/comment.lua")
}

#[test]
fn literal_nil() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/literal_nil.lua")
}

#[test]
fn literal_numeric_decimal() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/literal_numeric_decimal.lua")
}

#[test]
fn literal_numeric_hexadecimal() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/literal_numeric_hexadecimal.lua")
}

#[test]
fn literal_string() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/literal_string.lua")
}

#[test]
fn stmt_semicolon() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_semicolon.lua")
}

#[test]
fn stmt_local_declaration() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_local_declaration.lua")
}
