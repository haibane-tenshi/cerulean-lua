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
fn literal_bool() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/literal_bool.lua")
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
fn expr_fn_decl() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/expr_fn_decl.lua")
}

#[test]
fn expr_fn_call() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/expr_fn_call.lua")
}

#[test]
fn expr_table_decl() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/expr_table_decl.lua")
}

#[test]
fn expr_table_lookup() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/expr_table_lookup.lua")
}

#[test]
fn expr_variadic() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/expr_variadic.lua")
}

#[test]
fn expr_op() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/expr_op.lua")
}

#[test]
fn stmt_semicolon() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_semicolon.lua")
}

#[test]
fn stmt_local_declaration() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_local_declaration.lua")
}

#[test]
fn stmt_local_assignment() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_local_assignment.lua")
}

#[test]
fn stmt_local_multiassignment() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_local_multiassignment.lua")
}

#[test]
fn stmt_assignment() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_assignment.lua")
}

#[test]
fn stmt_multiassignment() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_multiassignment.lua")
}

#[test]
fn stmt_local_fn_decl() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_local_fn_decl.lua")
}

#[test]
fn stmt_fn_decl() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_fn_decl.lua")
}

#[test]
fn stmt_fn_call() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_fn_call.lua")
}

#[test]
fn stmt_label() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_label.lua")
}

#[test]
fn stmt_goto() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_goto.lua")
}

#[test]
fn stmt_do_end() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_do_end.lua")
}

#[test]
fn stmt_if_then() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_if_then.lua")
}

#[test]
fn stmt_numeric_for() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_numeric_for.lua")
}

#[test]
fn stmt_generic_for() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_generic_for.lua")
}

#[test]
fn stmt_repeat_until() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_repeat_until.lua")
}

#[test]
fn stmt_while_do() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_while_do.lua")
}

#[test]
fn stmt_break() -> Result<(), Box<dyn Error>> {
    run_test_pass("./syntax_test_suite/stmt_break.lua")
}
