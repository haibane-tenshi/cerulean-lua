mod expr;
mod stmt;

use std::borrow::Cow;
use thiserror::Error;

use crate::lex::{LexError, Lexer, Token};
use crate::opcode::{Chunk, FunctionId};
use crate::tracker::{
    BackpatchError, ChunkTracker, EmitError, Error as CodegenError, ExceededConstIdError,
    ExceededFnIdError, ExceededInstrIdError, FinishFnError, NoActiveFnError, StackStateError,
};

use expr::expr;
use stmt::{return_, statement};

#[derive(Debug, Error)]
#[error("parsing error")]
pub struct ParseError;

#[derive(Debug, Error)]
pub enum LexParseError {
    #[error(transparent)]
    Lex(#[from] LexError),

    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error("failed to generate code")]
    Codegen(#[from] CodegenError),

    #[error("reached end of input")]
    Eof,
}

impl LexParseError {
    pub fn eof_into_err(self) -> Self {
        match self {
            LexParseError::Eof => LexParseError::Parse(ParseError),
            t => t,
        }
    }
}

impl From<ExceededConstIdError> for LexParseError {
    fn from(value: ExceededConstIdError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<NoActiveFnError> for LexParseError {
    fn from(value: NoActiveFnError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<EmitError> for LexParseError {
    fn from(value: EmitError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<StackStateError> for LexParseError {
    fn from(value: StackStateError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<ExceededInstrIdError> for LexParseError {
    fn from(value: ExceededInstrIdError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<ExceededFnIdError> for LexParseError {
    fn from(value: ExceededFnIdError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<FinishFnError> for LexParseError {
    fn from(value: FinishFnError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

impl From<BackpatchError> for LexParseError {
    fn from(value: BackpatchError) -> Self {
        LexParseError::Codegen(value.into())
    }
}

trait NextToken {
    type Token;

    fn next_token(&mut self) -> Result<Self::Token, LexParseError>;
}

impl<'s> NextToken for Lexer<'s> {
    type Token = Token<'s>;

    fn next_token(&mut self) -> Result<Self::Token, LexParseError> {
        let r = self.next().ok_or(LexParseError::Eof)??;

        Ok(r)
    }
}

trait Require {
    fn require(self) -> Self;
}

impl<'s, T> Require for Result<(Lexer<'s>, T), LexParseError> {
    fn require(self) -> Self {
        self.map_err(LexParseError::eof_into_err)
    }
}

trait Optional<T> {
    type Source;

    fn optional(self, source: Self::Source) -> (Self::Source, Option<T>);
}

impl<'s, T> Optional<T> for Result<(Lexer<'s>, T), LexParseError> {
    type Source = Lexer<'s>;

    fn optional(self, source: Self::Source) -> (Self::Source, Option<T>) {
        match self {
            Ok((s, t)) => (s, Some(t)),
            Err(_) => (source, None),
        }
    }
}

fn match_token<'s>(mut s: Lexer<'s>, token: Token<'s>) -> Result<(Lexer<'s>, ()), LexParseError> {
    if s.next_token()? == token {
        Ok((s, ()))
    } else {
        Err(ParseError.into())
    }
}

fn identifier(mut s: Lexer) -> Result<(Lexer, &str), LexParseError> {
    match s.next_token()? {
        Token::Ident(ident) => Ok((s, ident)),
        _ => Err(ParseError.into()),
    }
}

fn literal_str(mut s: Lexer) -> Result<(Lexer, Cow<str>), LexParseError> {
    match s.next_token()? {
        Token::ShortLiteralString(r) => Ok((s, r)),
        _ => Err(ParseError.into()),
    }
}

pub fn chunk(s: Lexer) -> Result<Chunk, LexParseError> {
    let mut tracker = ChunkTracker::empty();

    tracker.start_fn()?;

    match block(s, &mut tracker) {
        Ok((mut s, ())) => match s.next_token() {
            Err(LexParseError::Eof) => (),
            _ => return Err(ParseError.into()),
        },
        Err(LexParseError::Eof) => (),
        Err(err) => return Err(err),
    }

    tracker.finish_fn(0)?;
    tracker.resolve().map_err(|_| ParseError.into())
}

fn block<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    tracker.current_mut()?.push_block()?;

    let r = inner_block(s, tracker);

    tracker.current_mut()?.pop_block()?;

    r
}

fn inner_block<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    loop {
        s = match statement(s.clone(), tracker) {
            Ok((s, ())) => s,
            Err(_) => break,
        };
    }

    let s = match return_(s.clone(), tracker) {
        Ok((s, ())) => s,
        Err(_) => s,
    };

    Ok((s, ()))
}

fn expr_adjusted_to<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
    count: u32,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mark = tracker.current()?.stack_top()? + count;
    let r = expr(s, tracker)?;
    tracker.current_mut()?.emit_adjust_to(mark)?;

    Ok(r)
}

fn expr_adjusted_to_1<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    expr_adjusted_to(s, tracker, 1)
}

fn par_expr<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let (s, ()) = match_token(s, Token::ParL)?;
    let (s, ()) = expr_adjusted_to_1(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::ParR).require()?;

    Ok((s, ()))
}

fn expr_list<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mut mark = tracker.current()?.stack_top()?;

    let (mut s, ()) = expr(s, tracker)?;

    let mut next_part = |s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        let (s, ()) = match_token(s, Token::Comma)?;

        // Expressions inside comma lists are adjusted to 1.
        mark += 1;
        tracker.current_mut()?.emit_adjust_to(mark)?;

        expr(s, tracker).require()
    };

    loop {
        s = match next_part(s.clone()) {
            Ok((s, ())) => s,
            Err(_) => break,
        }
    }

    Ok((s, ()))
}

fn expr_list_adjusted_to<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
    count: u32,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mark = tracker.current()?.stack_top()? + count;
    let r = expr_list(s, tracker)?;
    tracker.current_mut()?.emit_adjust_to(mark)?;

    Ok(r)
}

fn func_body<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, FunctionId), LexParseError> {
    let (s, ()) = match_token(s, Token::ParL)?;

    // Start function
    tracker.start_fn()?;

    // Currently this slot contains pointer to function itself.
    // In the future we will put environment here instead.
    tracker.current_mut()?.push_stack(None)?;

    let mut parlist = |s: Lexer<'s>| -> Result<_, LexParseError> {
        let mut count = 0;

        let (mut s, ident) = identifier(s)?;
        tracker.current_mut()?.push_stack(Some(ident))?;
        count += 1;

        let mut next_ident = |s: Lexer<'s>| -> Result<_, LexParseError> {
            let (s, ()) = match_token(s, Token::Comma)?;
            let (s, ident) = identifier(s).require()?;

            tracker.current_mut()?.push_stack(Some(ident))?;
            count += 1;

            Ok(s)
        };

        loop {
            s = match next_ident(s.clone()) {
                Ok(s) => s,
                Err(_) => break,
            };
        }

        Ok((s, count))
    };

    let (s, param_count) = parlist(s.clone()).optional(s);
    let param_count = param_count.unwrap_or(0);

    // An extra stack slot is taken by function pointer itself.
    let height = param_count + 1;

    let (s, ()) = match_token(s, Token::ParR).require()?;
    let (s, ()) = block(s, tracker).require()?;
    let (s, ()) = match_token(s, Token::End).require()?;

    // Finish function
    let func_id = tracker.finish_fn(height).map_err(|_| ParseError)?;

    Ok((s, func_id))
}
