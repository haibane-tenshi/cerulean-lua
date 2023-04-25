mod expr;
mod prefix_expr;
mod stmt;
mod tracker;

use thiserror::Error;

use crate::lex::{LexError, Lexer, Token};
use crate::opcode::{Chunk, FunctionId};
use tracker::{
    ChunkTracker, EmitError, Error as CodegenError, ExceededConstIdError, ExceededFnIdError,
    ExceededInstrIdError, FinishFnError, NoActiveFnError, StackStateError,
};

use expr::expr;
use prefix_expr::prefix_expr;
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

trait NextToken {
    type Token;

    fn next_token(&mut self) -> Result<Self::Token, LexParseError>;

    fn next_required_token(&mut self) -> Result<Self::Token, LexParseError> {
        self.next_token().map_err(LexParseError::eof_into_err)
    }
}

impl<'s> NextToken for Lexer<'s> {
    type Token = Token<'s>;

    fn next_token(&mut self) -> Result<Self::Token, LexParseError> {
        let r = self.next().ok_or(LexParseError::Eof)??;

        Ok(r)
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

fn par_expr<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    match s.next_token()? {
        Token::ParL => (),
        _ => return Err(ParseError.into()),
    }

    let mark = tracker.current()?.stack_top()?;
    let (mut s, ()) = expr(s, tracker).map_err(LexParseError::eof_into_err)?;
    // Parenthesised expressions are adjusted to 1.
    tracker.current_mut()?.emit_adjust_to(mark.next())?;

    match s.next_required_token()? {
        Token::ParR => (),
        _ => return Err(ParseError.into()),
    }

    Ok((s, ()))
}

fn expr_list<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let mut mark = tracker.current()?.stack_top()?;
    let (mut s, ()) = expr(s, tracker)?;

    let mut next_part = |mut s: Lexer<'s>| -> Result<(Lexer<'s>, ()), LexParseError> {
        match s.next_token()? {
            Token::Comma => (),
            _ => return Err(ParseError.into()),
        }

        // Expressions in comma lists are adjusted to 1.
        mark = mark.next();
        tracker.current_mut()?.emit_adjust_to(mark)?;

        expr(s, tracker).map_err(LexParseError::eof_into_err)
    };

    loop {
        s = match next_part(s.clone()) {
            Ok((s, ())) => s,
            Err(_) => break,
        }
    }

    Ok((s, ()))
}

fn func_body<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, FunctionId), LexParseError> {
    match s.next_token()? {
        Token::ParL => (),
        _ => return Err(ParseError.into()),
    };

    // Start function
    tracker.start_fn()?;

    // Currently this slot contains pointer to function itself.
    // In the future we will put environment here instead.
    tracker.current_mut()?.push_stack(None)?;

    let mut parlist = |mut s: Lexer<'s>| -> Result<_, LexParseError> {
        let mut count = 0;

        let ident = match s.next_token()? {
            Token::Ident(ident) => ident,
            _ => return Err(ParseError.into()),
        };
        tracker.current_mut()?.push_stack(Some(ident))?;
        count += 1;

        let mut next_ident = |mut s: Lexer<'s>| -> Result<_, LexParseError> {
            match s.next_token()? {
                Token::Comma => (),
                _ => return Err(ParseError.into()),
            }

            let ident = match s.next_required_token()? {
                Token::Ident(ident) => ident,
                _ => return Err(ParseError.into()),
            };
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

    let (mut s, param_count) = match parlist(s.clone()) {
        Ok((s, count)) => (s, count),
        Err(_) => (s, 0),
    };
    // An extra stack slot is taken by function pointer itself.
    let height = param_count + 1;

    match s.next_required_token()? {
        Token::ParR => (),
        _ => return Err(ParseError.into()),
    }

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    }

    // Finish function
    let func_id = tracker.finish_fn(height).map_err(|_| ParseError)?;

    Ok((s, func_id))
}
