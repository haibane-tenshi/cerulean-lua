mod expr;

use std::collections::HashMap;

use thiserror::Error;

use crate::lex::{LexError, Lexer, Token};
use crate::opcode::{Chunk, ConstId, OpCode, StackSlot};
use crate::value::Literal;

#[derive(Debug, Error)]
#[error("parsing error")]
pub struct ParseError;

#[derive(Debug, Error)]
pub enum LexParseError {
    #[error(transparent)]
    Lex(#[from] LexError),

    #[error(transparent)]
    Parse(#[from] ParseError),

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

#[derive(Debug, Default)]
struct ConstTracker {
    constants: Vec<Literal>,
    backlinks: HashMap<Literal, ConstId>,
}

impl ConstTracker {
    pub fn insert(&mut self, value: Literal) -> ConstId {
        *self.backlinks.entry(value.clone()).or_insert_with(|| {
            let index = self.constants.len().try_into().unwrap();
            self.constants.push(value);

            ConstId(index)
        })
    }

    pub fn resolve(self) -> Vec<Literal> {
        self.constants
    }
}

#[derive(Debug, Default)]
struct StackTracker<'s> {
    stack: Vec<Option<&'s str>>,
    backlinks: HashMap<&'s str, Vec<StackSlot>>,
    frames: Vec<usize>,
}

impl<'s> StackTracker<'s> {
    pub fn push(&mut self) -> StackSlot {
        let index = self.stack.len().try_into().unwrap();
        self.stack.push(None);

        StackSlot(index)
    }

    pub fn push_named(&mut self, name: &'s str) -> StackSlot {
        let index = self.stack.len().try_into().unwrap();
        self.stack.push(Some(name));

        let r = StackSlot(index);

        self.backlinks.entry(name).or_default().push(r);

        r
    }

    pub fn pop(&mut self) {
        let Some(Some(name)) = self.stack.pop() else {
            return
        };

        let Some(backlink) = self.backlinks.get_mut(&name) else {
            return
        };

        backlink.pop();

        if backlink.is_empty() {
            self.backlinks.remove(&name);
        }
    }

    pub fn lookup_slot(&self, name: &str) -> Option<StackSlot> {
        self.backlinks.get(name).and_then(|bl| bl.last()).copied()
    }

    pub fn push_frame(&mut self) {
        self.frames.push(self.stack.len());
    }

    pub fn pop_frame(&mut self) -> Option<u32> {
        let index = self.frames.pop()?;
        let count = self.stack.len().checked_sub(index)?.try_into().ok()?;

        for _ in 0..count {
            self.pop();
        }

        Some(count)
    }
}

#[derive(Debug, Default)]
struct ChunkTracker<'s> {
    constants: ConstTracker,
    stack: StackTracker<'s>,
    codes: Vec<OpCode>,
}

impl<'s> ChunkTracker<'s> {
    pub fn empty() -> Self {
        Default::default()
    }
}

pub fn chunk(s: Lexer) -> Result<Chunk, LexParseError> {
    let mut storages = ChunkTracker::empty();

    match block(s, &mut storages) {
        Ok((mut s, ())) => match s.next_token() {
            Err(LexParseError::Eof) => (),
            _ => return Err(ParseError.into()),
        },
        Err(LexParseError::Eof) => (),
        Err(err) => return Err(err),
    }

    let ChunkTracker {
        codes,
        constants,
        stack: _,
    } = storages;

    let constants = constants.resolve();

    let chunk = Chunk {
        codes,
        constants,
        lines: Default::default(),
    };

    Ok(chunk)
}

fn block<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    tracker.stack.push_frame();

    loop {
        s = match statement(s.clone(), tracker) {
            Ok((s, ())) => s,
            Err(_) => break,
        };
    }

    let extra_stack = tracker.stack.pop_frame().unwrap();

    // Remove excessive temporaries upon exiting block.
    if let Ok(extra_stack) = extra_stack.try_into() {
        tracker.codes.push(OpCode::PopStack(extra_stack))
    }

    Ok((s, ()))
}

fn statement<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let r = if let Ok(r) = semicolon(s.clone()) {
        Ok(r)
    } else if let Ok(r) = assignment(s.clone(), tracker) {
        Ok(r)
    } else if let Ok(r) = do_end(s.clone(), tracker) {
        Ok(r)
    } else {
        let mut s = s;
        let _ = s.next_token()?;
        Err(ParseError.into())
    };

    r
}

fn semicolon(mut s: Lexer) -> Result<(Lexer, ()), LexParseError> {
    match s.next_token()? {
        Token::Semicolon => Ok((s, ())),
        _ => Err(ParseError.into()),
    }
}

fn do_end<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    match s.next_token()? {
        Token::Do => (),
        _ => return Err(ParseError.into()),
    };

    let (mut s, ()) = block(s, tracker).map_err(LexParseError::eof_into_err)?;

    match s.next_required_token()? {
        Token::End => (),
        _ => return Err(ParseError.into()),
    };

    Ok((s, ()))
}

fn assignment<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    let tokens = [
        s.next_token()?,
        s.next_required_token()?,
        s.next_required_token()?,
    ];

    let ident = match tokens.as_slice() {
        [Token::Local, Token::Ident(ident), Token::Assign] => ident,
        _ => return Err(ParseError.into()),
    };

    let (s, ()) = expr::expr(s, tracker).map_err(LexParseError::eof_into_err)?;

    tracker.stack.pop();
    tracker.stack.push_named(ident);

    Ok((s, ()))
}
