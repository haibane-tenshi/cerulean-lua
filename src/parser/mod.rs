mod expr;
mod stmt;
mod tracker;

use thiserror::Error;

use crate::lex::{LexError, Lexer, Token};
use crate::opcode::{Chunk, FunctionId};
use tracker::ChunkTracker;

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

pub fn chunk(s: Lexer) -> Result<Chunk, LexParseError> {
    let mut tracker = ChunkTracker::empty();

    match block(s, &mut tracker) {
        Ok((mut s, ())) => match s.next_token() {
            Err(LexParseError::Eof) => (),
            _ => return Err(ParseError.into()),
        },
        Err(LexParseError::Eof) => (),
        Err(err) => return Err(err),
    }

    tracker.resolve().map_err(|_| ParseError.into())
}

fn block<'s>(
    s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    tracker.push_block();

    let r = inner_block(s, tracker);

    tracker.pop_block().unwrap();

    r
}

fn inner_block<'s>(
    mut s: Lexer<'s>,
    tracker: &mut ChunkTracker<'s>,
) -> Result<(Lexer<'s>, ()), LexParseError> {
    loop {
        s = match stmt::statement(s.clone(), tracker) {
            Ok((s, ())) => s,
            Err(_) => break,
        };
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
    tracker.push_frame();

    // Currently this slot contains pointer to function itself.
    // In the future we will put environment here instead.
    tracker.push_stack(None);

    let mut parlist = |mut s: Lexer<'s>| -> Result<_, LexParseError> {
        let mut count = 0;

        let ident = match s.next_token()? {
            Token::Ident(ident) => ident,
            _ => return Err(ParseError.into()),
        };
        tracker.push_stack(Some(ident));
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
            tracker.push_stack(Some(ident));
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
    let func_id = tracker.pop_frame(height).map_err(|_| ParseError)?;

    Ok((s, func_id))
}
