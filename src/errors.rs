use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum CompileError {
    InvalidLex(String),
    InvalidParse(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::InvalidLex(msg) => write!(f, "Lexer error: {}", msg),
            CompileError::InvalidParse(msg) => write!(f, "Parser error: {}", msg),
        }
    }
}

impl Error for CompileError {}
