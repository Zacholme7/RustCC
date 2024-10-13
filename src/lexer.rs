use crate::errors::CompileError;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    // regular expression that will match each specific token
    static ref TOKEN_REGEX: Vec<(&'static str, Regex)> = vec![
        ("Identifier", Regex::new(r"^[a-zA-Z_]\w*\b").unwrap()),
        ("Constant", Regex::new(r"^[0-9]+\b").unwrap()),
        ("Open Brace", Regex::new(r"^\{").unwrap()),
        ("Close Brace", Regex::new(r"^\}").unwrap()),
        ("Open Paren", Regex::new(r"^\(").unwrap()),
        ("Close Paren", Regex::new(r"^\)").unwrap()),
        ("Semicolon", Regex::new(r"^;").unwrap()),
        ("Tilde", Regex::new(r"^~").unwrap()),
        ("Two Hyphen", Regex::new(r"^--").unwrap()),
        ("Hyphen", Regex::new(r"^-").unwrap()),
    ];
}

// All of the tokens that our Lexer recognizes
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(KeywordType),
    Identifier(String),
    Number(usize),
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Tilde,
    TwoHyphen,
    Hyphen,
    Semicolon,
}

// Reserved Keywords
#[derive(Debug, Clone, PartialEq)]
pub enum KeywordType {
    Int,
    Void,
    Return,
}

pub fn program_to_tokens(program: &str) -> Result<Vec<Token>, CompileError> {
    let mut tokens = Vec::new();
    let mut remaining = program.trim();

    // while we have parts of the program left to lex
    while !remaining.is_empty() {
        // see if we have a match
        if let Some((_, regex)) = TOKEN_REGEX.iter().find(|(_, r)| r.is_match(remaining)) {
            // extract this match
            let mat = regex.find(remaining).unwrap();
            let tok_str = mat.as_str();

            // figure out which match we have
            let token = match tok_str {
                "{" => Token::OpenBrace,
                "}" => Token::CloseBrace,
                "(" => Token::OpenParen,
                ")" => Token::CloseParen,
                "~" => Token::Tilde,
                "--" => Token::TwoHyphen,
                "-" => Token::Hyphen,
                ";" => Token::Semicolon,
                _ if tok_str.chars().next().unwrap().is_alphabetic() => {
                    if ["int", "void", "return"].contains(&tok_str) {
                        match tok_str {
                            "int" => Token::Keyword(KeywordType::Int),
                            "void" => Token::Keyword(KeywordType::Void),
                            "return" => Token::Keyword(KeywordType::Return),
                            _ => panic!("Will not reach here"),
                        }
                    } else {
                        Token::Identifier(tok_str.to_string())
                    }
                }
                _ if tok_str.chars().next().unwrap().is_numeric() => {
                    Token::Number(tok_str.parse::<usize>().unwrap())
                }
                _ => {
                    return Err(CompileError::InvalidLex(format!(
                        "Unable to tokenize: {}",
                        remaining
                    )))
                }
            };
            tokens.push(token);
            remaining = &remaining[mat.end()..].trim();
        } else {
            return Err(CompileError::InvalidLex(format!(
                "Unable to tokenize: {}",
                remaining
            )));
        }
    }

    Ok(tokens)
}
