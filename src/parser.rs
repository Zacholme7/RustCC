use std::vec::IntoIter;
use crate::errors::CompileError;
use crate::lexer::Token;
use crate::lexer::KeywordType;

// ASDL ast defintion
type Identifier = String;

#[derive(Debug)]
pub enum ProgramAst {
    Program(FunctionDefinitionAst)
}
#[derive(Debug)]
pub enum FunctionDefinitionAst {
    Function(Identifier, Statement)
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression)
}

#[derive(Debug)]
pub enum Expression {
    Constant(usize)
}

#[derive(Debug)]
pub struct Parser {
    tokens: IntoIter<Token>
}


impl Parser {
    pub fn new(tokens: Vec<Token> ) -> Self {
        Parser {tokens : tokens.into_iter()}
    }

    pub fn parse_program(&mut self) -> Result<ProgramAst, CompileError> {
        let function = self.parse_function()?;
        // make sure there is nothing remaining
        if self.tokens.next() != None { return Err(CompileError::InvalidParse(format!("Extra tokens at end of file"))); }
        Ok(ProgramAst::Program(function))
    }

    fn parse_function(&mut self) -> Result<FunctionDefinitionAst, CompileError> {
        self.expect(Token::Keyword(KeywordType::Int))?;
        let identifier = self.parse_identifier()?; 
        self.expect(Token::OpenParen)?;
        self.expect(Token::Keyword(KeywordType::Void))?;
        self.expect(Token::CloseParen)?;
        self.expect(Token::OpenBrace)?;
        let statement = self.parse_statement()?;
        self.expect(Token::Semicolon)?;
        self.expect(Token::CloseBrace)?;
        Ok(FunctionDefinitionAst::Function(identifier, statement))
    }

    fn parse_identifier(&mut self) -> Result<Identifier, CompileError> {
        match self.tokens.next() {
            Some(Token::Identifier(ident)) => Ok(ident),
            _ => Err(CompileError::InvalidParse("Expected identifier".to_string()))
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, CompileError> {
        self.expect(Token::Keyword(KeywordType::Return))?;
        let expression = self.parse_expression()?;
        Ok(Statement::Return(expression))
    }

    fn parse_expression(&mut self) -> Result<Expression, CompileError> {
        match self.tokens.next() {
            Some(Token::Number(num)) => Ok(Expression::Constant(num)),
            _ => Err(CompileError::InvalidParse("Expected expression".to_string()))
        }
    }

    pub fn expect(&mut self, expected_token: Token) -> Result<bool, CompileError> {
        let current_token = self.tokens.next().unwrap();
        
        if current_token != expected_token {
            Err(CompileError::InvalidParse(format!("Expected {:?}, Got {:?}", current_token, expected_token)))
        } else {
            Ok(true)
        }
    }

}