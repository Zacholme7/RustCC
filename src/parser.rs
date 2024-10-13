use std::vec::IntoIter;
use std::iter::Peekable;
use crate::errors::CompileError;
use crate::lexer::Token;
use crate::lexer::KeywordType;

// ASDL ast defintion
type Identifier = String;


// program = Program(function_definition)
#[derive(Debug)]
pub enum ProgramAst {
    Program(FunctionDefinitionAst)
}

// function_definition = Function(identifier name, statement body)
#[derive(Debug)]
pub enum FunctionDefinitionAst {
    Function(Identifier, Statement)
}

// statement = Return(exp)
#[derive(Debug)]
pub enum Statement {
    Return(Expression)
}

// exp = Constant(int) | Unary(unary_operator, exp)
#[derive(Debug)]
pub enum Expression {
    Constant(usize),
    Unary(UnaryOp, Box<Expression>)
}

// unary_operator = Complement | Negate
#[derive(Debug)]
pub enum UnaryOp {
    Complement,
    Negate
}


#[derive(Debug)]
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>
}


impl Parser {
    pub fn new(tokens: Vec<Token> ) -> Self {
        Parser {tokens : tokens.into_iter().peekable()}
    }

    // <program> ::= <function>
    pub fn parse_program(&mut self) -> Result<ProgramAst, CompileError> {
        let function = self.parse_function()?;
        // make sure there is nothing remaining
        if self.tokens.next() != None { return Err(CompileError::InvalidParse(format!("Extra tokens at end of file"))); }
        Ok(ProgramAst::Program(function))
    }

    // <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
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


    // <identifier> ::= ? An identifier token ?
    fn parse_identifier(&mut self) -> Result<Identifier, CompileError> {
        match self.tokens.next() {
            Some(Token::Identifier(ident)) => Ok(ident),
            _ => Err(CompileError::InvalidParse("Expected identifier".to_string()))
        }
    }


    // <statement> ::= "return" <exp> ";"
    fn parse_statement(&mut self) -> Result<Statement, CompileError> {
        self.expect(Token::Keyword(KeywordType::Return))?;
        let expression = self.parse_expression()?;
        Ok(Statement::Return(expression))
    }

    // <exp> ::= <int> | <unop> <exp> | "(" <exp> ")"
    fn parse_expression(&mut self) -> Result<Expression, CompileError> {
        let peeked_token = match self.tokens.peek() {
            Some(token) => token.clone(),
            None => return Err(CompileError::InvalidParse("Expected a token".to_string()))
        };

        match peeked_token {
            Token::Number(num) => {
                self.tokens.next();
                Ok(Expression::Constant(num))
            }
            Token::Tilde | Token::Hyphen => {
                let operator = self.parse_unop()?;
                let inner_exp = self.parse_expression()?;
                Ok(Expression::Unary(operator, Box::new(inner_exp)))
            }
            Token::OpenParen => {
                self.tokens.next();
                let inner_exp = self.parse_expression()?;
                self.expect(Token::CloseParen)?;
                Ok(inner_exp)
            }
            _ => Err(CompileError::InvalidParse("Expected expression".to_string()))
        }
    }

    // <unop> ::= "-" | "~"
    fn parse_unop(&mut self) -> Result<UnaryOp, CompileError> {
        match self.tokens.next() {
            Some(Token::Tilde) => Ok(UnaryOp::Complement),
            Some(Token::Hyphen) => Ok(UnaryOp::Negate),
            _ => Err(CompileError::InvalidParse("Expected unary opeartor".to_string()))
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