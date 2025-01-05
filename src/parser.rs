use crate::errors::CompileError;
use crate::lexer::KeywordType;
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

// ASDL ast defintion
type Identifier = String;

// program = Program(function_definition)
#[derive(Debug)]
pub enum AstProgram {
    Program(AstFunctionDefinition),
}

// function_definition = Function(identifier name, statement body)
#[derive(Debug)]
pub enum AstFunctionDefinition {
    Function(Identifier, AstStatement),
}

// statement = Return(exp)
#[derive(Debug)]
pub enum AstStatement {
    Return(AstExpression),
}

// exp = Constant(int) | Unary(unary_operator, exp)
#[derive(Debug)]
pub enum AstExpression {
    Constant(usize),
    Unary(AstUnaryOp, Box<AstExpression>),
    Binary(AstBinaryOp, Box<AstExpression>, Box<AstExpression>),
}

// unary_operator = Complement | Negate
#[derive(Debug)]
pub enum AstUnaryOp {
    Complement,
    Negate,
}

// binary_operator = Add | Subtract | Multiply | Divide | Remainder
#[derive(Debug)]
pub enum AstBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder
}

#[derive(Debug)]
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.into_iter().peekable(),
        }
    }

    // <program> ::= <function>
    pub fn parse_program(&mut self) -> Result<AstProgram, CompileError> {
        let function = self.parse_function()?;
        // make sure there is nothing remaining
        if self.tokens.next().is_some() {
            return Err(CompileError::InvalidParse(
                "Extra token at end of file".to_string(),
            ));
        }
        Ok(AstProgram::Program(function))
    }

    // <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
    fn parse_function(&mut self) -> Result<AstFunctionDefinition, CompileError> {
        self.expect(Token::Keyword(KeywordType::Int))?;
        let identifier = self.parse_identifier()?;
        self.expect(Token::OpenParen)?;
        self.expect(Token::Keyword(KeywordType::Void))?;
        self.expect(Token::CloseParen)?;
        self.expect(Token::OpenBrace)?;
        let statement = self.parse_statement()?;
        self.expect(Token::Semicolon)?;
        self.expect(Token::CloseBrace)?;
        Ok(AstFunctionDefinition::Function(identifier, statement))
    }

    // <identifier> ::= ? An identifier token ?
    fn parse_identifier(&mut self) -> Result<Identifier, CompileError> {
        match self.tokens.next() {
            Some(Token::Identifier(ident)) => Ok(ident),
            _ => Err(CompileError::InvalidParse(
                "Expected identifier".to_string(),
            )),
        }
    }

    // <statement> ::= "return" <exp> ";"
    fn parse_statement(&mut self) -> Result<AstStatement, CompileError> {
        self.expect(Token::Keyword(KeywordType::Return))?;
        let expression = self.parse_expression(0)?;
        Ok(AstStatement::Return(expression))
    }

    // <exp> ::= <factor> | <exp> <binop> <exp>
    fn parse_expression(&mut self, min_prec: usize) -> Result<AstExpression, CompileError> {
        let mut left = self.parse_factor()?;
        let mut next_token = self.tokens.peek().ok_or(CompileError::InvalidParse("Expected Token".to_string()))?.clone();
        let mut prec = self.get_prec(&next_token);
        while [Token::Hyphen, Token::ForwardSlash, Token::PercentSign, Token::Plus, Token::Asterisk].contains(&next_token) && prec >= min_prec {
            let operator = self.parse_binop()?;
            let right = self.parse_expression(prec + 1)?;
            left = AstExpression::Binary(operator, Box::new(left), Box::new(right));
            next_token = self.tokens.peek().ok_or(CompileError::InvalidParse("Expected Token".to_string()))?.clone();
            prec = self.get_prec(&next_token);
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<AstExpression, CompileError> {
        let next_token = self.tokens.peek();
        match next_token {
            Some(Token::Number(_)) => {
                let constant = self.parse_int()?;
                Ok(constant)
            }
            Some(Token::Tilde) | Some(Token::Hyphen) => {
                let operator = self.parse_unop()?;
                let inner_exp = self.parse_factor()?;
                Ok(AstExpression::Unary(operator, Box::new(inner_exp)))
            },
            Some(Token::OpenParen) => {
                self.tokens.next();
                let inner_exp = self.parse_expression(0)?;
                self.expect(Token::CloseParen)?;
                Ok(inner_exp)
            }
            _ => Err(CompileError::InvalidParse(
                "Expected expression".to_string(),
            )),
        }
    }


    // <unop> ::= "-" | "~"
    fn parse_unop(&mut self) -> Result<AstUnaryOp, CompileError> {
        match self.tokens.next() {
            Some(Token::Tilde) => Ok(AstUnaryOp::Complement),
            Some(Token::Hyphen) => Ok(AstUnaryOp::Negate),
            _ => Err(CompileError::InvalidParse(
                "Expected unary opeartor".to_string(),
            )),
        }
    }

    // <binop> ::= "-" | "+" | "*" | "/" | "%"
    fn parse_binop(&mut self) -> Result<AstBinaryOp, CompileError> {
        match self.tokens.next() {
            Some(Token::Hyphen) => Ok(AstBinaryOp::Subtract),
            Some(Token::Plus) => Ok(AstBinaryOp::Add),
            Some(Token::Asterisk) => Ok(AstBinaryOp::Multiply),
            Some(Token::ForwardSlash) => Ok(AstBinaryOp::Divide),
            Some(Token::PercentSign) => Ok(AstBinaryOp::Remainder),
            _ => Err(CompileError::InvalidParse(
                "Expected unary opeartor".to_string(),
            )),

        }
    }

    //<int> ::= ? A constant token ?
    fn parse_int(&mut self) -> Result<AstExpression, CompileError> {
        match self.tokens.next() {
            Some(Token::Number(num)) => Ok(AstExpression::Constant(num)),
            _ => Err(CompileError::InvalidParse("Expected integer".to_string())),
        }
    }

    // Consumes a token and compares it to a predefined expected value
    fn expect(&mut self, expected_token: Token) -> Result<bool, CompileError> {
        let current_token = self.tokens.next().unwrap();

        if current_token != expected_token {
            Err(CompileError::InvalidParse(format!(
                "Expected {:?}, Got {:?}",
                current_token, expected_token
            )))
        } else {
            Ok(true)
        }
    }

    // Gets the precedence level of a binary operator
    fn get_prec(&self, bin_op: &Token) -> usize {
        match bin_op {
            Token::Plus | Token::Hyphen => 45,
            Token::Asterisk | Token::ForwardSlash | Token::PercentSign => 50,
            _ => 0
        }
    }
}
