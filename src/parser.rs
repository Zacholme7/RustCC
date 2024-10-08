use crate::errors::CompileError;
use crate::lexer::KeywordType;
use crate::lexer::Token;
use std::iter::Peekable;
use std::vec::IntoIter;

// ASDL ast defintion
type Identifier = String;

// program = Program(function_definition)
#[derive(Debug)]
pub enum ProgramAst {
    Program(FunctionDefinitionAst),
}

// function_definition = Function(identifier name, statement body)
#[derive(Debug)]
pub enum FunctionDefinitionAst {
    Function(Identifier, Statement),
}

// statement = Return(exp)
#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

// exp = Constant(int) | Unary(unary_operator, exp)
#[derive(Debug)]
pub enum Expression {
    Constant(usize),
    Unary(UnaryOperator, Box<Expression>),
}

// unary_operator = Complement | Negate
#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
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
    pub fn parse_program(&mut self) -> Result<ProgramAst, CompileError> {
        let function = self.parse_function()?;
        // make sure there is nothing remaining
        if self.tokens.next().is_some() {
            return Err(CompileError::InvalidParse(
                "Extra token at end of file".to_string(),
            ));
        }
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
            _ => Err(CompileError::InvalidParse(
                "Expected identifier".to_string(),
            )),
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
        let next_token = self.tokens.peek();
        match next_token {
            Some(Token::Number(_)) => {
                let constant = self.parse_int()?;
                Ok(constant)
            }
            Some(Token::Tilde) | Some(Token::Hyphen) => {
                let operator = self.parse_unop()?;
                let inner_exp = self.parse_expression()?;
                Ok(Expression::Unary(operator, Box::new(inner_exp)))
            }
            Some(Token::OpenParen) => {
                self.tokens.next();
                let inner_exp = self.parse_expression()?;
                self.expect(Token::CloseParen)?;
                Ok(inner_exp)
            }
            _ => Err(CompileError::InvalidParse(
                "Malformed expression".to_string(),
            )),
        }
    }

    //<unop> ::= "-" | "~"
    fn parse_unop(&mut self) -> Result<UnaryOperator, CompileError> {
        match self.tokens.next() {
            Some(Token::Tilde) => Ok(UnaryOperator::Complement),
            Some(Token::Hyphen) => Ok(UnaryOperator::Negate),
            _ => Err(CompileError::InvalidParse(
                "Expected unary operator".to_string(),
            )),
        }
    }

    //<int> ::= ? A constant token ?
    fn parse_int(&mut self) -> Result<Expression, CompileError> {
        match self.tokens.next() {
            Some(Token::Number(num)) => Ok(Expression::Constant(num)),
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
}

