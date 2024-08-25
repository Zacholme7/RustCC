use std::vec::IntoIter;

type Identifier = String;

// represent the abstract sytax tree
pub enum ProgramAst {
    Program(FunctionDefinitionAst)
}

pub enum FunctionDefinitionAst {
    Function(Identifier, Statement)
}

pub enum Statement {
    Return(Expression)
}

pub enum Expression {
    Constant(usize)
}


pub fn tokens_to_ast(tokens: Vec<String>) -> ProgramAst {
    // convert tokens into iterator for easy consuming
    let mut tokens = tokens.into_iter();
    ProgramAst::Program(program_from_ast(&mut tokens))
}

fn program_from_ast(tokens: &mut IntoIter<String>) -> FunctionDefinitionAst {
    expect("int", tokens);

    let identifier: Identifier = tokens.next().unwrap();

    expect("(", tokens);
    expect("void", tokens);
    expect(")", tokens);
    expect("{", tokens);

    let statement = statement_from_function(tokens);

    expect("}", tokens);

    FunctionDefinitionAst::Function(identifier, statement)
}

fn statement_from_function(tokens: &mut IntoIter<String>) -> Statement {
    expect("return", tokens);
    let exp = expression_from_statement(tokens);
    expect(";", tokens);
    Statement::Return(exp)
}

fn expression_from_statement(tokens: &mut IntoIter<String>) -> Expression{
    let exp: usize = tokens.next().unwrap().parse::<usize>().unwrap();
    Expression::Constant(exp)
}

fn expect(expected: &str, tokens: &mut IntoIter<String>)  {
    let next_token = tokens.next().unwrap();
    assert_eq!(expected, next_token);
}