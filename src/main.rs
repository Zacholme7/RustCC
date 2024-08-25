use std::{env, vec::IntoIter};
use regex::Regex;
use std::fs;
fn main() {

    // get the args and confirm we have a path
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("Please run the compiler with your .c file")
    }

    // read in the contents
    let file = &args[1];
    let program = fs::read_to_string(file).unwrap();

    // setup all of our token regex
    let token_rx= vec![
        ("Identifier", Regex::new(r"[a-zA-Z_]\w*\b").unwrap()),
        ("Constant", Regex::new(r"[0-9]+\b").unwrap()),
        ("Open Paren", Regex::new(r"\(").unwrap()),
        ("Close Paren", Regex::new(r"\)").unwrap()),
        ("Open Brace", Regex::new(r"\{").unwrap()),
        ("Close Brace", Regex::new(r"\}").unwrap()),
        ("Semicolon brace", Regex::new(r";").unwrap()),
    ];

    // go through the input
    let mut position = 0;

    let mut tokens = Vec::new();
    while position < program.len() {
        let slice = &program[position..];
        let mut matched = false;

        // go through all of the regexs
        for (_, regex) in &token_rx {
            // if we find one
            if let Some(mat) = regex.find(slice) {
                // make sure it is the next element in the input
                if mat.start() == 0 {
                    tokens.push(mat.as_str());
                    matched = true;
                    position += mat.end();
                    break;
                }
            }
        }
        if !matched { position += 1; }
    }
    println!("{:?}", tokens);

    let ast = parse_ast(tokens);
    println!("{:#?}", ast);
}

fn parse_ast(mut tokens: Vec<&str>) -> AST{
    let tokens = tokens.into_iter();
    let program = parse_function(tokens);
    AST::Program(program)
}

fn parse_function(mut tokens: IntoIter<&str>) -> FunctionDefinition {
    let ret_type = tokens.next().unwrap();
    assert_eq!("int", ret_type);
    let identifier: Identifier = tokens.next().unwrap().to_string();
    assert_eq!("(", tokens.next().unwrap());
    assert_eq!("void", tokens.next().unwrap());
    assert_eq!(")", tokens.next().unwrap());
    assert_eq!("{", tokens.next().unwrap());
    let statement = parse_statement(&mut tokens);
    assert_eq!("}", tokens.next().unwrap());
    FunctionDefinition::Function(identifier, statement)

}

fn parse_statement(tokens: &mut IntoIter<&str>) -> Body{
    assert_eq!("return", tokens.next().unwrap());
    let exp = parse_expression(tokens);
    assert_eq!(";", tokens.next().unwrap());
    Body::Return(exp)
}

fn parse_expression(tokens: &mut IntoIter<&str>) -> Exp{
    let exp: usize = tokens.next().unwrap().parse::<usize>().unwrap();
    Exp::Constant(exp)
}

type Identifier = String;

#[derive(Debug)]
enum AST {
    Program(FunctionDefinition)
}

#[derive(Debug)]
enum FunctionDefinition {
    Function(Identifier, Body)
}

#[derive(Debug)]
enum Body {
    Return(Exp)
}

#[derive(Debug)]
enum Exp {
    Constant(usize)
}
