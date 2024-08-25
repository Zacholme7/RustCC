use std::{env, vec::IntoIter};
use regex::Regex;
use std::fmt;
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
    let asm = parse_function_asm(ast);
    println!("{}", asm);

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

fn parse_function_asm(AST::Program(pg): AST) -> Assembly{
    Assembly::Program(fn_def_to_asm(pg))
}

fn fn_def_to_asm(
    FunctionDefinition::Function(identifier, body): FunctionDefinition
) -> FunctionDefinitionAsm {
    FunctionDefinitionAsm::FunctionAsm(
        identifier,
        get_inst_from_body(body)
    )
}

fn get_inst_from_body(Body::Return(exp): Body) -> Vec<Instruction> {
    let mut instrs : Vec<Instruction> = Vec::new();

    if let Exp::Constant(num) = exp {
        instrs.push(
            Instruction::Mov(
                Operand::Imm(num),
                Operand::Register
            ));
    }
    instrs.push(Instruction::Ret);
    instrs
}

#[derive(Debug)]
enum Assembly {
    Program(FunctionDefinitionAsm)
}



impl fmt::Display for Assembly {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Assembly::Program(pg) => write!(f, "{}", pg)
        }
    }
}


#[derive(Debug)]
enum FunctionDefinitionAsm {
    FunctionAsm(Identifier, Vec<Instruction>)
}

impl fmt::Display for FunctionDefinitionAsm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionDefinitionAsm::FunctionAsm(
                identifier,
                instructions
            ) => {
                writeln!(f, "\t.globl {}", identifier);
                writeln!(f, "{}:", identifier);
                for instr in instructions {
                    writeln!(f, "\t{}", instr);
                }
                Ok(())
            }
        }
    }
}



#[derive(Debug)]
enum Instruction {
    Mov(Operand, Operand),
    Ret
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov(op1, op2) => {
                write!(f, "movl {}, {}", op1, op2)
            }
            Instruction::Ret => write!(f, "ret")
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Register => write!(f, "%eax"),
            Operand::Imm(number) => write!(f, "${}", number)
        }
    }
}

#[derive(Debug)]
enum Operand {
    Imm(usize), 
    Register
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
