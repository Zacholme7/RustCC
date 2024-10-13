use anyhow::Result;
use clap::Parser as CmdParser;
use codegen::ProgramAsm;
use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

use crate::codegen::ast_to_asm;
use crate::lexer::{program_to_tokens, Token};
use crate::parser::Parser;
use crate::tackygen::generate_tacky_ast;

mod codegen;
mod errors;
mod lexer;
mod parser;
mod tackygen;

#[derive(CmdParser)]
struct Args {
    // The source file
    file: PathBuf,
    // Signal to run the lexer
    #[arg(long)]
    lex: bool,
    // Signal to run the parser
    #[arg(long)]
    parse: bool,
    // Signal to run tacky generation
    #[arg(long)]
    tacky: bool,
    // signal to run codegen
    #[arg(long)]
    codegen: bool,
}

#[derive(Debug, Clone, Copy)]
enum Stage {
    Lexer,
    Parser,
    TackyGen,
    Codegen,
}

fn main() -> Result<()> {
    // parse the args
    let args = Args::parse();

    // get all the stages we want to run
    let stages = if args.codegen {
        vec![Stage::Lexer, Stage::Parser, Stage::TackyGen, Stage::Codegen]
    } else if args.tacky {
        vec![Stage::Lexer, Stage::Parser, Stage::TackyGen]
    } else if args.parse {
        vec![Stage::Lexer, Stage::Parser]
    } else if args.lex {
        vec![Stage::Lexer]
    } else {
        vec![Stage::Lexer, Stage::Parser, Stage::TackyGen, Stage::Codegen]
    };

    // preprocess the c file
    let preprocessed_file = preprocess(args.file.clone());

    // outputs
    let mut tokens = None;
    let mut ast = None;
    let mut tacky_ast = None;
    let mut asm = None;

    // read in from the preprocessed file
    let program = fs::read_to_string(preprocessed_file).unwrap();

    // process all of the stages
    for stage in stages {
        match stage {
            Stage::Lexer => {
                tokens = Some(program_to_tokens(program.as_str())?);
                println!("The tokens are {:?}", tokens);
            }
            Stage::Parser => {
                let mut parser = Parser::new(tokens.take().unwrap());
                ast = Some(parser.parse_program()?);
                println!("The ast is {:?}", ast);

            }
            Stage::TackyGen => {
                tacky_ast = Some(generate_tacky_ast(ast.take().expect("Parser must run before tacky generation"))?);
                println!("The tacky ast it {:?}", tacky_ast);
            }
            Stage::Codegen => {
                asm = Some(ast_to_asm(
                    ast.take().expect("Parser must be run before codegen"),
                ));
            }
        }
    }

    if let Some(asm) = asm {
        // write assembly to file
        let asm_file = args.file.with_extension("s");
        emit_asm(asm, asm_file.clone());

        // assemble and link
        assemble_and_link(asm_file.clone());
        //fs::remove_file(asm_file);
    }
    Ok(())
}

pub fn emit_asm(asm: ProgramAsm, file_name: PathBuf) {
    let mut file = fs::File::create(file_name).unwrap();
    file.write_all(format!("{}", asm).as_bytes()).unwrap();
}

// Compile driver

// 1) Preprocess the file
// -E: tells gcc to only run the preprocessor
// -P: tells gcc not to emit any linemarkets
fn preprocess(file_name: PathBuf) -> PathBuf {
    let output_file = file_name.with_extension("i");
    Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(file_name.as_os_str())
        .arg("-o")
        .arg(output_file.clone())
        .output()
        .unwrap();
    output_file
}

// 2) Our compiler will run and produce a .s file

// 3) Assemble and link the file to produce executable
fn assemble_and_link(file_name: PathBuf) {
    Command::new("gcc")
        .arg(file_name.clone())
        .arg("-o")
        .arg(file_name.file_stem().unwrap())
        .output()
        .unwrap();
}
