use std::env;
use std::fs;
use std::io::Write;
use clap::Parser;
use codegen::ProgramAsm;
use std::path::PathBuf;
use anyhow::Result;
use std::process::Command;

use crate::lexer::program_to_tokens;
use crate::parser::tokens_to_ast;
use crate::codegen::ast_to_asm;

mod lexer;
mod parser;
mod codegen;

#[derive(Parser)]
struct Args {
    // The source file
    file: PathBuf,
    // Signal to run the lexer
    #[arg(long)]
    lexer: bool,
    // Signal to run the parser
    #[arg(long)]
    parser: bool,
    // signal to run codegen
    #[arg(long)]
    codegen: bool,
}

#[derive(Debug, Clone, Copy)]
enum Stage {
    Lexer,
    Parser,
    Codegen,
}

fn main() -> Result<()> {
    // parse the args
    let args = Args::parse();

    // get all the stages we want to run
    let stages = if args.lexer || args.parser || args.codegen {
        vec![
            (Stage::Lexer, args.lexer),
            (Stage::Parser, args.parser),
            (Stage::Codegen, args.codegen),
        ]
        .into_iter()
        .filter(|&(_, enabled)| enabled)
        .map(|(stage, _)| stage)
        .collect()
    } else {
        vec![Stage::Lexer, Stage::Parser, Stage::Codegen]
    };

    // preprocess the c file
    let preprocessed_file = preprocess(args.file.clone());

    // outputs
    let mut tokens = None;
    let mut ast = None;
    let mut asm = None;

    // read in from the preprocessed file
    let program = fs::read_to_string(preprocessed_file).unwrap();

    // process all of the stages
    for stage in stages {
        match stage {
            Stage::Lexer => {
                tokens = Some(program_to_tokens(program.clone()));
            }
            Stage::Parser => {
                ast = Some(tokens_to_ast(tokens.take().expect("Lexer must be run before parser")));
            }
            Stage::Codegen => {
                asm = Some(ast_to_asm(ast.take().expect("Parser must be run before codegen")));
            }
        }
    }


    if let Some(asm) = asm {
        // write assembly to file
        let asm_file = args.file.with_extension("s");
        emit_asm(asm, asm_file);

        // assemble and link
        let assembly_file = args.file.with_extension("s");
        assemble_and_link(assembly_file);
    }
    Ok(())
}


pub fn emit_asm(asm: ProgramAsm, file_name: PathBuf) {
    let mut file = fs::File::create(file_name).unwrap();
    file.write_all(format!("{}", asm).as_bytes()).unwrap();
}

fn preprocess(file_name: PathBuf) -> PathBuf {
    let output_file = file_name.with_extension("i");
    Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(file_name.as_os_str())
        .arg("-o")
        .arg(output_file.clone())
        .output().unwrap();
    output_file
}

fn assemble_and_link(file_name: PathBuf) {
    Command::new("gcc")
        .arg(file_name.clone())
        .arg("-o")
        .arg(file_name.file_stem().unwrap())
        .output().unwrap();
}