use anyhow::Result;
use clap::Parser as CmdParser;
use std::fs;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use crate::asmgen::{generate_asm_ast, AsmProgram};
use crate::lexer::program_to_tokens;
use crate::parser::Parser;
use crate::tackygen::generate_tacky_ast;

mod asmgen;
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
                println!("The tokens are {:#?}", tokens);
            }
            Stage::Parser => {
                let mut parser = Parser::new(tokens.take().unwrap());
                ast = Some(parser.parse_program()?);
                println!("The ast is {:#?}", ast);
            }
            Stage::TackyGen => {
                tacky_ast = Some(generate_tacky_ast(
                    ast.take().expect("Parser must run before tacky generation"),
                )?);
                println!("The tacky ast it {:#?}", tacky_ast);
            }
            Stage::Codegen => {
                asm = Some(generate_asm_ast(tacky_ast.take().expect("blah"))?);
                println!("The asm ast is {:#?}", asm);
            }
        }
    }

    if let Some(asm) = asm {
        // write assembly to file
        let asm_file = args.file.with_extension("s");
        emit_asm(&asm, &asm_file);

        // assemble and link
        let executable = assemble_and_link(&asm_file).unwrap();
        let _ = fs::remove_file(executable);
    }
    Ok(())
}

pub fn emit_asm(asm: &AsmProgram, file_name: &Path) {
    let mut file = fs::File::create(file_name).unwrap();
    file.write_all(format!("{}", asm).as_bytes()).unwrap()
}

fn assemble_and_link(asm_file: &Path) -> Result<PathBuf> {
    let output_file = asm_file.with_extension("");
    Command::new("gcc")
        .arg(asm_file)
        .arg("-o")
        .arg(&output_file)
        .output()
        .unwrap();
    Ok(output_file)
}

// The preprocess function remains the same
fn preprocess(file_name: PathBuf) -> PathBuf {
    let output_file = file_name.with_extension("i");
    Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(file_name.as_os_str())
        .arg("-o")
        .arg(output_file.clone())
        .output()
        .expect("Failed to preprocess file");
    output_file
}
