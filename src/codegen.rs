use std::fmt;
use crate::parser::{Expression, Statement, FunctionDefinitionAst, ProgramAst};


type Identifier = String;

// Assembly language tree representation
pub enum ProgramAsm {
    Program(FunctionDefinitionAsm)
}

pub enum FunctionDefinitionAsm {
    Function(Identifier, Vec<Instruction>)
}

pub enum Instruction {
    Mov(Operand, Operand),
    Ret
}

pub enum Operand {
    Imm(usize),
    Register
}

/// Parse AST into ASM tree
pub fn ast_to_asm(
    ProgramAst::Program(ast): ProgramAst
) -> ProgramAsm {
    ProgramAsm::Program(asm_from_function(ast))
}

fn asm_from_function(
    FunctionDefinitionAst::Function(identifier, body): FunctionDefinitionAst
) -> FunctionDefinitionAsm {
    FunctionDefinitionAsm::Function(
        identifier,
        instructions_from_statement(body)
    )
}

fn instructions_from_statement(Statement::Return(stmt): Statement) -> Vec<Instruction> {
    let mut instrs : Vec<Instruction> = Vec::new();

    let num = match stmt {
        Expression::Constant(num) => num,
        Expression::Unary(_, _) => todo!()
    };

    instrs.push(
        Instruction::Mov(
            Operand::Imm(num),
            Operand::Register
    ));
    instrs.push(Instruction::Ret);
    instrs
}

// Display implementations for codegen
impl fmt::Display for ProgramAsm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProgramAsm::Program(function_def) => write!(f, "{}", function_def)
        }
    }
}

impl fmt::Display for FunctionDefinitionAsm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionDefinitionAsm::Function(
                identifier,
                instructions
            ) => {
                writeln!(f, "\t.globl _{}", identifier)?;
                writeln!(f, "_{}:", identifier)?;
                for instr in instructions {
                    writeln!(f, "\t{}", instr)?;
                }
                Ok(())
            }
        }
    }
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