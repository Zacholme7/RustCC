use crate::asmgen::*;
use std::fmt;

impl fmt::Display for AsmProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let AsmProgram::Program(asm_func_def) = self;
        write!(f, "{}", asm_func_def)?;
        writeln!(f, ".section .note.GNU-stack,\"\",@progbits")
    }
}

impl fmt::Display for AsmFunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let AsmFunctionDefinition::Function(iden, instructions) = self;
        writeln!(f, "\t.globl {}", iden)?;
        writeln!(f, "{}:", iden)?;
        writeln!(f, "\tpushq %rbp")?;
        writeln!(f, "\tmovq  %rsp, %rbp")?;
        for instr in instructions {
            write!(f, "{}", instr)?;
        }
        Ok(())
    }
}

impl fmt::Display for AsmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmInstruction::Mov(op1, op2) => writeln!(f, "\tmovl  {}, {}", op1, op2),
            AsmInstruction::Unary(unop, op) => writeln!(f, "\t{}  {}", unop, op),
            AsmInstruction::AllocateStack(offset) => writeln!(f, "\tsubq  ${}, %rsp", offset),
            AsmInstruction::Ret => {
                writeln!(f, "\tmovq  %rbp, %rsp")?;
                writeln!(f, "\tpopq  %rbp")?;
                writeln!(f, "\tret")
            }
        }
    }
}

impl fmt::Display for AsmOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmOperand::Reg(reg) => write!(f, "{}", reg),
            AsmOperand::Stack(offset) => write!(f, "{}(%rbp)", offset),
            AsmOperand::Imm(value) => write!(f, "${}", value),
            _ => panic!("Cannot produce codegen for op"),
        }
    }
}

impl fmt::Display for AsmReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmReg::AX => write!(f, "%eax"),
            AsmReg::R10 => write!(f, "%r10d"),
        }
    }
}

impl fmt::Display for AsmUnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmUnOp::Neg => write!(f, "negl"),
            AsmUnOp::Not => write!(f, "notl"),
        }
    }
}
