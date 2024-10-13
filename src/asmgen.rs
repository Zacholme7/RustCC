use crate::errors::CompileError;
use crate::tackygen::*;
use std::collections::HashMap;

//program = Program(function_definition)
#[derive(Debug, Clone)]
pub enum AsmProgram {
    Program(AsmFunctionDefinition),
}

//function_definition = Function(identifier name, instruction* instructions)
#[derive(Debug, Clone)]
pub enum AsmFunctionDefinition {
    Function(String, Vec<AsmInstruction>),
}

//instruction = Mov(operand src, operand dst)
// | Unary(unary_operator, operand)
// | AllocateStack(int)
// | Ret
#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Mov(AsmOperand, AsmOperand),
    Unary(AsmUnOp, AsmOperand),
    AllocateStack(i32),
    Ret,
}

//unary_operator = Neg | Not
#[derive(Debug, Clone)]
pub enum AsmUnOp {
    Neg,
    Not,
}

// operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum AsmOperand {
    Imm(usize),
    Reg(AsmReg),
    Pseudo(String),
    Stack(i32),
}

//reg = AX | R10
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum AsmReg {
    AX,
    R10,
}

// Parse the tacky ast and convert it into an asm ast
pub fn generate_asm_ast(tacky_ast: TackyProgram) -> Result<AsmProgram, CompileError> {
    // first pass to translate into asm ast
    let TackyProgram::Program(tacky_func_def) = tacky_ast;
    let asm_function = generate_asm_function(tacky_func_def)?;
    Ok(AsmProgram::Program(asm_function))
}

// Parse the tacky function definition and produce an asm function definition
fn generate_asm_function(
    tacky_function: TackyFunctionDefinition,
) -> Result<AsmFunctionDefinition, CompileError> {
    // 1) translate tacky instructions into asm instructions
    let TackyFunctionDefinition::Function(iden, instructions) = tacky_function;
    let mut asm_instructions = generate_asm_instructions(instructions)?;

    // 2) replace pseudoregisters with stack offsets
    let mut offset_map: HashMap<AsmOperand, i32> = HashMap::new();
    let mut next_offset = 0;

    // closure to replace pseudo operands
    let mut replace_pseudo = |op: &mut AsmOperand| {
        if let AsmOperand::Pseudo(_) = op {
            let offset = *offset_map.entry(op.clone()).or_insert_with(|| {
                next_offset -= 4;
                next_offset
            });
            *op = AsmOperand::Stack(offset);
        }
    };

    for asm_instruction in asm_instructions.iter_mut() {
        match asm_instruction {
            AsmInstruction::Mov(op1, op2) => {
                replace_pseudo(op1);
                replace_pseudo(op2);

            }
            AsmInstruction::Unary(_, op) => {
                replace_pseudo(op);

            }
            _ => continue,
        }
    }

    // 3) fix up instructions
    let mut fixed_asm_instructions: Vec<AsmInstruction> = vec![];
    asm_instructions.insert(0, AsmInstruction::AllocateStack(next_offset));
    for asm_instruction in asm_instructions.iter_mut() {
        match asm_instruction {
            AsmInstruction::Mov(op1, op2) => {
                if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (op1.clone(), op2.clone()) {
                    fixed_asm_instructions.push(AsmInstruction::Mov(op1.clone(), AsmOperand::Reg(AsmReg::R10)));
                    fixed_asm_instructions.push(AsmInstruction::Mov(AsmOperand::Reg(AsmReg::R10), op2.clone()));
                } else {
                    fixed_asm_instructions.push(asm_instruction.clone())
                }
            }
            _ => fixed_asm_instructions.push(asm_instruction.clone())
        }
    }
    Ok(AsmFunctionDefinition::Function(iden, fixed_asm_instructions))
}

// Parse the tacky instructions and generate asm instructions
fn generate_asm_instructions(
    tacky_instructions: Vec<TackyInstruction>,
) -> Result<Vec<AsmInstruction>, CompileError> {
    let mut asm_instructions: Vec<AsmInstruction> = vec![];
    for tacky_instruction in tacky_instructions {
        match tacky_instruction {
            TackyInstruction::Return(tacky_val) => {
                let asm_val = generate_asm_operand(tacky_val)?;
                asm_instructions.push(AsmInstruction::Mov(asm_val, AsmOperand::Reg(AsmReg::AX)));
                asm_instructions.push(AsmInstruction::Ret);
            }
            TackyInstruction::Unary(tacky_un_op, tacky_val1, tacky_val2) => {
                let asm_src = generate_asm_operand(tacky_val1)?;
                let asm_dst = generate_asm_operand(tacky_val2)?;
                let asm_un_op = generate_asm_unop(tacky_un_op)?;
                asm_instructions.push(AsmInstruction::Mov(asm_src, asm_dst.clone()));
                asm_instructions.push(AsmInstruction::Unary(asm_un_op, asm_dst));
            }
        }
    }
    Ok(asm_instructions)
}

// Parse a tacky value into an asm operand
fn generate_asm_operand(tacky_val: TackyVal) -> Result<AsmOperand, CompileError> {
    match tacky_val {
        TackyVal::Constant(num) => Ok(AsmOperand::Imm(num)),
        TackyVal::Var(iden) => Ok(AsmOperand::Pseudo(iden)),
    }
}

// Parse a tacky unary operator into a asm unary operator
fn generate_asm_unop(tacky_unop: TackyUnaryOp) -> Result<AsmUnOp, CompileError> {
    match tacky_unop {
        TackyUnaryOp::Complement => Ok(AsmUnOp::Not),
        TackyUnaryOp::Negate => Ok(AsmUnOp::Neg),
    }
}
