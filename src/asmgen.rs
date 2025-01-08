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

//instruction = 
// | Mov(operand src, operand dst)
// | Unary(unary_operator, operand)
// | Binary(binary_operator, operand, operand)
// | Idiv(operand)
// | Cdq
// | AllocateStack(int)
// | Ret
#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Mov(AsmOperand, AsmOperand),
    Unary(AsmUnOp, AsmOperand),
    Binary(AsmBinOp, AsmOperand, AsmOperand),
    Idiv(AsmOperand),
    Cdq,
    AllocateStack(i32),
    Ret,
}

//unary_operator = Neg | Not
#[derive(Debug, Clone)]
pub enum AsmUnOp {
    Neg,
    Not,
}

//binary_operator = Add | Sub | Mul
#[derive(Debug, Clone, PartialEq)]
pub enum AsmBinOp {
    Add,
    Sub,
    Mul
}

// operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum AsmOperand {
    Imm(usize),
    Reg(AsmReg),
    Pseudo(String),
    Stack(i32),
}

//reg = AX | DX | R10 | R11
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum AsmReg {
    AX,
    DX,
    R10,
    R11
}

// Parse the tacky ast and convert it into an asm ast
pub fn generate_asm_ast(tacky_ast: TackyProgram) -> Result<AsmProgram, CompileError> {
    let TackyProgram::Program(tacky_func_def) = tacky_ast;
    let asm_function = generate_asm_function(tacky_func_def)?;
    Ok(AsmProgram::Program(asm_function))
}

// Parse the tacky function definition and produce an asm function definition
fn generate_asm_function(
    tacky_function: TackyFunctionDefinition,
) -> Result<AsmFunctionDefinition, CompileError> {
    // 1) Translate tacky instructions into asm instructions
    let TackyFunctionDefinition::Function(iden, instructions) = tacky_function;
    let mut asm_instructions = generate_asm_instructions(instructions)?;

    // 2) Replace pseudoregisters with stack offsets
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
            AsmInstruction::Binary(_, op1, op2) => {
                replace_pseudo(op1);
                replace_pseudo(op2);
            }
            AsmInstruction::Idiv(op) => {
                replace_pseudo(op)
            }
            _ => continue,
        }
    }

    // 3) Fix up instructions
    // Need to fix up invalid Mov instructions. May have a case where both the source and
    // destination are stack operands. Need to introduce two instructions to move into a tmp and
    // then from a temp
    let mut fixed_asm_instructions: Vec<AsmInstruction> = vec![];
    asm_instructions.insert(0, AsmInstruction::AllocateStack(next_offset));
    for asm_instruction in asm_instructions.iter_mut() {
        match asm_instruction {
            // Mov instruction cannot take stack as both operanads. Fix it by moving to R10 first
            // and then from R10
            AsmInstruction::Mov(op1, op2) => {
                if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (op1.clone(), op2.clone()) {
                    fixed_asm_instructions.push(AsmInstruction::Mov(
                        op1.clone(),
                        AsmOperand::Reg(AsmReg::R10),
                    ));
                    fixed_asm_instructions.push(AsmInstruction::Mov(
                        AsmOperand::Reg(AsmReg::R10),
                        op2.clone(),
                    ));
                } else {
                    fixed_asm_instructions.push(asm_instruction.clone())
                }
            }
            // IDiv instruction cannot take a Immediate as the operand. fix it to move the value
            // into a register and then use the register in the operator 
            AsmInstruction::Idiv(op) => {
                if let AsmOperand::Imm(_) = op {
                    // mov imm, r10d
                    fixed_asm_instructions.push(AsmInstruction::Mov(
                        op.clone(),
                        AsmOperand::Reg(AsmReg::R10)
                    ));
                    // idiv r10d
                    fixed_asm_instructions.push(AsmInstruction::Idiv(
                        AsmOperand::Reg(AsmReg::R10)
                    ));
                } else {
                    fixed_asm_instructions.push(asm_instruction.clone())
                }
            }
            // Add and Sub instructions cannot take a memory address as both source and
            // destination. Move into a temporary and then operate on the temp
            AsmInstruction::Binary(bin_op, op1, op2) => {
                match bin_op {
                    AsmBinOp::Add | AsmBinOp::Sub => {
                        // movl 
                        if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (op1.clone(), op2.clone()) {
                            fixed_asm_instructions.push(AsmInstruction::Mov(
                                op1.clone(),
                                AsmOperand::Reg(AsmReg::R10),
                            ));
                            if *bin_op == AsmBinOp::Add {
                                fixed_asm_instructions.push(AsmInstruction::Binary(
                                    AsmBinOp::Add,
                                    AsmOperand::Reg(AsmReg::R10),
                                    op2.clone(),
                                ));
                            } else {
                                fixed_asm_instructions.push(AsmInstruction::Binary(
                                    AsmBinOp::Sub,
                                    AsmOperand::Reg(AsmReg::R10),
                                    op2.clone(),
                                ));
                            }
                        } else {
                            fixed_asm_instructions.push(asm_instruction.clone())
                        }
                    }
                    AsmBinOp::Mul => {
                        // cant use memory address as source or destination
                        // Load destination into R11: movl dest R11
                        fixed_asm_instructions.push(AsmInstruction::Mov(
                            op2.clone(),
                            AsmOperand::Reg(AsmReg::R11)
                        ));
                        // Multiply it by the source: imull source R11
                        fixed_asm_instructions.push(AsmInstruction::Binary(
                            AsmBinOp::Mul,
                            op1.clone(),
                            AsmOperand::Reg(AsmReg::R11)
                        ));
                        // Store it in the desination: movl R11 dest
                        fixed_asm_instructions.push(AsmInstruction::Mov(
                            AsmOperand::Reg(AsmReg::R11),
                            op1.clone()
                        ));
                    }
                }
            }

            _ => fixed_asm_instructions.push(asm_instruction.clone()),
        }
    }
    Ok(AsmFunctionDefinition::Function(
        iden,
        fixed_asm_instructions,
    ))
}

// Parse the tacky instructions and generate asm instructions
fn generate_asm_instructions(
    tacky_instructions: Vec<TackyInstruction>,
) -> Result<Vec<AsmInstruction>, CompileError> {
    let mut asm_instructions: Vec<AsmInstruction> = vec![];
    for tacky_instruction in tacky_instructions {
        match tacky_instruction {
            TackyInstruction::Return(tacky_val) => {
                let asm_val = generate_asm_operand(tacky_val);
                asm_instructions.push(AsmInstruction::Mov(asm_val, AsmOperand::Reg(AsmReg::AX)));
                asm_instructions.push(AsmInstruction::Ret);
            }
            TackyInstruction::Unary(tacky_un_op, tacky_val1, tacky_val2) => {
                let asm_src = generate_asm_operand(tacky_val1);
                let asm_dst = generate_asm_operand(tacky_val2);
                let asm_un_op = generate_asm_unop(tacky_un_op);
                asm_instructions.push(AsmInstruction::Mov(asm_src, asm_dst.clone()));
                asm_instructions.push(AsmInstruction::Unary(asm_un_op, asm_dst));
            }
            TackyInstruction::Binary(TackyBinaryOp::Divide, tacky_val1, tacky_val2, tacky_dest) => {
                let asm_src1 = generate_asm_operand(tacky_val1);
                let asm_src2 = generate_asm_operand(tacky_val2);
                let asm_dst = generate_asm_operand(tacky_dest);
                asm_instructions.push(AsmInstruction::Mov(asm_src1, AsmOperand::Reg(AsmReg::AX)));
                asm_instructions.push(AsmInstruction::Cdq);
                asm_instructions.push(AsmInstruction::Idiv(asm_src2));
                asm_instructions.push(AsmInstruction::Mov(AsmOperand::Reg(AsmReg::AX), asm_dst));
            }
            TackyInstruction::Binary(TackyBinaryOp::Remainder, tacky_val1, tacky_val2, tacky_dest) => {
                let asm_src1 = generate_asm_operand(tacky_val1);
                let asm_src2 = generate_asm_operand(tacky_val2);
                let asm_dst = generate_asm_operand(tacky_dest);
                asm_instructions.push(AsmInstruction::Mov(asm_src1, AsmOperand::Reg(AsmReg::AX)));
                asm_instructions.push(AsmInstruction::Cdq);
                asm_instructions.push(AsmInstruction::Idiv(asm_src2));
                asm_instructions.push(AsmInstruction::Mov(AsmOperand::Reg(AsmReg::DX), asm_dst));
            }
            TackyInstruction::Binary(tacky_bin_op, tacky_val1, tacky_val2, tacky_dest) => {
                let asm_src1 = generate_asm_operand(tacky_val1);
                let asm_src2 = generate_asm_operand(tacky_val2);
                let asm_dst = generate_asm_operand(tacky_dest);
                let asm_bin_op = generate_asm_binop(tacky_bin_op);
                asm_instructions.push(AsmInstruction::Mov(asm_src1, asm_dst.clone()));
                asm_instructions.push(AsmInstruction::Binary(asm_bin_op, asm_src2, asm_dst));
            }
        }
    }
    Ok(asm_instructions)
}

// Parse a tacky value into an asm operand
fn generate_asm_operand(tacky_val: TackyVal) -> AsmOperand {
    match tacky_val {
        TackyVal::Constant(num) => AsmOperand::Imm(num),
        TackyVal::Var(iden) => AsmOperand::Pseudo(iden),
    }
}

// Parse a tacky unary operator into a asm unary operator
fn generate_asm_unop(tacky_unop: TackyUnaryOp) -> AsmUnOp {
    match tacky_unop {
        TackyUnaryOp::Complement => AsmUnOp::Not,
        TackyUnaryOp::Negate => AsmUnOp::Neg,
    }
}

// Parse a Tacky Binary operator into a ASM Binary operator
fn generate_asm_binop(tacky_binop: TackyBinaryOp) -> AsmBinOp {
    match tacky_binop {
        TackyBinaryOp::Add => AsmBinOp::Add,
        TackyBinaryOp::Subtract => AsmBinOp::Sub,
        TackyBinaryOp::Multiply => AsmBinOp::Mul,
        _ => panic!("this should never reach here")
    }
}
