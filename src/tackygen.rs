use crate::errors::CompileError;
use crate::parser::*;
use std::sync::atomic::{AtomicUsize, Ordering};

// identifier to represent temp registers in tacky repr
static ID_IDEN: AtomicUsize = AtomicUsize::new(0);

//program = Program(function_definition)
#[derive(Debug, Clone)]
pub enum TackyProgram {
    Program(TackyFunctionDefinition),
}

//function_definition = Function(identifier, 1 instruction* body)
#[derive(Debug, Clone)]
pub enum TackyFunctionDefinition {
    Function(String, Vec<TackyInstruction>),
}

//instruction = Return(val) 
//              | Unary(unary_operator, val src, val dst)
//              | Binary(binary_operator, val src1, val src2, val dst)
#[derive(Debug, Clone)]
pub enum TackyInstruction {
    Return(TackyVal),
    Unary(TackyUnaryOp, TackyVal, TackyVal),
    Binary(TackyBinaryOp, TackyVal, TackyVal, TackyVal),
}

//val = Constant(int) | Var(identifier)
#[derive(Debug, Clone)]
pub enum TackyVal {
    Constant(usize),
    Var(String),
}

//unary_operator = Complement | Negate
#[derive(Debug, Clone)]
pub enum TackyUnaryOp {
    Complement,
    Negate,
}

//binary_operator = Add | Subtract | Multiply | Divide | Remainder
#[derive(Debug, Clone)]
pub enum TackyBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder
}


// Parse the abstract syntax tree and generate a tacky abstract syntax tree
pub fn generate_tacky_ast(ast: AstProgram) -> Result<TackyProgram, CompileError> {
    let AstProgram::Program(ast_func_def) = ast;
    let tacky_function = generate_tacky_function(ast_func_def)?;
    Ok(TackyProgram::Program(tacky_function))
}

// from a ast function, generate a tacky function
fn generate_tacky_function(
    ast_function: AstFunctionDefinition,
) -> Result<TackyFunctionDefinition, CompileError> {
    // extract out the expression
    let AstFunctionDefinition::Function(iden, statement) = ast_function;
    let AstStatement::Return(ast_expression) = statement;

    // generate all of the instructions
    let mut tacky_instructions: Vec<TackyInstruction> = Vec::new();
    let dst = generate_tacky_instructions(ast_expression, &mut tacky_instructions)?;
    tacky_instructions.push(TackyInstruction::Return(dst));

    // return a new tacky function definition
    Ok(TackyFunctionDefinition::Function(iden, tacky_instructions))
}

// From a AST statement, generate a list of Tacky instructions
fn generate_tacky_instructions(
    ast_expression: AstExpression,
    tacky_instructions: &mut Vec<TackyInstruction>,
) -> Result<TackyVal, CompileError> {
    match ast_expression {
        AstExpression::Constant(ast_const) => Ok(TackyVal::Constant(ast_const)),
        AstExpression::Unary(ast_unary_op, ast_expression) => {
            // Generate the source operand and the unary operator
            let src = generate_tacky_instructions(*ast_expression, tacky_instructions)?;
            let tacky_op = generate_tacky_unop(ast_unary_op);

            // Generate a temporary destination
            let dst_name = make_temporary();
            let dst = TackyVal::Var(dst_name);

            // Record new unary instruction
            tacky_instructions.push(TackyInstruction::Unary(tacky_op, src, dst.clone()));

            Ok(dst)
        }
        AstExpression::Binary(ast_binary_op, ast_expression1, ast_expression2) => {
            // Generate both source operands the the binary operator
            let src1 = generate_tacky_instructions(*ast_expression1, tacky_instructions)?;
            let src2 = generate_tacky_instructions(*ast_expression2, tacky_instructions)?;
            let tacky_op = generate_tacky_binop(ast_binary_op);

            // Generate a temporary destination
            let dst_name = make_temporary();
            let dst = TackyVal::Var(dst_name);

            // Record the new binary instruction
            tacky_instructions.push(TackyInstruction::Binary(tacky_op, src1, src2, dst.clone()));
            Ok(dst)
        }
    }
}

// From a AST Binary operator, construct a Tacky Binary Operator
fn generate_tacky_binop(ast_binary_op: AstBinaryOp) -> TackyBinaryOp {
    match ast_binary_op {
        AstBinaryOp::Add => TackyBinaryOp::Add,
        AstBinaryOp::Subtract => TackyBinaryOp::Subtract,
        AstBinaryOp::Divide => TackyBinaryOp::Divide,
        AstBinaryOp::Multiply => TackyBinaryOp::Multiply,
        AstBinaryOp::Remainder => TackyBinaryOp::Remainder,
        _ => todo!()
    }
}

// From a AST Unary operator, construct a Tacky Unary operator
fn generate_tacky_unop(ast_unary_op: AstUnaryOp) -> TackyUnaryOp {
    match ast_unary_op {
        AstUnaryOp::Complement => TackyUnaryOp::Complement,
        AstUnaryOp::Negate => TackyUnaryOp::Negate,
        _ => todo!()
    }
}

// return a globally unique, temporary variable name for tmp register naming
fn make_temporary() -> String {
    // using atomic usize here lets us get around unsafe code as modifying static mut int is unsafe
    // but incrementing an atomic counter is not
    let val = ID_IDEN.fetch_add(1, Ordering::Relaxed);
    format!("tmp.{}", val)
}
