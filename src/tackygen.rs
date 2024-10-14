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

//instruction = Return(val) | Unary(unary_operator, val src, val dst)
#[derive(Debug, Clone)]
pub enum TackyInstruction {
    Return(TackyVal),
    Unary(TackyUnaryOp, TackyVal, TackyVal),
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

// from a ast statement, generate a list of tacky instructions
fn generate_tacky_instructions(
    ast_expression: AstExpression,
    tacky_instructions: &mut Vec<TackyInstruction>,
) -> Result<TackyVal, CompileError> {
    match ast_expression {
        AstExpression::Constant(ast_const) => Ok(TackyVal::Constant(ast_const)),
        AstExpression::Unary(ast_unary_op, ast_expression) => {
            let src = generate_tacky_instructions(*ast_expression, tacky_instructions)?;
            let dst_name = make_temporary();
            let dst = TackyVal::Var(dst_name);
            let tacky_op = generate_tacky_unop(ast_unary_op)?;
            tacky_instructions.push(TackyInstruction::Unary(tacky_op, src, dst.clone()));
            Ok(dst)
        }
    }
}

// From a ast unary operator, construt a tacky unary operator
fn generate_tacky_unop(ast_unary_op: AstUnaryOp) -> Result<TackyUnaryOp, CompileError> {
    match ast_unary_op {
        AstUnaryOp::Complement => Ok(TackyUnaryOp::Complement),
        AstUnaryOp::Negate => Ok(TackyUnaryOp::Negate),
    }
}

// return a globally unique, temporary variable name for tmp register naming
fn make_temporary() -> String {
    // using atomic usize here lets us get around unsafe code as modifying static mut int is unsafe
    // but incrementing an atomic counter is not
    let val = ID_IDEN.fetch_add(1, Ordering::Relaxed);
    format!("tmp.{}", val)
}
