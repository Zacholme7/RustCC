
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::parser::*;
use crate::errors::CompileError;

static ID_IDEN: AtomicUsize = AtomicUsize::new(0);


//program = Program(function_definition)
#[derive(Debug, Clone)]
pub enum TackyProgram {
    Program(TackyFunctionDefinition)
}

//function_definition = Function(identifier, 1 instruction* body) 
#[derive(Debug, Clone)]
pub enum TackyFunctionDefinition {
    Function(String, Vec<TackyInstruction>)
}

//instruction = Return(val) | Unary(unary_operator, val src, val dst) 
#[derive(Debug, Clone)]
pub enum TackyInstruction {
    Return(TackyVal),
    Unary(TackyUnaryOp, TackyVal, TackyVal)
}

//val = Constant(int) | Var(identifier)
#[derive(Debug, Clone)]
pub enum TackyVal {
    Constant(usize),
    Var(String)
}

//unary_operator = Complement | Negate
#[derive(Debug, Clone)]
pub enum TackyUnaryOp {
    Complement,
    Negate
}

// Parse the abstract syntax tree and generate a tacky abstract syntax tree
pub fn generate_tacky_ast(ast: ProgramAst) -> Result<TackyProgram, CompileError> {
    let ProgramAst::Program(ast_func_def) = ast;
    let tacky_function = generate_tacky_function(ast_func_def)?;
    Ok(TackyProgram::Program(tacky_function))
}

// from a ast function, generate a tacky function
pub fn generate_tacky_function(ast_function: FunctionDefinitionAst) -> Result<TackyFunctionDefinition, CompileError> {
    let FunctionDefinitionAst::Function(iden, statement) = ast_function;
    let Statement::Return(ast_expression) = statement;
    let mut tacky_instructions: Vec<TackyInstruction> = Vec::new();
    generate_tacky_instructions(ast_expression, &mut tacky_instructions)?;
    Ok(TackyFunctionDefinition::Function(iden, tacky_instructions))
}

// from a ast statement, generate a list of tacky instructions
pub fn generate_tacky_instructions(
    ast_expression: Expression, 
    mut tacky_instructions: &mut Vec<TackyInstruction>
) -> Result<TackyVal, CompileError> {
    match ast_expression {
        Expression::Constant(ast_const) => {
            Ok(TackyVal::Constant(ast_const))
        }
        Expression::Unary(ast_unary_op, ast_expression) => {
            let src = generate_tacky_instructions(*ast_expression, &mut tacky_instructions)?;
            let dst_name = make_temporary();
            let dst = TackyVal::Var(dst_name);
            let tacky_op = generate_tacky_unop(ast_unary_op)?;
            tacky_instructions.push(TackyInstruction::Unary(tacky_op, src, dst.clone()));
            Ok(dst)
        }
    }
}

pub fn generate_tacky_unop(ast_unary_op: UnaryOperator) -> Result<TackyUnaryOp, CompileError> {
    match ast_unary_op {
        UnaryOperator::Complement => Ok(TackyUnaryOp::Complement),
        UnaryOperator::Negate => Ok(TackyUnaryOp::Negate),
        _ => todo!()
    }
}

pub fn make_temporary() -> String {
    let val = ID_IDEN.fetch_add(1, Ordering::Relaxed);
    format!("tmp.{}", val)
}