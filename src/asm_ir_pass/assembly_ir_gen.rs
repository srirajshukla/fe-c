use crate::asm_ir_pass::assembly_ir::{AsmFunction, AsmInstruction, AsmOperand, AssemblyAst};
use crate::asm_ir_pass::assembly_ir::{AsmRegister, AsmUnaryOperator};
use crate::asm_ir_pass::stack_allocation::{assign_stack_allocation, fix_mov_stack_allocation};
use crate::tacky_pass::tacky_ir::{
    TackyFunction, TackyInstruction, TackyProgram, TackyUnary, TackyUnaryOperator, TackyVal,
};

/// This module takes in a tacky IR representation and
///  builds an Assembly IR tree from it.
///

pub fn generate_asm_ir(tacky: &TackyProgram) -> AssemblyAst {
    return lower_program(&tacky);
}

pub fn lower_program(program: &TackyProgram) -> AssemblyAst {
    let mut function_def = lower_function(&program.function_definition);
    let stack_allocated_inst = assign_stack_allocation(&function_def.instructions);
    let stack_fixed_inst = fix_mov_stack_allocation(&stack_allocated_inst);
    
    function_def.instructions = stack_fixed_inst;
    
    AssemblyAst { function_def }
}

fn lower_function(function: &TackyFunction) -> AsmFunction {
    println!("Matching function: {:#?}", &function);
    let mut instructions = Vec::new();

    for inst in &function.body {
        let mut gen_instructions = lower_instruction(&inst);
        instructions.append(&mut gen_instructions);
    }

    return AsmFunction {
        name: function.identifier.clone(),
        instructions: instructions,
    };
}

fn lower_instruction(instruction: &TackyInstruction) -> Vec<AsmInstruction> {
    match instruction {
        TackyInstruction::Return(ret) => match ret {
            None => vec![AsmInstruction::Ret],
            Some(v) => {
                let mut gen_instructions = Vec::new();
                let operand = lower_tacky_val(v);
                gen_instructions.push(AsmInstruction::Mov(
                    operand,
                    AsmOperand::Reg(AsmRegister::AX),
                ));
                gen_instructions.push(AsmInstruction::Ret);
                gen_instructions
            }
        },
        TackyInstruction::Unary(TackyUnary {
            operator,
            src,
            dest,
        }) => {
            let mut gen_instructions = Vec::new();
            let src_out = lower_tacky_val(&src);
            let dst_out = lower_tacky_val(&dest);
            gen_instructions.push(AsmInstruction::Mov(src_out, dst_out.clone()));
            let unary_operator = lower_unary_op(&operator);
            gen_instructions.push(AsmInstruction::AsmUnary(unary_operator, dst_out));
            return gen_instructions;
        }
    }
}

fn lower_tacky_val(v: &TackyVal) -> AsmOperand {
    match v {
        TackyVal::Constant(v) => AsmOperand::Imm(*v),
        TackyVal::Variable(v) => AsmOperand::Pseudo(v.clone()),
    }
}

fn lower_unary_op(op: &TackyUnaryOperator) -> AsmUnaryOperator {
    match op {
        TackyUnaryOperator::Complement => AsmUnaryOperator::Not,
        TackyUnaryOperator::Negate => AsmUnaryOperator::Neg,
    }
}
