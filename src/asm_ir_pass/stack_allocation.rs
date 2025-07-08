use crate::asm_ir_pass::assembly_ir::{AsmBinaryOperator, AsmInstruction, AsmOperand, AsmRegister};
use std::collections::HashMap;

pub fn assign_stack_allocation(instruction: &Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    let mut var_map: HashMap<String, i64> = HashMap::new();
    let mut current_stack_offset: i64 = 0;

    for inst in instruction {
        let operands = match inst {
            AsmInstruction::Mov(src, dst) => vec![src, dst],
            AsmInstruction::AsmUnary(_, op) => vec![op],
            AsmInstruction::AsmBinary(_, op1, op2) => vec![op1, op2],
            AsmInstruction::Idiv(src) => vec![src],
            _ => vec![],
        };

        for op in &operands {
            if let AsmOperand::Pseudo(name) = op {
                if !var_map.contains_key(name) {
                    current_stack_offset -= 8;
                    var_map.insert(name.clone(), current_stack_offset);
                }
            }
        }
    }

    let total_stack_space = if current_stack_offset == 0 {
        0
    } else {
        (var_map.len() as i64 * 8 + 15) & -16
    };

    let mut new_instructions = Vec::with_capacity(instruction.len());

    new_instructions.push(AsmInstruction::Push(AsmRegister::RBP));
    new_instructions.push(AsmInstruction::Mov(
        AsmOperand::Reg(AsmRegister::RSP),
        AsmOperand::Reg(AsmRegister::RBP),
    ));

    if total_stack_space > 0 {
        new_instructions.push(AsmInstruction::AllocateStack(total_stack_space));
    }

    for inst in instruction {
        match inst {
            AsmInstruction::Ret => {
                new_instructions.push(AsmInstruction::Mov(
                    AsmOperand::Reg(AsmRegister::RBP),
                    AsmOperand::Reg(AsmRegister::RSP),
                ));
                new_instructions.push(AsmInstruction::Pop(AsmRegister::RBP));

                new_instructions.push(AsmInstruction::Ret);
            }
            AsmInstruction::Mov(src, dest) => {
                let src = replace_pseudo(src, &var_map);
                let dest = replace_pseudo(dest, &var_map);

                new_instructions.push(AsmInstruction::Mov(src, dest));
            }
            AsmInstruction::Idiv(src) => {
                let src = replace_pseudo(src, &var_map);
                new_instructions.push(AsmInstruction::Idiv(src));
            }
            AsmInstruction::AsmUnary(operator, operand) => {
                let operand = replace_pseudo(operand, &var_map);
                new_instructions.push(AsmInstruction::AsmUnary(operator.clone(), operand));
            }
            AsmInstruction::AsmBinary(op, op1, op2) => {
                let op1 = replace_pseudo(op1, &var_map);
                let op2 = replace_pseudo(op2, &var_map);

                new_instructions.push(AsmInstruction::AsmBinary(op.clone(), op1, op2));
            }
            AsmInstruction::AllocateStack(_)
            | AsmInstruction::Push(_)
            | AsmInstruction::Pop(_)
            | AsmInstruction::Cdq => {
                new_instructions.push(inst.clone());
            }
        }
    }

    println!(
        "New instructions after psuedo replacement:\n{:?}",
        &new_instructions
    );

    let new_instructions = fix_mov_stack_allocation(&new_instructions);
    println!(
        "New instructions after mov stack allocation replacement:\n{:?}",
        &new_instructions
    );
    let new_instructions = fix_memory_operand(&new_instructions);
    println!(
        "New instructions after memory op fix:\n{:?}",
        &new_instructions
    );

    return new_instructions;
}

fn replace_pseudo(op: &AsmOperand, pseudo_map: &HashMap<String, i64>) -> AsmOperand {
    if let AsmOperand::Pseudo(name) = op {
        if let Some(val) = pseudo_map.get(name) {
            return AsmOperand::Stack(*val);
        }
    }

    return op.clone();
}

pub fn fix_mov_stack_allocation(instruction: &Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    let mut new_instructions = Vec::new();
    for inst in instruction {
        if let AsmInstruction::Mov(src, dest) = inst {
            if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (src, dest) {
                new_instructions.push(AsmInstruction::Mov(
                    src.clone(),
                    AsmOperand::Reg(AsmRegister::R10),
                ));
                new_instructions.push(AsmInstruction::Mov(
                    AsmOperand::Reg(AsmRegister::R10),
                    dest.clone(),
                ));
            } else {
                new_instructions.push(inst.clone());
            }
        } else {
            new_instructions.push(inst.clone());
        }
    }

    return new_instructions;
}

fn fix_memory_operand(instructions: &Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    let mut new_instructions = Vec::new();

    for inst in instructions {
        match inst {
            AsmInstruction::AsmBinary(op, src, dst)
                if matches!(
                    op,
                    AsmBinaryOperator::Add
                        | AsmBinaryOperator::Sub
                        | AsmBinaryOperator::Xor
                        | AsmBinaryOperator::And
                        | AsmBinaryOperator::Or
                ) =>
            {
                if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (src, dst) {
                    new_instructions.push(AsmInstruction::Mov(
                        src.clone(),
                        AsmOperand::Reg(AsmRegister::R10),
                    ));
                    new_instructions.push(AsmInstruction::AsmBinary(
                        op.clone(),
                        AsmOperand::Reg(AsmRegister::R10),
                        dst.clone(),
                    ));
                } else {
                    new_instructions.push(inst.clone());
                }
            }
            AsmInstruction::AsmBinary(op, src, dst) 
                if *op == AsmBinaryOperator::Mult => 
            {
                if let AsmOperand::Stack(_) = dst {
                    let temp_reg = AsmOperand::Reg(AsmRegister::R11);
                    new_instructions.push(AsmInstruction::Mov(dst.clone(), temp_reg.clone()));
                    new_instructions.push(AsmInstruction::AsmBinary(
                        op.clone(),
                        src.clone(),
                        temp_reg.clone(),
                    ));
                    new_instructions.push(AsmInstruction::Mov(temp_reg, dst.clone()));
                } else {
                    new_instructions.push(inst.clone());
                }
            }
            AsmInstruction::AsmBinary(op, src, dst)
                if matches!(op, AsmBinaryOperator::Shr | AsmBinaryOperator::Shl) =>
            {
                if let AsmOperand::Stack(_) = src {
                    new_instructions.push(AsmInstruction::Mov(
                        src.clone(),
                        AsmOperand::Reg(AsmRegister::ECX),
                    ));
                    new_instructions.push(AsmInstruction::AsmBinary(
                        op.clone(),
                        AsmOperand::Reg(AsmRegister::CL),
                        dst.clone(),
                    ));
                } else {
                    new_instructions.push(inst.clone());
                }
            }
            AsmInstruction::Idiv(src) => {
                if let AsmOperand::Imm(_) = src {
                    let temp_reg = AsmOperand::Reg(AsmRegister::R10);
                    new_instructions.push(AsmInstruction::Mov(src.clone(), temp_reg.clone()));
                    new_instructions.push(AsmInstruction::Idiv(temp_reg));
                } else {
                    new_instructions.push(inst.clone());
                }
            }
            _ => {
                new_instructions.push(inst.clone());
            }
        }
    }
    return new_instructions;
}
