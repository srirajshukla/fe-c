use crate::asm_ir_pass::assembly_ir::{AsmFunction, AsmInstruction, AsmOperand, AsmRegister};
use std::collections::HashMap;

pub fn assign_stack_allocation(instruction: &Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    let mut var_map: HashMap<String, i64> = HashMap::new();
    let mut current_stack_offset: i64 = 0;

    for inst in instruction {
        let operands = match inst {
            AsmInstruction::Mov(src, dst) => vec![src, dst],
            AsmInstruction::AsmUnary(_, op) => vec![op],
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
            AsmInstruction::AsmUnary(operator, operand) => {
                let operand = replace_pseudo(operand, &var_map);
                new_instructions.push(AsmInstruction::AsmUnary(operator.clone(), operand));
            }
            AsmInstruction::AllocateStack(_) | AsmInstruction::Push(_) | AsmInstruction::Pop(_) => {
                new_instructions.push(inst.clone());
            }
        }
    }

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
            if std::mem::discriminant(src) == std::mem::discriminant(dest)
                && std::mem::discriminant(src) == std::mem::discriminant(&AsmOperand::Stack(0))
            {
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
