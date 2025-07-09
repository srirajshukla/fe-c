use crate::asm_ir_pass::assembly_ir::{AsmBinaryOperator, AsmInstruction, AsmOperand, AsmRegister};
use std::collections::HashMap;

/// This module is responsible for allocating space on the stack for variables.
/// It replaces pseudo-registers with stack-relative addresses and ensures proper stack alignment.

pub fn assign_stack_allocation(instruction: &Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    println!("[StackAlloc] Assigning stack allocation.");
    let mut var_map: HashMap<String, i64> = HashMap::new();
    let mut current_stack_offset: i64 = 0;

    // First pass: identify all pseudo-registers and assign them a stack offset.
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
                    current_stack_offset -= 8; // Each variable gets 8 bytes
                    var_map.insert(name.clone(), current_stack_offset);
                    println!("[StackAlloc] Mapping pseudo '{}' to stack offset {}", name, current_stack_offset);
                }
            }
        }
    }

    // The System V ABI requires the stack to be 16-byte aligned before a call instruction.
    // We align the total stack space to a 16-byte boundary to maintain this alignment.
    let total_stack_space = if current_stack_offset == 0 {
        0
    } else {
        (var_map.len() as i64 * 8 + 15) & -16
    };
    println!("[StackAlloc] Total stack space allocated: {} bytes (16-byte aligned)", total_stack_space);

    let mut new_instructions = Vec::with_capacity(instruction.len());

    // Function prologue: set up the stack frame.
    new_instructions.push(AsmInstruction::Push(AsmRegister::RBP));
    new_instructions.push(AsmInstruction::Mov(
        AsmOperand::Reg(AsmRegister::RSP),
        AsmOperand::Reg(AsmRegister::RBP),
    ));

    if total_stack_space > 0 {
        new_instructions.push(AsmInstruction::AllocateStack(total_stack_space));
    }

    // Second pass: replace all pseudo-registers with their assigned stack offsets.
    for inst in instruction {
        match inst {
            AsmInstruction::Ret => {
                // Function epilogue: restore the stack frame.
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

    if std::env::var("CC_DEBUG").is_ok() {
        println!(
            "[StackAlloc] Instructions after pseudo replacement:\n{}",
            new_instructions.iter().fold(String::from("\t"), |acc, n| acc + &n.to_string() + "\n\t" )
        );
    } else {
        println!("Didn't find CC_DEBUG");
    }

    let new_instructions = fix_mov_stack_allocation(&new_instructions);
    if std::env::var("CC_DEBUG").is_ok() {
        println!(
            "[StackAlloc] Instructions after mov stack allocation replacement:\n{}",
            new_instructions.iter().fold(String::from("\t"), |acc, n| acc + &n.to_string() + "\n\t" )
        );
    }
    let new_instructions = fix_memory_operand(&new_instructions);
    if std::env::var("CC_DEBUG").is_ok() {
        println!(
            "[StackAlloc] Instructions after memory op fix:\n{}",
            new_instructions.iter().fold(String::from("\t"), |acc, n| acc + &n.to_string() + "\n\t" )
        );
    }

    return new_instructions;
}

/// Replaces a pseudo-register operand with a stack-relative operand.
fn replace_pseudo(op: &AsmOperand, pseudo_map: &HashMap<String, i64>) -> AsmOperand {
    if let AsmOperand::Pseudo(name) = op {
        if let Some(val) = pseudo_map.get(name) {
            return AsmOperand::Stack(*val);
        }
    }

    return op.clone();
}

/// Fixes `mov` instructions that have two stack operands, which is not allowed in x86-64.
/// An instruction like mov `-8(%rbp), -16(%rbp)` (which is invalid), will be converted
/// A temporary register (R10) is used to break the move into two separate instructions:
/// `mov [src], %r10` and `mov %r10, [dest]`.
pub fn fix_mov_stack_allocation(instruction: &Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    let mut new_instructions = Vec::new();
    for inst in instruction {
        if let AsmInstruction::Mov(src, dest) = inst {
            if let (AsmOperand::Stack(_), AsmOperand::Stack(_)) = (src, dest) {
                println!("[StackAlloc] Fixing mov with two stack operands: {:?}", inst);
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

/// Fixes instructions that have memory operands where they are not allowed by x86-64
/// architecture rules. This function introduces temporary registers to hold the values
/// from memory before performing the operation, effectively breaking down illegal
/// memory-to-memory or memory-as-destination operations into valid sequences.
fn fix_memory_operand(instructions: &Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    let mut new_instructions = Vec::new();

    for inst in instructions {
        match inst {
            // Case 1: Binary operations (ADD, SUB, XOR, AND, OR) with two stack operands.
            // Why illegal: The x86-64 architecture generally does not allow direct
            // memory-to-memory operations. An instruction like `add [mem1], [mem2]`
            // is not supported. At least one operand must be a register.
            // Example of illegal instruction: `add qword ptr [rbp-8], qword ptr [rbp-16]`
            //
            // How it's fixed: Move one operand (the source) to a temporary register (R10),
            // then perform the operation using the temporary register and the memory destination.
            //
            // Original (illegal): `OP stack_src, stack_dst`
            // Becomes (legal sequence):
            //   `mov stack_src, R10`  (Move value from stack_src to R10)
            //   `OP R10, stack_dst`   (Perform operation with R10 and stack_dst)
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
                    println!("[StackAlloc] Fixing binary op with two stack operands: {:?}", inst);
                    new_instructions.push(AsmInstruction::Mov(
                        src.clone(),
                        AsmOperand::Reg(AsmRegister::R10), // Move source to R10
                    ));
                    new_instructions.push(AsmInstruction::AsmBinary(
                        op.clone(),
                        AsmOperand::Reg(AsmRegister::R10), // Use R10 as source
                        dst.clone(),
                    ));
                } else {
                    new_instructions.push(inst.clone());
                }
            }
            // Case 2: Multiplication (IMUL) with a stack destination.
            // Why illegal: The `imul` instruction (two-operand form, `imul reg, reg/mem`)
            // does not allow a memory operand as the destination. The destination must
            // always be a general-purpose register.
            // Example of illegal instruction: `imul rax, qword ptr [rbp-8]` (where [rbp-8] is the destination)
            //
            // How it's fixed: Move the value from the stack destination to a temporary register (R11),
            // perform the multiplication using R11 as the destination, then move the result
            // back from R11 to the original stack destination.
            //
            // Original (illegal): `imul src, stack_dst`
            // Becomes (legal sequence):
            //   `mov stack_dst, R11` (Load current value of stack_dst into R11)
            //   `imul src, R11`      (Perform multiplication, result in R11)
            //   `mov R11, stack_dst` (Store result from R11 back to stack_dst)
            AsmInstruction::AsmBinary(op, src, dst)
                if *op == AsmBinaryOperator::Mult =>
            {
                if let AsmOperand::Stack(_) = dst {
                    println!("[StackAlloc] Fixing imul with stack destination: {:?}", inst);
                    let temp_reg = AsmOperand::Reg(AsmRegister::R11);
                    // Step 1: Move the current value of the stack destination to a temporary register.
                    // This is crucial if the stack destination is also implicitly a source for the operation.
                    new_instructions.push(AsmInstruction::Mov(dst.clone(), temp_reg.clone()));
                    // Step 2: Perform the multiplication using the temporary register as destination.
                    new_instructions.push(AsmInstruction::AsmBinary(
                        op.clone(),
                        src.clone(),
                        temp_reg.clone(),
                    ));
                    // Step 3: Move the result from the temporary register back to the original stack destination.
                    new_instructions.push(AsmInstruction::Mov(temp_reg, dst.clone()));
                } else {
                    new_instructions.push(inst.clone());
                }
            }
            // Case 3: Shift operations (SHR, SHL) with a stack source for the shift count.
            // Why illegal: For shift instructions where the shift count is a register,
            // the x86-64 architecture mandates that the count *must* be in the `CL` register
            // (the lower 8 bits of `RCX`). It cannot be directly a memory operand.
            // Example of illegal instruction: `shl rax, qword ptr [rbp-8]` (where [rbp-8] is the shift count)
            //
            // How it's fixed: Move the stack source (which represents the shift amount)
            // into the `ECX` register. The `shl`/`shr` instructions will then implicitly
            // use the `CL` portion of `ECX` as the shift count.
            //
            // Original (illegal): `SHR/SHL value_to_shift, stack_src_shift_amount`
            // Becomes (legal sequence):
            //   `mov stack_src_shift_amount, ECX` (Load shift amount into ECX)
            //   `SHR/SHL value_to_shift, CL`      (Perform shift, CL is implicitly used)
            AsmInstruction::AsmBinary(op, src, dst)
                if matches!(op, AsmBinaryOperator::Shr | AsmBinaryOperator::Shl) =>
            {
                if let AsmOperand::Stack(_) = src {
                    println!("[StackAlloc] Fixing shift with stack source: {:?}", inst);
                    // Step 1: Move the stack source (shift amount) into the ECX register.
                    // The `shl`/`shr` instructions implicitly use `CL` (lower 8 bits of `ECX`)
                    // for the shift count when the count is a register.
                    new_instructions.push(AsmInstruction::Mov(
                        src.clone(),
                        AsmOperand::Reg(AsmRegister::ECX), // Move shift amount to ECX
                    ));
                    // Step 2: Perform the shift operation. The `CL` register is implicitly used.
                    new_instructions.push(AsmInstruction::AsmBinary(
                        op.clone(),
                        AsmOperand::Reg(AsmRegister::CL), // Use CL as the shift count
                        dst.clone(),
                    ));
                } else {
                    new_instructions.push(inst.clone());
                }
            }
            // Case 4: Integer division (IDIV) with an immediate source.
            // Why illegal: The `idiv` instruction (single-operand form) does not accept
            // an immediate value as its operand. It must be a register or a memory location.
            // Example of illegal instruction: `idiv 5`
            //
            // How it's fixed: Move the immediate value to a temporary register (R10),
            // then perform the division using that temporary register.
            //
            // Original (illegal): `idiv immediate_src`
            // Becomes (legal sequence):
            //   `mov immediate_src, R10` (Load immediate value into R10)
            //   `idiv R10`               (Perform division using R10)
            AsmInstruction::Idiv(src) => {
                if let AsmOperand::Imm(_) = src {
                    println!("[StackAlloc] Fixing idiv with immediate source: {:?}", inst);
                    let temp_reg = AsmOperand::Reg(AsmRegister::R10);
                    // Step 1: Move the immediate source value into a temporary register.
                    new_instructions.push(AsmInstruction::Mov(src.clone(), temp_reg.clone()));
                    // Step 2: Perform the division using the temporary register.
                    new_instructions.push(AsmInstruction::Idiv(temp_reg));
                } else {
                    new_instructions.push(inst.clone());
                }
            }
            // Default: For all other instructions, just clone and add to the new list.
            _ => {
                new_instructions.push(inst.clone());
            }
        }
    }
    return new_instructions;
}
