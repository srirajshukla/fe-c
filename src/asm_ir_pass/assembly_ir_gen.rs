use crate::asm_ir_pass::assembly_ir::{AsmBinaryOperator, AsmRegister, AsmUnaryOperator};
use crate::asm_ir_pass::assembly_ir::{AsmFunction, AsmInstruction, AsmOperand, AssemblyAst};
use crate::asm_ir_pass::stack_allocation::assign_stack_allocation;
use crate::tacky_pass::tacky_ir::{
    TackyBinary, TackyBinaryOperator, TackyFunction, TackyInstruction, TackyProgram, TackyUnary,
    TackyUnaryOperator, TackyVal,
};

/// This module takes a Tacky IR representation and builds an Assembly IR tree from it.
/// This is the final stage before code generation.

pub fn generate_asm_ir(tacky: &TackyProgram) -> AssemblyAst {
    println!("[AsmIR] Generating Assembly IR from Tacky IR.");
    return lower_program(&tacky);
}

pub fn lower_program(program: &TackyProgram) -> AssemblyAst {
    let mut function_def = lower_function(&program.function_definition);
    if std::env::var("CC_DEBUG").is_ok() {
        println!("[AsmIR] First pass before stack allocation: \n {}", &function_def);
    }
    let stack_allocated_inst = assign_stack_allocation(&function_def.instructions);

    function_def.instructions = stack_allocated_inst;

    if std::env::var("CC_DEBUG").is_ok() {
        println!("[AsmIR] Second pass after stack allocation: \n{}", &function_def);
    }

    AssemblyAst { function_def }
}

fn lower_function(function: &TackyFunction) -> AsmFunction {
    println!("[AsmIR] Lowering function: {}", &function.identifier);
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

/// Lowers a single Tacky instruction to a sequence of Assembly IR instructions.
fn lower_instruction(instruction: &TackyInstruction) -> Vec<AsmInstruction> {
    if std::env::var("CC_DEBUG").is_ok() {
        println!("[AsmIR] Lowering instruction: {:?}", instruction);
    }
    match instruction {
        TackyInstruction::Return(ret) => match ret {
            None => vec![AsmInstruction::Ret],
            Some(v) => {
                let mut gen_instructions = Vec::new();
                let operand = lower_tacky_val(v);
                // The return value is placed in the RAX register according to the System V ABI.
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
        TackyInstruction::Binary(TackyBinary {
            operator,
            src1,
            src2,
            dest,
        }) => {
            let mut gen_instructions = Vec::new();

            match operator {
                // Case 1: Division (TackyBinaryOperator::Divide)
                // Why special: In x86-64 assembly, the `idiv` instruction is used for signed division.
                // It implicitly uses the `RDX:RAX` register pair as a 128-bit dividend.
                // - `RAX` holds the lower 64 bits of the dividend.
                // - `RDX` holds the upper 64 bits of the dividend.
                // After division, the quotient is stored in `RAX` and the remainder in `RDX`.
                //
                // Steps:
                // 1. Move the first source operand (`src1`, the dividend) into `RAX`.
                // 2. Execute `cqo` (Convert Quadword to Octoword) to sign-extend `RAX` into `RDX`.
                //    This prepares `RDX:RAX` for a 128-bit signed division.
                // 3. Execute `idiv src2_out` where `src2_out` is the divisor. The divisor can be
                //    a register or memory operand. The result (quotient in `RAX`, remainder in `RDX`)
                //    is implicit.
                // 4. Move the quotient (from `RAX`) to the destination (`dest`).
                //
                // Example (Tacky: `result = a / b`):
                //   `mov a, %rax`
                //   `cqo`
                //   `idiv b`
                //   `mov %rax, result`
                TackyBinaryOperator::Divide => {
                    let src1_out = lower_tacky_val(&src1);
                    // Step 1: Move the dividend (src1) into RAX.
                    gen_instructions.push(AsmInstruction::Mov(
                        src1_out,
                        AsmOperand::Reg(AsmRegister::AX),
                    ));
                    // Step 2: Sign-extend RAX into RDX using `cqo`.
                    gen_instructions.push(AsmInstruction::Cdq);

                    let src2_out = lower_tacky_val(&src2);
                    // Step 3: Perform the signed division. Divisor is src2_out.
                    // Quotient will be in RAX, remainder in RDX.
                    gen_instructions.push(AsmInstruction::Idiv(src2_out));

                    let dst_out = lower_tacky_val(&dest);
                    // Step 4: Move the quotient (from RAX) to the final destination.
                    gen_instructions.push(AsmInstruction::Mov(
                        AsmOperand::Reg(AsmRegister::AX),
                        dst_out,
                    ));
                }
                // Case 2: Remainder (TackyBinaryOperator::Remainder)
                // Why special: Similar to division, the `idiv` instruction is used. After `idiv`,
                // the remainder is implicitly stored in the `RDX` register.
                //
                // Steps:
                // 1. Move the first source operand (`src1`, the dividend) into `RAX`.
                // 2. Execute `cqo` to sign-extend `RAX` into `RDX`.
                // 3. Execute `idiv src2_out` (divisor).
                // 4. Move the remainder (from `RDX`) to the destination (`dest`).
                //
                // Example (Tacky: `result = a % b`):
                //   `mov a, %rax`
                //   `cqo`
                //   `idiv b`
                //   `mov %rdx, result`
                TackyBinaryOperator::Remainder => {
                    let src1_out = lower_tacky_val(&src1);
                    // Step 1: Move the dividend (src1) into RAX.
                    gen_instructions.push(AsmInstruction::Mov(
                        src1_out,
                        AsmOperand::Reg(AsmRegister::AX),
                    ));
                    // Step 2: Sign-extend RAX into RDX using `cqo`.
                    gen_instructions.push(AsmInstruction::Cdq);

                    let src2_out = lower_tacky_val(&src2);
                    // Step 3: Perform the signed division. Divisor is src2_out.
                    // Quotient will be in RAX, remainder in RDX.
                    gen_instructions.push(AsmInstruction::Idiv(src2_out));

                    let dst_out = lower_tacky_val(&dest);
                    // Step 4: Move the remainder (from RDX) to the final destination.
                    gen_instructions.push(AsmInstruction::Mov(
                        AsmOperand::Reg(AsmRegister::DX),
                        dst_out,
                    ));
                }
                // Default case for other binary operators (Add, Subtract, Multiply, And, Or, Xor, Shift).
                // These operators generally follow a pattern of moving src1 to dest, then applying
                // the binary operation with src2 and dest.
                //
                // Steps:
                // 1. Move the first source operand (`src1`) to the destination (`dest`).
                // 2. Apply the binary operation using the second source operand (`src2`) and the destination (`dest`).
                //    The result is stored back in `dest`.
                //
                // Example (Tacky: `result = a + b`):
                //   `mov a, result`
                //   `add b, result`
                _ => {
                    let src1_out = lower_tacky_val(&src1);
                    let dst_out = lower_tacky_val(&dest);
                    gen_instructions.push(AsmInstruction::Mov(src1_out, dst_out.clone()));

                    let bin_operator = lower_binary_op(&operator);
                    let src2_out = lower_tacky_val(&src2);
                    gen_instructions.push(AsmInstruction::AsmBinary(
                        bin_operator,
                        src2_out,
                        dst_out,
                    ));
                }
            }

            return gen_instructions;
        }
    }
}

/// Converts a Tacky value to an Assembly operand.
fn lower_tacky_val(v: &TackyVal) -> AsmOperand {
    match v {
        TackyVal::Constant(v) => AsmOperand::Imm(*v),
        TackyVal::Variable(v) => AsmOperand::Pseudo(v.clone()),
    }
}

/// Converts a Tacky unary operator to an Assembly unary operator.
fn lower_unary_op(op: &TackyUnaryOperator) -> AsmUnaryOperator {
    match op {
        TackyUnaryOperator::Complement => AsmUnaryOperator::Not,
        TackyUnaryOperator::Negate => AsmUnaryOperator::Neg,
    }
}

/// Converts a Tacky binary operator to an Assembly binary operator.
fn lower_binary_op(op: &TackyBinaryOperator) -> AsmBinaryOperator {
    match op {
        TackyBinaryOperator::Add => AsmBinaryOperator::Add,
        TackyBinaryOperator::Subtract => AsmBinaryOperator::Sub,
        TackyBinaryOperator::Multiply => AsmBinaryOperator::Mult,
        TackyBinaryOperator::And => AsmBinaryOperator::And,
        TackyBinaryOperator::Or => AsmBinaryOperator::Or,
        TackyBinaryOperator::Xor => AsmBinaryOperator::Xor,
        TackyBinaryOperator::LeftShift => AsmBinaryOperator::Shl,
        TackyBinaryOperator::RightShift => AsmBinaryOperator::Shr,
        _ => unreachable!(),
    }
}
