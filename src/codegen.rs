use crate::asm_ir_pass::assembly_ir::{
    AsmFunction, AsmInstruction, AsmOperand, AsmRegister, AsmUnaryOperator, AssemblyAst,AsmBinaryOperator,
};

/// The Codegen struct is responsible for generating the final assembly code from the Assembly IR.
pub struct Codegen;

impl Codegen {
    /// Generates the assembly code for the entire program.
    pub fn generate_asm_string(ast: &AssemblyAst) -> String {
        println!("[Codegen] Generating assembly string from Assembly IR.");
        let program_asm = Codegen::map_program(&ast);
        if std::env::var("CC_DEBUG").is_ok() {
            println!("[Codegen] Generated Assembly:\n{}", program_asm);
        }
        program_asm
    }

    /// Maps the entire program to an assembly string.
    fn map_program(ast: &AssemblyAst) -> String {
        Codegen::map_function(&ast.function_def)
    }

    /// Maps a single function to an assembly string.
    fn map_function(func: &AsmFunction) -> String {
        let mut out = String::new();
        // The `.globl` directive makes the function name visible to the linker.
        // Without this, the linker wouldn't be able to find the function.
        out.push_str("\t.globl ");
        out.push_str(&func.name.clone());
        out.push_str("\n");

        out.push_str(&func.name.clone());
        out.push_str(":\n");

        // Map each instruction to its assembly representation.
        for inst in &func.instructions {
            let ins = Codegen::map_instruction(inst);
            out.push_str("\t");
            out.push_str(ins.as_str());
            out.push_str("\n");
        }

        // The `.section .note.GNU-stack,"",@progbits` directive marks the stack as non-executable.
        // This is a security feature to prevent stack-based buffer overflow attacks.
        out.push_str("\n");
        out.push_str(".section .note.GNU-stack,\"\",@progbits");
        out.push_str("\n");

        return out;
    }

    /// Maps a single instruction to its assembly representation.
    fn map_instruction(inst: &AsmInstruction) -> String {
        match inst {
            AsmInstruction::Ret => "ret".to_string(),
            AsmInstruction::Mov(src, dest) => {
                        format!(
                            "movq {}, {}",
                            Codegen::map_operand(src),
                            Codegen::map_operand(dest)
                        )
                    }
            AsmInstruction::AsmUnary(operator, operand) => {
                        format!(
                            "{} {}",
                            Self::map_unary_operator(&operator),
                            Codegen::map_operand(&operand)
                        )
                    }
            AsmInstruction::AllocateStack(size) => {
                        format!("subq ${}, %rsp", size)
                    },
            AsmInstruction::Push(reg) => {
                        format!("pushq {}", Self::map_register(&reg.clone()))
                    }
            AsmInstruction::Pop(reg) => {
                        format!("popq {}", Self::map_register(&reg.clone()))
                    }
            AsmInstruction::Cdq => {
                format!("cqo")
            }
            AsmInstruction::Idiv(asm_operand) => {
                format!("idivq {}", Self::map_operand(asm_operand))
            }
            AsmInstruction::AsmBinary(asm_binary_operator, asm_operand1, asm_operand2) => {
                format!("{} {}, {}", Self::map_binary_operator(&asm_binary_operator), Self::map_operand(&asm_operand1), Self::map_operand(&asm_operand2))
            },
        }
    }

    /// Maps an operand to its assembly representation.
    fn map_operand(op: &AsmOperand) -> String {
        match op {
            AsmOperand::Reg(reg) => Codegen::map_register(reg).into(),
            AsmOperand::Imm(val) => format!("${}", val),
            // Pseudo-registers should have been replaced by now.
            &AsmOperand::Pseudo(_) => panic!("pseudo shouldn't be part of codegen"),
            &AsmOperand::Stack(pos) => format!("{}(%rbp)", pos),
        }
    }

    /// Maps a register to its assembly representation.
    fn map_register(reg: &AsmRegister) -> &'static str {
        match reg {
            AsmRegister::AX => "%rax",
            AsmRegister::DX => "%rdx",
            AsmRegister::ECX => "%rcx",
            AsmRegister::R10 => "%r10",
            AsmRegister::R11 => "%r11",
            AsmRegister::CL => "%cl",
            AsmRegister::RBP => "%rbp",
            AsmRegister::RSP => "%rsp",
        }
    }

    /// Maps a unary operator to its assembly representation.
    fn map_unary_operator(op: &AsmUnaryOperator) -> &'static str {
        match op {
            AsmUnaryOperator::Neg => "negq",
            AsmUnaryOperator::Not => "notq",
        }
    }

    /// Maps a binary operator to its assembly representation.
    fn map_binary_operator(op: &AsmBinaryOperator) -> &'static str {
        match op {
            AsmBinaryOperator::Add => "addq",
            AsmBinaryOperator::Sub => "subq",
            AsmBinaryOperator::Mult => "imulq",
            AsmBinaryOperator::Xor => "xorq",
            AsmBinaryOperator::And => "andq",
            AsmBinaryOperator::Or => "orq",
            AsmBinaryOperator::Shr => "shrq",
            AsmBinaryOperator::Shl => "shlq",
        }
    }
}
