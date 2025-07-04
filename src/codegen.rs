use crate::asm_ir_pass::assembly_ir::{
    AsmFunction, AsmInstruction, AsmOperand, AsmRegister, AsmUnaryOperator, AssemblyAst,
};

pub struct Codegen;

impl Codegen {
    pub fn generate_asm_string(ast: &AssemblyAst) -> String {
        Codegen::map_program(&ast)
    }

    fn map_program(ast: &AssemblyAst) -> String {
        let function_out = Codegen::map_function(&ast.function_def);
        return function_out;
    }

    fn map_function(func: &AsmFunction) -> String {
        let mut out = String::new();
        out.push_str("\t");
        out.push_str(".globl ");
        out.push_str(&func.name.clone());
        out.push_str("\n");

        out.push_str(&func.name.clone());
        out.push_str(":\n");

        for inst in &func.instructions {
            let ins = Codegen::map_instruction(inst);
            out.push_str("\t");
            out.push_str(ins.as_str());
            out.push_str("\n");
        }

        out.push_str("\n");
        out.push_str(".section .note.GNU-stack,\"\",@progbits");
        out.push_str("\n");

        return out;
    }

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
        }
    }

    fn map_operand(op: &AsmOperand) -> String {
        match op {
            AsmOperand::Reg(reg) => Codegen::map_register(reg).into(),
            AsmOperand::Imm(val) => format!("${}", val),
            &AsmOperand::Pseudo(_) => panic!("pseudo shouldn't be part of codegen"),
            &AsmOperand::Stack(pos) => format!("{}(%rbp)", pos),
        }
    }

    fn map_register(reg: &AsmRegister) -> &'static str {
        match reg {
            AsmRegister::AX => "%rax",
            AsmRegister::ECX => "%rcx",
            AsmRegister::R10 => "%r10",
            AsmRegister::RBP => "%rbp",
            AsmRegister::RSP => "%rsp",
        }
    }

    fn map_unary_operator(op: &AsmUnaryOperator) -> &'static str {
        match op {
            AsmUnaryOperator::Neg => "negq",
            AsmUnaryOperator::Not => "notq",
        }
    }
}
