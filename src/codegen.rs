use crate::assembly::{AssemblyAst, FunctionAst, Instruction};

pub struct Codegen;

impl Codegen {
    pub fn generate_asm_string(ast: &AssemblyAst) -> String {
        Codegen::map_program(&ast)
    }

    fn map_program(ast: &AssemblyAst) -> String {
        let function_out = Codegen::map_function(&ast.function_def);
        return function_out;
    }

    fn map_function(func: &FunctionAst) -> String {
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

    fn map_instruction(inst: &Instruction) -> String {
        inst.to_string()
    }
}
