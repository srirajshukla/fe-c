use std::fs;

use crate::assembly::{AssemblyAst, FunctionAst, Instruction};

pub struct Codegen<'a> {
    pub file_name: &'a str,
}

impl Codegen<'_> {
    pub fn generate_asm(&self, ast: &AssemblyAst) -> Result<String, ()> {
        let program_out = Codegen::map_program(&ast);
        self.write_to_file(&program_out)
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

    fn write_to_file(&self, contents: &str) -> Result<String, ()> {
        let output_file_name = match self.file_name.rsplit_once('.') {
            Some((f, _f2)) => {
                f.to_owned() + ".s"
            }
            None => self.file_name.to_owned() + ".s"
        };

        match fs::write(&output_file_name, contents) {
            Ok(_) => {
                println!("Generated output file: {}", &output_file_name);
                return Ok(output_file_name);
            },
            Err(err) => {
                dbg!(format!("Error generating output file name: {}\nError: {}", &output_file_name, &err));
                Err(())
            }
        }
    }
}