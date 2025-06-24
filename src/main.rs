use std::fs;
use std::process::Command;

use crate::assembly::AssemblyAst;
use crate::codegen::Codegen;
use crate::lexer::Lexer;
use crate::parser::Parser;

mod assembly;
mod codegen;
mod lexer;
mod parser;

struct InputOptions {
    file_name: String,
    lex_only: bool,
}

impl InputOptions {
    fn from_opts(opts: &[String]) -> Self {
        let mut v = Self {
            file_name: String::new(),
            lex_only: false,
        };

        for opt in opts {
            if opt.contains("--lex") {
                v.lex_only = true;
            } else {
                v.file_name = opt.clone();
            }
        }

        return v;
    }

    fn read_contents(&self) -> String {
        match fs::read_to_string(&self.file_name) {
            Ok(content) => content,
            Err(err) => {
                println!("Error: {}", &err);
                std::process::exit(1);
            }
        }
    }
}

fn generate_exe(asm_file: &str) {
    let (output_file, _) = asm_file.rsplit_once('.').unwrap();
    let output = Command::new("gcc")
        .arg(asm_file)
        .arg("-o")
        .arg(output_file)
        .output()
        .expect("failed to generate exe");

    println!("output: {:#?}", &output);
}

 fn main() {
    let args: Vec<String> = std::env::args().collect();
    dbg!(&args);

    let opts = InputOptions::from_opts(&args[1..]);

    let program = opts.read_contents();
    let mut lexer = Lexer::new(program);

    let tokens = lexer.tokenize();
    println!("{:#?}", &tokens);

    let tokens = match tokens {
        Ok(t) => t,
        Err(msg) => {
            println!("{}", msg);
            std::process::exit(1);
        }
    };

    println!("==== Lexing Complete ====");

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    println!("{:#?}", &ast);

    let ast = match ast {
        Ok(t) => t,
        Err(msg) => {
            println!("{:#?}", msg);
            std::process::exit(1);
        }
    };

    println!("==== Parsing Complete ====");

    let assembly_ast = AssemblyAst::get_ast(&ast);
    println!("{:#?}", &assembly_ast);

    println!("==== Codegen complete ====");

    let codegen = Codegen {
        file_name: &opts.file_name,
    };

    match codegen.generate_asm(&assembly_ast) {
        Ok(output_file) => {
            generate_exe(&output_file);
        }
        Err(_) => {
            std::process::exit(-1);
        }
    }

    println!("==== Codewriting Complete ====");
}
