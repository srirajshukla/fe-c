use std::fs;
use std::process::Command;

use fe_c::{CompilationStage, compile};

#[derive(Debug)]
struct InputOptions {
    file_name: String,
    stage: CompilationStage,
}

impl InputOptions {
    fn from_opts(opts: &[String]) -> Self {
        let mut v = Self {
            file_name: String::new(),
            stage: CompilationStage::Run,
        };

        for opt in opts {
            match opt.as_str() {
                "--lex" => v.stage = CompilationStage::Lex,
                "--parse" => v.stage = CompilationStage::Parse,
                "--validate" => v.stage = CompilationStage::Validate,
                "--tacky" => v.stage = CompilationStage::Tacky,
                "--codegen" => v.stage = CompilationStage::Codegen,
                "--run" => v.stage = CompilationStage::Run,
                _ => v.file_name = opt.to_string(),
            }
        }

        v
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

fn write_to_file(input_file: &str, contents: &str) -> Result<String, ()> {
    let output_file_name = match input_file.rsplit_once('.') {
        Some((f, _f2)) => f.to_owned() + ".s",
        None => input_file.to_owned() + ".s",
    };

    match fs::write(&output_file_name, contents) {
        Ok(_) => {
            println!("Generated output file: {}", &output_file_name);
            return Ok(output_file_name);
        }
        Err(err) => {
            dbg!(format!(
                "Error generating output file name: {}\nError: {}",
                &output_file_name, &err
            ));
            Err(())
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    dbg!(&args);

    let opts = InputOptions::from_opts(&args[1..]);

    let program = opts.read_contents();

    match compile(program, &opts.stage) {
        Ok(result) => match opts.stage {
            CompilationStage::Lex
            | CompilationStage::Parse
            | CompilationStage::Validate
            | CompilationStage::Tacky
            | CompilationStage::Codegen => {
                println!("{:#?}", result);
            }
            CompilationStage::Run => {
                let asm_file_name = write_to_file(&opts.file_name, &result).unwrap();
                generate_exe(&asm_file_name);
            }
        },
        Err(err) => {
            eprintln!("Compilation Failed: {:#?}", &err);
            std::process::exit(1);
        }
    }

    println!("==== Codewriting Complete ====");
}
