use std::fs;
use std::process::Command;

use fe_c::compile;

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
    match compile(program) {
        Ok(assembly_code) => {
            let asm_file_name = write_to_file(&opts.file_name, &assembly_code).unwrap();
            generate_exe(&asm_file_name);
        }
        Err(err) => {
            eprintln!("Compilation Failed: {:#?}", &err);
            std::process::exit(1);
        }
    }

    println!("==== Codewriting Complete ====");
}
