use crate::assembly::AssemblyAst;
use crate::codegen::Codegen;
use crate::lexer::Lexer;
use crate::parser::{Parser, ParserError};

pub mod assembly;
pub mod codegen;
pub mod lexer;
pub mod parser;

pub fn compile(source_code: String) -> Result<String, CompilerError> {
    let mut lexer = Lexer::new(source_code);
    let tokens = lexer.tokenize().map_err(CompilerError::Lexer)?;

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(CompilerError::Parser)?;

    let assembly_ast = AssemblyAst::get_ast(&ast);

    let assembly_code = Codegen::generate_asm_string(&assembly_ast);

    Ok(assembly_code)
}

#[derive(Debug)]
pub enum CompilerError {
    Lexer(String),
    Parser(ParserError),
}
