pub mod assembly;
pub mod codegen;
pub mod ir_gen;
pub mod lexer;
pub mod parser;
mod tacky_gen;
mod tacky_ir;

use crate::codegen::Codegen;
use crate::lexer::Lexer;
use crate::parser::{Parser, ParserError};
use crate::tacky_gen::{TackyError, TackyIrGenerator};

#[derive(Debug)]
pub enum CompilationStage {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
    Run,
}

pub fn compile(source_code: String, stage: &CompilationStage) -> Result<String, CompilerError> {
    let mut lexer = Lexer::new(source_code);
    let tokens = lexer.tokenize().map_err(CompilerError::Lexer)?;
    println!("tokens: {:#?}", tokens);
    if matches!(stage, CompilationStage::Lex) {
        return Ok("lex finished".to_string());
    }

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(CompilerError::Parser)?;
    println!("Parse Tree:\n{:#?}", &ast);

    if matches!(stage, CompilationStage::Parse) {
        return Ok("parse finished".to_string());
    }

    let mut tacky = TackyIrGenerator::new();
    let tacky_ast = tacky.generate_program(&ast).map_err(CompilerError::Tacky)?;

    println!("Tacky AST:\n{:#?}", &tacky_ast);
    println!("Tacky AST:\n{}", &tacky_ast);
    println!("Tacky IR:\n{}", &tacky_ast.pretty_print_with_comments());

    if matches!(stage, CompilationStage::Tacky) {
        return Ok("tacky finished".to_string());
    }

    let assembly_ast = ir_gen::generate_ir(&ast);
    println!("IR ast: \n {:#?}", &assembly_ast);

    if matches!(stage, CompilationStage::Tacky) {
        return Ok("tacky finished".to_string());
    }

    let assembly_code = Codegen::generate_asm_string(&assembly_ast);
    Ok(assembly_code)
}

#[derive(Debug)]
pub enum CompilerError {
    Lexer(String),
    Parser(ParserError),
    Tacky(TackyError),
}
