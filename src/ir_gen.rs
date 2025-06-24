use crate::{
    assembly::{AssemblyAst, FunctionAst, Instruction, Operand},
    parser::{Expression, Function, Program, Statement},
};

pub fn generate_ir(parse_tree: &Program) -> AssemblyAst {
    return lower_program(&parse_tree);
}

fn lower_program(program: &Program) -> AssemblyAst {
    let function_def = lower_function(&program.functions[0]);

    return AssemblyAst {
        function_def: function_def,
    };
}

fn lower_function(function: &Function) -> FunctionAst {
    println!("Matching function: {:#?}", &function);
    let mut instructions = Vec::new();

    for stmt in &function.body.stmt {
        let mut gen_instructions = lower_statement(&stmt);
        instructions.append(&mut gen_instructions);
    }

    return FunctionAst {
        name: function.name.clone(),
        instructions: instructions,
    };
}

fn lower_statement(statement: &Statement) -> Vec<Instruction> {
    let mut instructions = Vec::new();
    if let Statement::RETURN(exp, _) = statement {
        match exp {
            None => {
                instructions.push(Instruction::Ret);
            }
            Some(ex) => {
                let val = match ex {
                    Expression::LITERAL(val, _) => val,
                    Expression::IDENTIFIER(identifier, _) => {
                        todo!("identifier : {} expression not implemented yet", identifier)
                    }
                };

                let mv = Instruction::Mov(Operand::Imm(*val), Operand::Register);
                instructions.push(mv);
                instructions.push(Instruction::Ret);
            }
        }
    }

    return instructions;
}
