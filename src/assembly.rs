use crate::parser::{Expression, Function, Program, Statement};

#[derive(Debug)]
pub struct AssemblyAst {
    pub function_def: FunctionAst
}

#[derive(Debug)]
pub struct FunctionAst {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum Instruction {
    Ret,
    Mov(Operand, Operand)
}

impl Instruction {
    pub fn to_string(&self) -> String {
        match self {
            Instruction::Ret => "ret".to_string(),
            Instruction::Mov(src, dest) => {
                let mut ins = String::from("movl ");
                ins.push_str(src.to_string().as_str());
                ins.push_str(", ");
                ins.push_str(dest.to_string().as_str());
                return ins;
            }
        }
    }
}

#[derive(Debug)]
pub enum Operand {
    Imm(i64),
    Register
}

impl Operand {
    pub fn to_string(&self) -> String {
        match self {
            Operand::Imm(v) => {
                let mut o =  String::from("$");
                o.push_str(v.to_string().as_str());
                return o;
            }
            Operand::Register => {
                "%eax".to_string()
            }
        }
    }
}

impl AssemblyAst {
    pub fn get_ast(parse_tree: &Program) -> Self {
        return Self::match_program(&parse_tree);
    }

    fn match_program(program: &Program) -> AssemblyAst {
        let function_def = Self::match_function(&program.functions[0]);

        return Self {
            function_def: function_def
        }
    }

    fn match_function(function: &Function) -> FunctionAst {
        println!("Matching function: {:#?}", &function); 
        let mut instructions = Vec::new();

        for stmt in &function.body.stmt {
            let mut gen_instructions = Self::match_statement(&stmt);
            instructions.append(&mut gen_instructions);
        }

        return FunctionAst { name: function.name.clone(), instructions: instructions }
    }

    fn match_statement(statement: &Statement) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        if let Statement::RETURN(exp, _) = statement {
            match exp {
                None => {
                    instructions.push(Instruction::Ret);
                },
                Some(ex) => {
                    let val = match ex {
                        Expression::LITERAL(val, _) => val,
                        Expression::IDENTIFIER(identifier, _) => todo!("identifier : {} expression not implemented yet", identifier),
                    };

                    let mv = Instruction::Mov(Operand::Imm(*val), Operand::Register);
                    instructions.push(mv);
                    instructions.push(Instruction::Ret);
                }
            }
        }

        return instructions;
    }
}