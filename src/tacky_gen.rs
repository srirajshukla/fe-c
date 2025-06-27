use crate::{parser, tacky_ir};

pub struct TackyIrGenerator {
    var_cnt: usize,
}

#[derive(Debug)]
pub enum TackyError {
    Error(String),
}

impl TackyIrGenerator {
    pub fn new() -> Self {
        Self { var_cnt: 0 }
    }

    fn make_temporary(&mut self) -> String {
        let var = format!("tmp.{}", self.var_cnt);
        self.var_cnt += 1;
        return var;
    }

    pub fn generate_program(
        &mut self,
        program: &parser::Program,
    ) -> Result<tacky_ir::TackyProgram, TackyError> {
        let function = program
            .functions
            .first()
            .ok_or(TackyError::Error("expected a function".to_string()))?;

        let function_ir = self.generate_function(&function)?;

        Ok(tacky_ir::TackyProgram {
            function_definition: function_ir,
        })
    }

    fn generate_function(
        &mut self,
        function: &parser::Function,
    ) -> Result<tacky_ir::TackyFunction, TackyError> {
        let mut instructions = Vec::new();

        for stmt in &function.body.stmt {
            let mut gen_instructions = self.generate_statement(&stmt)?;
            instructions.append(&mut gen_instructions);
        }

        Ok(tacky_ir::TackyFunction {
            identifier: function.name.clone(),
            body: instructions,
        })
    }

    fn generate_statement(
        &mut self,
        stmt: &parser::Statement,
    ) -> Result<Vec<tacky_ir::TackyInstruction>, TackyError> {
        match stmt {
            parser::Statement::RETURN(expr, _sp) => {
                let mut instructions = Vec::new();

                match expr {
                    None => {
                        let tacky = tacky_ir::TackyInstruction::Return(None);
                        instructions.push(tacky);
                    }
                    Some(expr) => {
                        let (mut expr_tacky, return_val) = self.emit_expression(&expr)?;
                        instructions.append(&mut expr_tacky);
                        instructions.push(tacky_ir::TackyInstruction::Return(Some(return_val)));
                    }
                }

                Ok(instructions)
            }
            _ => Err(TackyError::Error(format!(
                "unexpected statement {:?}",
                stmt
            ))),
        }
    }

    fn emit_expression(
        &mut self,
        expr: &parser::Expression,
    ) -> Result<(Vec<tacky_ir::TackyInstruction>, tacky_ir::TackyVal), TackyError> {
        match expr {
            parser::Expression::LITERAL(c, _) => Ok((Vec::new(), tacky_ir::TackyVal::Constant(*c))),
            parser::Expression::IDENTIFIER(s, _) => Err(TackyError::Error(format!(
                "tacky for identifier {} not yet implemented",
                &s
            ))),
            parser::Expression::UNARY(op, expr) => {
                let (mut instructions, operand) = self.emit_expression(expr)?;
                let result_var = self.make_temporary();
                let result_var = tacky_ir::TackyVal::Variable(result_var);

                let unary = tacky_ir::TackyInstruction::Unary(tacky_ir::TackyUnary {
                    operator: Self::convert_unary_operator(op),
                    src: operand,
                    dest: result_var.clone(),
                });

                instructions.push(unary);
                Ok((instructions, result_var))
            }
        }
    }

    fn convert_unary_operator(op: &parser::UnaryOperator) -> tacky_ir::TackyUnaryOperator {
        match op {
            parser::UnaryOperator::Complement => tacky_ir::TackyUnaryOperator::Complement,
            parser::UnaryOperator::Negate => tacky_ir::TackyUnaryOperator::Negate,
        }
    }
}
