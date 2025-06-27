use crate::lexer::{Keyword, Span, Token, TokenType};

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    return_type: Type,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Vec<Statement>,
    span: Span,
}

#[derive(Debug)]
struct Parameter {
    param_type: Type,
    param_name: String,
    span: Span,
}

#[derive(Debug)]
pub enum Statement {
    RETURN(Option<Expression>, Span),
    EXPRESSION(Expression, Span),
}

#[derive(Debug)]
pub enum Expression {
    LITERAL(i64, Span),
    IDENTIFIER(String, Span),
    UNARY(UnaryOperator, Box<Expression>),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

#[derive(Debug)]
enum Type {
    INT,
    VOID,
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken { expected: String, found: Token },
    UnexpectedEof { expected: String },
    InvalidNumber(String),
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        return Self { tokens, pos: 0 };
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut functions = Vec::new();

        while !self.is_at_end() {
            let function = self.parse_function()?;
            functions.push(function);
        }

        let program = Program { functions };
        return Ok(program);
    }

    fn parse_function(&mut self) -> Result<Function, ParserError> {
        let start_pos = self.peek().span.start;
        let return_type = self.parse_type()?;
        let function_name = self.consume_identifier("Expected function name after type")?;

        self.consume_token(TokenType::LPAREN, "Expected '(' after function name")?;
        let parameters = self.parse_parameter_list()?;
        self.consume_token(TokenType::RPAREN, "Expected ')' at end of functi")?;

        let body = self.parse_block()?;

        let end_pos = body.span.end;

        return Ok(Function {
            return_type: return_type,
            name: function_name,
            parameters: parameters,
            body: body,
            span: Span {
                start: start_pos,
                end: end_pos,
            },
        });
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParserError> {
        let mut parameters = Vec::new();

        let ct = &self.peek().ttype;

        if *ct == TokenType::RPAREN {
            return Ok(parameters);
        }
        if *ct == TokenType::KEYWORD(Keyword::VOID) {
            self.advance();
            return Ok(parameters);
        }

        let param_start = self.pos;
        let param_type = self.parse_type()?;
        let param_name = self.consume_identifier("Expected variable name after type")?;
        let param_end = self.pos;

        parameters.push(Parameter {
            param_type: param_type,
            param_name: param_name,
            span: Span {
                start: param_start,
                end: param_end,
            },
        });

        Ok(parameters)
    }

    fn parse_block(&mut self) -> Result<Block, ParserError> {
        let start_pos = self.peek().span.start;
        self.consume_token(TokenType::LBRACE, "Expecetd '{' at start of block")?;

        let mut stmts = Vec::new();

        while !self.check(TokenType::RBRACE) && !self.is_at_end() {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        let end_token = self.consume_token(TokenType::RBRACE, "Expected '}' at end of block")?;
        let end_pos = end_token.span.end;

        return Ok(Block {
            stmt: stmts,
            span: start_pos..end_pos,
        });
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        if self.match_token(TokenType::KEYWORD(Keyword::RETURN)) {
            self.parse_return_statement()
        } else {
            todo!("this token is not yet implemented")
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let start_pos = self.previous().span.start;

        let expr = if self.check(TokenType::SEMICOLON) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        let end_token =
            self.consume_token(TokenType::SEMICOLON, "Expected ';' after return statement")?;
        let end_pos = end_token.span.end;

        return Ok(Statement::RETURN(expr, start_pos..end_pos));
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        match self.peek().ttype {
            TokenType::BITWISE | TokenType::NEGATION => self.parse_unary_expression(),
            TokenType::LPAREN => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume_token(TokenType::RPAREN, "Expected ')' after expression")?;
                return Ok(expr);
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParserError> {
        match self.peek().ttype {
            TokenType::BITWISE => {
                self.advance();
                let primary_value = self.parse_expression()?;
                let primary_value = Box::new(primary_value);
                return Ok(Expression::UNARY(UnaryOperator::Complement, primary_value));
            }
            TokenType::NEGATION => {
                self.advance();
                let primary_value = self.parse_expression()?;
                let primary_value = Box::new(primary_value);
                return Ok(Expression::UNARY(UnaryOperator::Negate, primary_value));
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    expected: "Expected a unary operator".to_string(),
                    found: self.peek().clone(),
                });
            }
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, ParserError> {
        match &self.peek().ttype {
            TokenType::NUMBER => {
                let token = self.advance().clone();
                let value = token
                    .lexeme
                    .parse::<i64>()
                    .map_err(|_| ParserError::InvalidNumber(token.lexeme.clone()))?;
                Ok(Expression::LITERAL(value, token.span))
            }
            TokenType::IDENTIFIER => {
                let token = self.advance().clone();
                Ok(Expression::IDENTIFIER(token.lexeme.clone(), token.span))
            }
            _ => Err(ParserError::UnexpectedToken {
                expected: "Expecting an expression".to_string(),
                found: self.peek().clone(),
            }),
        }
    }

    fn consume_token(&mut self, tt: TokenType, msg: &str) -> Result<&Token, ParserError> {
        if self.check(tt) {
            return Ok(self.advance());
        } else if self.is_at_end() {
            return Err(ParserError::UnexpectedEof {
                expected: (msg.to_string()),
            });
        } else {
            return Err(ParserError::UnexpectedToken {
                expected: (msg.to_string()),
                found: (self.peek().clone()),
            });
        }
    }

    fn consume_identifier(&mut self, msg: &str) -> Result<String, ParserError> {
        if let TokenType::IDENTIFIER = self.peek().ttype {
            Ok(self.advance().lexeme.clone())
        } else if self.is_at_end() {
            Err(ParserError::UnexpectedEof {
                expected: msg.to_string(),
            })
        } else {
            Err(ParserError::UnexpectedToken {
                expected: msg.to_string(),
                found: self.peek().clone(),
            })
        }
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        match self.peek().ttype {
            TokenType::KEYWORD(Keyword::INT) => {
                self.advance();
                Ok(Type::INT)
            }

            TokenType::KEYWORD(Keyword::VOID) => {
                self.advance();
                Ok(Type::VOID)
            }

            _ => Err(ParserError::UnexpectedToken {
                expected: "type".to_string(),
                found: self.peek().clone(),
            }),
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.pos - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokens.len() || self.peek().ttype == TokenType::EOF
    }

    fn check(&self, expected: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        } else {
            return self.peek().ttype == expected;
        }
    }

    fn match_token(&mut self, expected: TokenType) -> bool {
        if self.check(expected) {
            self.advance();
            return true;
        }
        return false;
    }
}
