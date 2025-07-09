/// Represents the different types of tokens that the lexer can produce.
/// Each token corresponds to a syntactic element of the C language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Operators
    PLUS,
    ASTERISK,
    ForwardSlash,
    PERCENT,
    AND,
    OR,
    XOR,
    LeftShift,
    RightShift,
    BITWISE,
    NEGATION,
    DECREMENT,

    // Delimiters
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    SEMICOLON,

    // Literals and Identifiers
    NUMBER,
    IDENTIFIER,

    // Keywords
    KEYWORD(Keyword),

    // End of File
    EOF,
}

/// Represents the keywords of the C language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    INT,
    VOID,
    RETURN,
}

impl Keyword {
    /// Converts a keyword enum to its string representation.
    #[allow(unused)]
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::INT => "int",
            Keyword::VOID => "void",
            Keyword::RETURN => "return",
        }
    }

    /// Converts a string to a keyword enum if it's a valid keyword.
    pub fn to_keyword(s: &str) -> Option<Self> {
        match s {
            "int" => Some(Self::INT),
            "void" => Some(Self::VOID),
            "return" => Some(Self::RETURN),
            _ => None,
        }
    }

    /// Checks if a given string is a keyword.
    pub fn is_keyword(s: &str) -> bool {
        Keyword::to_keyword(s).is_some()
    }
}

/// Represents a range in the source code, used for error reporting.
pub type Span = std::ops::Range<usize>;

/// Represents a token, which is a single unit of the source code.
/// It contains the token type, its location in the source code (span),
/// and the actual text of the token (lexeme).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub ttype: TokenType,
    pub span: Span,
    pub lexeme: String,
}

impl Token {
    fn new(ttype: TokenType, start: usize, end: usize, value: String) -> Self {
        return Self {
            ttype: ttype,
            span: Span {
                start: start,
                end: end,
            },
            lexeme: value,
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexer {
    /// The input source code as a vector of characters.
    input: Vec<char>,
    /// The current position of the lexer in the input.
    pos: usize,
    /// The current character being processed.
    current_char: Option<char>,
}

impl Lexer {
    pub fn new(program: String) -> Self {
        let input: Vec<char> = program.chars().collect();
        let current_char = input.first().copied();
        println!("[Lexer] Initialized with program of length {}", input.len());
        Self {
            input: input,
            pos: 0,
            current_char: current_char,
        }
    }

    /// Advances the lexer to the next character in the input.
    fn advance(&mut self) {
        self.pos += 1;
        self.current_char = self.input.get(self.pos).copied();
    }

    /// Peeks at the next character in the input without consuming it.
    fn peek(&self) -> Option<char> {
        self.input.get(self.pos + 1).copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current_char {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Skips characters until a newline is encountered.
    /// This is used for single-line comments.
    fn skip_until_newline(&mut self) -> Result<(), String> {
        while let Some(ch) = self.current_char {
            self.advance();
            if ch == '\n' {
                break;
            }
        }
        Ok(())
    }

    /// Skips a comment, which can be single-line (//) or multi-line (/* ... */).
    fn skip_comment(&mut self) -> Result<(), String> {
        let cc = self.current_char;

        if cc.is_none() {
            return Err(format!("Unexpected EOF at position {}", self.pos));
        }

        let cc = cc.unwrap();

        if cc == '/' {
            // This is a single line comment
            println!("[Lexer] Skipping single-line comment");
            self.advance(); // skip /
            self.skip_until_newline()?;
        } else if cc == '*' {
            // This is a multi-line comment
            println!("[Lexer] Skipping multi-line comment");
            self.advance();

            while let Some(ch) = self.current_char {
                self.advance();
                if ch == '*' {
                    if self.current_char.is_none() {
                        return Err(format!("Unexpected EOF at position {}", self.pos));
                    }

                    let cc = self.current_char.unwrap();
                    if cc == '/' {
                        self.advance();
                        break;
                    }
                }
            }
        }
        return Ok(());
    }

    /// Parses a number literal.
    fn parse_number(&mut self) -> Result<Token, String> {
        println!("[Lexer] Parsing a number");
        let start_pos = self.pos;
        let mut numbers = String::new();

        while let Some(ch) = self.current_char {
            if ch.is_ascii_digit() {
                numbers.push(ch);
                self.advance();
            } else {
                // A number cannot be followed by an alphabetic character.
                if ch.is_alphabetic() {
                    return Err(format!(
                        "A number cannot end with alphabets. Ends with {} at {}",
                        ch, self.pos
                    ));
                }
                break;
            }
        }

        if numbers.is_empty() {
            return Err("Not a valid number".to_string());
        }
        let token = Token::new(TokenType::NUMBER, start_pos, self.pos, numbers.to_string());
        println!("[Lexer] Parsed number: {:?}", token);
        return Ok(token);
    }

    /// Parses a keyword or an identifier.
    fn parse_keyword_or_ident(&mut self) -> Result<Token, String> {
        println!("[Lexer] Parsing a keyword or identifier");
        let start_pos = self.pos;
        let mut word = String::new();

        if let Some(ch) = self.current_char {
            if ch.is_alphabetic() || ch == '_' {
                word.push(ch);
                self.advance();
            } else {
                return Err(format!(
                    "Identifier should start with alphabet or _, starts with {}",
                    ch
                ));
            }
        }

        while let Some(ch) = self.current_char {
            if ch.is_alphanumeric() || ch == '_' {
                word.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if Keyword::is_keyword(&word) {
            let kw = Keyword::to_keyword(&word).unwrap();
            let token = Token::new(TokenType::KEYWORD(kw), start_pos, self.pos, word);
            println!("[Lexer] Parsed keyword: {:?}", token);
            return Ok(token);
        } else {
            let token = Token::new(TokenType::IDENTIFIER, start_pos, self.pos, word);
            println!("[Lexer] Parsed identifier: {:?}", token);
            return Ok(token);
        }
    }

    /// Gets the next token from the input stream.
    fn get_next_token(&mut self) -> Result<Token, String> {
        loop {
            self.skip_whitespace();

            let i = self.pos;
            if let Some(current_char) = self.current_char {
                println!(
                    "[Lexer] Lexing at position: {}, character: '{}'",
                    i, current_char
                );
            }

            match self.current_char {
                None => return Ok(Token::new(TokenType::EOF, i, i, String::new())),
                Some('+') => {
                    self.advance();
                    return Ok(Token::new(TokenType::PLUS, i + 1, i, "+".to_string()));
                }
                Some('*') => {
                    self.advance();
                    return Ok(Token::new(TokenType::ASTERISK, i + 1, i, "*".to_string()));
                }
                Some('%') => {
                    self.advance();
                    return Ok(Token::new(TokenType::PERCENT, i + 1, i, "%".to_string()));
                }
                Some('~') => {
                    self.advance();
                    return Ok(Token::new(TokenType::BITWISE, i, i + 1, "~".to_string()));
                }
                Some('&') => {
                    self.advance();
                    return Ok(Token::new(TokenType::AND, i, i + 1, "&".to_string()));
                }
                Some('|') => {
                    self.advance();
                    return Ok(Token::new(TokenType::OR, i, i + 1, "|".to_string()));
                }
                Some('^') => {
                    self.advance();
                    return Ok(Token::new(TokenType::XOR, i, i + 1, "^".to_string()));
                }
                Some('-') => {
                    self.advance();
                    if let Some(x) = self.current_char {
                        if x == '-' {
                            self.advance();
                            return Ok(Token::new(
                                TokenType::DECREMENT,
                                i,
                                i + 2,
                                "--".to_string(),
                            ));
                        }
                    }
                    return Ok(Token::new(TokenType::NEGATION, i, i + 1, "-".to_string()));
                }
                Some('(') => {
                    self.advance();
                    return Ok(Token::new(TokenType::LPAREN, i, i + 1, "(".to_string()));
                }
                Some(')') => {
                    self.advance();
                    return Ok(Token::new(TokenType::RPAREN, i, i + 1, ")".to_string()));
                }
                Some('{') => {
                    self.advance();
                    return Ok(Token::new(TokenType::LBRACE, i, i + 1, "{".to_string()));
                }
                Some('}') => {
                    self.advance();
                    return Ok(Token::new(TokenType::RBRACE, i, i + 1, "}".to_string()));
                }
                Some(';') => {
                    self.advance();
                    return Ok(Token::new(TokenType::SEMICOLON, i, i + 1, ";".to_string()));
                }
                Some('#') => {
                    // Preprocessor directives are treated as comments for now.
                    println!("[Lexer] Skipping preprocessor directive.");
                    self.advance();
                    self.skip_until_newline()?;
                }
                Some('/') => {
                    let next = self.peek();
                    if next.is_none() {
                        return Err(format!("Unexpected EOF after / at position {}", i));
                    }
                    let next = next.unwrap();

                    self.advance();
                    if next == '/' || next == '*' {
                        self.skip_comment()?;
                    } else {
                        return Ok(Token::new(
                            TokenType::ForwardSlash,
                            i,
                            i + 1,
                            "/".to_string(),
                        ));
                    }
                }
                Some('>') => {
                    let next = self.peek();
                    if next.is_none() {
                        return Err(format!("Unexpected EOF after > at position {}", i));
                    }
                    let next = next.unwrap();

                    if next == '>' {
                        self.advance();
                        self.advance();
                        return Ok(Token::new(
                            TokenType::RightShift,
                            i,
                            i + 2,
                            ">>".to_string(),
                        ));
                    }
                }
                Some('<') => {
                    let next = self.peek();
                    if next.is_none() {
                        return Err(format!("Unexpected EOF after < at position {}", i));
                    }
                    let next = next.unwrap();

                    if next == '<' {
                        self.advance();
                        self.advance();
                        return Ok(Token::new(TokenType::LeftShift, i, i + 2, "<<".to_string()));
                    }
                }

                Some(ch) if ch.is_ascii_digit() => {
                    return self.parse_number();
                }
                Some(ch) if ch.is_alphabetic() || ch == '_' => {
                    return self.parse_keyword_or_ident();
                }
                ch => {
                    return Err(format!(
                        "Unimplemented character: {:#?} at position: {}",
                        &ch, i
                    ));
                }
            }
        }
    }

    /// Tokenizes the entire input program.
    /// This method repeatedly calls `get_next_token` until it reaches the end of the file.
    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        println!("[Lexer] Starting tokenization.");
        let mut tokens = Vec::new();

        loop {
            let token = self.get_next_token()?;
            let is_eof = token.ttype == TokenType::EOF;

            if is_eof {
                println!("[Lexer] Reached end of file.");
                break;
            }

            tokens.push(token);
        }
        println!(
            "[Lexer] Tokenization complete. Found {} tokens.",
            tokens.len()
        );
        if std::env::var("CC_DEBUG").is_ok() {
            dbg!(&tokens);
        }

        return Ok(tokens);
    }
}
