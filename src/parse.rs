use crate::ast::{Expression, Precedence, Program, Statement};
use crate::tokenize::{Lexer, Token, TokenType};

use std::collections::HashMap;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        let mut parser = Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(
            TokenType::Identifier,
            Self::parse_identifier as PrefixParseFn,
        );

        parser
    }

    fn register_prefix(&mut self, token_type: TokenType, func: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, func);
    }

    fn register_infix(&mut self, token_type: TokenType, func: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, func);
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while !self.current_token_is((TokenType::EOF)) {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        program
    }

    fn string(&self, program: &Program) -> Vec<String> {
        let mut program_string = vec![];

        for statement in &program.statements {
            match statement {
                Statement::Let(ident, expr) => {
                    program_string.push(format!("let {} = {};\n", ident, expr.to_string()))
                }
                Statement::Return(expr) => program_string.push(format!("return {};\n", expr)),
                Statement::Expression(expr) => program_string.push(format!("{};\n", expr)),
            }
        }

        program_string
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&self, token: TokenType) -> bool {
        self.current_token.token_type == token
    }

    fn peek_token_is(&self, token: TokenType) -> bool {
        self.peek_token.token_type == token
    }

    fn expect_peek(&mut self, token: TokenType) -> bool {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            true
        } else {
            self.errors.push(format!(
                "expected next token to be {:?}, got {:?} instead",
                token.clone(),
                self.peek_token.token_type
            ));
            return false;
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let peek_token_type = self.peek_token.token_type.clone();

        if let TokenType::Identifier = peek_token_type {
            self.next_token();

            let ident = self.current_token.literal.clone();

            self.expect_peek(TokenType::Assign);

            if self.current_token_is(TokenType::Assign) {
                self.next_token();

                // todo
                while !self.peek_token_is(TokenType::Semicolon) {
                    self.next_token();
                }

                return Some(Statement::Let(ident.clone(), Expression::IntegerLiteral(0)));
            }
        }

        None
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(Expression::IntegerLiteral(0)))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        if let Some(expression) = self.parse_expression(Precedence::Lowest) {
            if self.peek_token_is(TokenType::Semicolon) {
                self.next_token();
            }

            Some(Statement::Expression(expression))
        } else {
            None
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type)?;
        let left_exp = prefix(self)?;

        Some(left_exp)
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        if let TokenType::Identifier = self.current_token.token_type {
            Some(Expression::Identifier(self.current_token.literal.clone()))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize::Lexer;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Statement::Let("x".to_string(), Expression::IntegerLiteral(10)),
                Statement::Return(Expression::IntegerLiteral(5)),
                Statement::Expression(Expression::Identifier("foobar".to_string())),
            ],
        };

        let lexer = Lexer::new("");
        let parser = Parser::new(lexer);

        let program_string = parser.string(&program);

        let expected = vec![
            "let x = 10;\n".to_string(),
            "return 5;\n".to_string(),
            "foobar;\n".to_string(),
        ];

        assert_eq!(parser.errors.len(), 0, "Unvalid statements found");

        assert_eq!(
            program.statements.len(),
            3,
            "Unexpected number of statements"
        );

        assert_eq!(program_string, expected);
    }

    #[test]
    fn test_let_statement_parsing() {
        let input = "
            let x = 10;
            let y = 15;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "Unvalid statements found. {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements.len(),
            2,
            "Unexpected number of statements"
        );

        let expected_identifiers = vec!["x", "y"];

        for (i, ident) in expected_identifiers.iter().enumerate() {
            match &program.statements[i] {
                Statement::Let(identifer, _) => {
                    assert_eq!(identifer, ident, "Unexpected identifier name");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }

    #[test]
    fn test_return_statement_parsing() {
        let input = "
            return 10;
            return 15;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "Unvalid statements found. {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements.len(),
            2,
            "Unexpected number of statements"
        );

        let expected_values = vec![Expression::IntegerLiteral(0), Expression::IntegerLiteral(0)];

        for (i, v) in expected_values.iter().enumerate() {
            match &program.statements[i] {
                Statement::Return(value) => {
                    assert_eq!(value, v, "Unexpected identifier name");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }

    #[test]
    fn test_expression_statement_parsing() {
        let input = "
            foo;
            bar
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(
            parser.errors.len(),
            0,
            "Unvalid statements found. {:?}",
            parser.errors
        );

        assert_eq!(
            program.statements.len(),
            2,
            "Unexpected number of statements"
        );

        let expected_values = vec![
            Expression::Identifier("foo".to_string()),
            Expression::Identifier("bar".to_string()),
        ];

        for (i, v) in expected_values.iter().enumerate() {
            match &program.statements[i] {
                Statement::Expression(value) => {
                    assert_eq!(value, v, "Unexpected identifier name");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }
}
