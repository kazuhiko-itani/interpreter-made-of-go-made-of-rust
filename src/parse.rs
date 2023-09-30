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

        Self::setup_prefix_parse_fns(&mut parser);
        Self::setup_infix_parse_fns(&mut parser);

        parser
    }

    fn setup_prefix_parse_fns(&mut self) {
        self.register_prefix(TokenType::IDENT, Self::parse_ident as PrefixParseFn);
        self.register_prefix(TokenType::INT, Self::parse_integer_literal as PrefixParseFn);
        self.register_prefix(TokenType::BANG, Self::parse_prefix_expression);
        self.register_prefix(TokenType::MINUS, Self::parse_prefix_expression);
    }

    fn setup_infix_parse_fns(&mut self) {
        self.register_infix(TokenType::PLUS, Self::parse_infix_expression);
        self.register_infix(TokenType::MINUS, Self::parse_infix_expression);
        self.register_infix(TokenType::EQ, Self::parse_infix_expression);
        self.register_infix(TokenType::NOTEQ, Self::parse_infix_expression);
        self.register_infix(TokenType::SLASH, Self::parse_infix_expression);
        self.register_infix(TokenType::ASTERISK, Self::parse_infix_expression);
        self.register_infix(TokenType::GT, Self::parse_infix_expression);
        self.register_infix(TokenType::LT, Self::parse_infix_expression);
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

    fn current_precedence(&self) -> Precedence {
        match self.current_token.token_type {
            TokenType::EQ => Precedence::Equals,
            TokenType::NOTEQ => Precedence::Equals,
            TokenType::LT => Precedence::LessGreater,
            TokenType::GT => Precedence::LessGreater,
            TokenType::PLUS => Precedence::Sum,
            TokenType::MINUS => Precedence::Sum,
            TokenType::SLASH => Precedence::Product,
            TokenType::ASTERISK => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match self.peek_token.token_type {
            TokenType::EQ => Precedence::Equals,
            TokenType::NOTEQ => Precedence::Equals,
            TokenType::LT => Precedence::LessGreater,
            TokenType::GT => Precedence::LessGreater,
            TokenType::PLUS => Precedence::Sum,
            TokenType::MINUS => Precedence::Sum,
            TokenType::SLASH => Precedence::Product,
            TokenType::ASTERISK => Precedence::Product,
            _ => Precedence::Lowest,
        }
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
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let peek_token_type = self.peek_token.token_type.clone();

        if let TokenType::IDENT = peek_token_type {
            self.next_token();

            let ident = self.current_token.literal.clone();

            self.expect_peek(TokenType::ASSIGN);

            if self.current_token_is(TokenType::ASSIGN) {
                self.next_token();

                // todo
                while !self.peek_token_is(TokenType::SEMICOLON) {
                    self.next_token();
                }

                self.next_token();

                return Some(Statement::Let(ident.clone(), Expression::IntegerLiteral(0)));
            }
        }

        None
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::Return(Expression::IntegerLiteral(0)))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        if let Some(expression) = self.parse_expression(Precedence::Lowest) {
            if self.peek_token_is(TokenType::SEMICOLON) {
                self.next_token();
            }

            Some(Statement::Expression(expression))
        } else {
            None
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.prefix_parse_fns.get(&self.current_token.token_type);
        if prefix.is_none() {
            self.errors.push(format!(
                "no prefix parse function for {:?} found",
                self.current_token.token_type
            ));
            return None;
        }

        let mut left = prefix.unwrap()(self)?;

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix_opt = {
                let infix = self.infix_parse_fns.get(&self.peek_token.token_type);
                infix.map(|f| *f)
            };

            if infix_opt.is_none() {
                return Some(left);
            }

            self.next_token();

            let infix = infix_opt.unwrap();
            left = infix(self, left)?;
        }

        Some(left)
    }

    fn parse_ident(&mut self) -> Option<Expression> {
        if let TokenType::IDENT = self.current_token.token_type {
            Some(Expression::IDENT(self.current_token.literal.clone()))
        } else {
            None
        }
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        if let TokenType::INT = self.current_token.token_type {
            if let Ok(int) = self.current_token.literal.parse::<i64>() {
                Some(Expression::IntegerLiteral(int))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let operator = self.current_token.literal.clone();

        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;

        Some(Expression::Prefix(operator, Box::new(right)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let operator = self.current_token.literal.clone();
        let precedence = self.current_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix(Box::new(left), operator, Box::new(right)))
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
                Statement::Expression(Expression::IDENT("foobar".to_string())),
                Statement::Expression(Expression::Prefix(
                    "!".to_string(),
                    Box::new(Expression::IDENT("foo".to_string())),
                )),
            ],
        };

        let lexer = Lexer::new("");
        let parser = Parser::new(lexer);

        let program_string = parser.string(&program);

        let expected = vec![
            "let x = 10;\n".to_string(),
            "return 5;\n".to_string(),
            "foobar;\n".to_string(),
            "(!foo);\n".to_string(),
        ];

        assert_eq!(parser.errors.len(), 0, "Unvalid statements found");

        assert_eq!(
            program.statements.len(),
            4,
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

        let expected_idents = vec!["x", "y"];

        for (i, ident) in expected_idents.iter().enumerate() {
            match &program.statements[i] {
                Statement::Let(identifer, _) => {
                    assert_eq!(identifer, ident, "Unexpected IDENT name");
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
                    assert_eq!(value, v, "Unexpected IDENT name");
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
            5;
            10;
            !foo;
            -10;
            10 == 10;
            10 != 5;
            10 + 10;
            10 - 10;
            10 * 10;
            10 / 10
            10 > 5;
            10 < 15;
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
            14,
            "Unexpected number of statements"
        );

        let expected_values = vec![
            Expression::IDENT("foo".to_string()),
            Expression::IDENT("bar".to_string()),
            Expression::IntegerLiteral(5),
            Expression::IntegerLiteral(10),
            Expression::Prefix(
                "!".to_string(),
                Box::new(Expression::IDENT("foo".to_string())),
            ),
            Expression::Prefix("-".to_string(), Box::new(Expression::IntegerLiteral(10))),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "==".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "!=".to_string(),
                Box::new(Expression::IntegerLiteral(5)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "+".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "-".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "*".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                "/".to_string(),
                Box::new(Expression::IntegerLiteral(10)),
            ),
            Expression::Infix(
                Box::new(Expression::IntegerLiteral(10)),
                ">".to_string(),
                Box::new(Expression::IntegerLiteral(5)),
            ),
        ];

        for (i, v) in expected_values.iter().enumerate() {
            match &program.statements[i] {
                Statement::Expression(value) => {
                    assert_eq!(value, v, "Unexpected IDENT name");
                }
                _ => panic!("Unexpected statement type"),
            }
        }
    }
}
