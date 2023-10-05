use crate::ast::{Expression, Program, Statement};
use crate::object::{Object, FALSE, TRUE};

pub fn eval(program: Program) -> Vec<Object> {
    let mut results = Vec::new();

    for statement in program.statements {
        let result = eval_statement(statement);
        results.push(result);
    }

    results
}

fn eval_statement(statement: Statement) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        _ => eval_expression(Expression::Ident("todo".to_string())),
    }
}

fn eval_expression(expression: Expression) -> Object {
    match expression {
        Expression::IntegerLiteral(integer) => Object::Integer(integer),
        Expression::Boolean(bool) => {
            if bool == "true" {
                Object::Boolean(&TRUE)
            } else {
                Object::Boolean(&FALSE)
            }
        }
        Expression::Prefix(op, expr) => {
            let right = eval_expression(*expr);

            match op.as_str() {
                "!" => eval_bang_operator_expression(right),
                "-" => eval_minus_prefix_operator_expression(right),
                _ => eval_expression(Expression::Ident("todo".to_string())),
            }
        }
        _ => eval_expression(Expression::Ident("todo".to_string())),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(bool) => {
            if bool.value {
                Object::Boolean(&FALSE)
            } else {
                Object::Boolean(&TRUE)
            }
        }
        _ => Object::Boolean(&FALSE),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(integer) => Object::Integer(-integer),
        _ => panic!("Unknown operator: -{}", right),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Parser;
    use crate::tokenize::Lexer;

    #[test]
    fn test_eval_integer() {
        let input = "
            5;
            10;
            -5;
            -10;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let result = eval(program);

        assert_eq!(result.len(), 4, "Unexpected number of object");

        let expected_list = vec![5, 10, -5, -10];

        for (i, expected) in expected_list.iter().enumerate() {
            match &result[i] {
                Object::Integer(integer) => {
                    assert_eq!(integer, expected, "Unexpected integer value");
                }
                _ => panic!("Unexpected object type"),
            }
        }
    }

    #[test]
    fn test_eval_bool() {
        let input = "
            true;
            false;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let result = eval(program);

        assert_eq!(result.len(), 2, "Unexpected number of object");

        let expected_list = vec![true, false];

        for (i, expected) in expected_list.iter().enumerate() {
            match &result[i] {
                Object::Boolean(bool_value) => {
                    assert_eq!(bool_value.value, *expected, "Unexpected boolean value");
                }
                _ => panic!("Unexpected object type"),
            }
        }
    }

    #[test]
    fn test_bang_operator_eval() {
        let input = "
            !true;
            !false;
            !5;
            !!true;
            !!false;
            !!5;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let result = eval(program);

        assert_eq!(result.len(), 6, "Unexpected number of object");

        let expected_list = vec![false, true, false, true, false, true];

        for (i, expected) in expected_list.iter().enumerate() {
            match &result[i] {
                Object::Boolean(bool_value) => {
                    assert_eq!(bool_value.value, *expected, "Unexpected boolean value");
                }
                _ => panic!("Unexpected object type"),
            }
        }
    }
}
