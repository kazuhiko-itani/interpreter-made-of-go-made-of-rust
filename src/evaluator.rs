use crate::ast::{Expression, Program, Statement};
use crate::object::{BoolValue, Object, FALSE, NULL, TRUE};

pub fn eval(program: Program) -> Object {
    eval_program(program)
}

fn eval_program(program: Program) -> Object {
    let mut result = Object::Null(&NULL);

    for statement in program.statements {
        result = eval_statement(statement);
    }

    result
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
        Expression::Infix(left, op, right) => {
            let evaluated_left: Object = eval_expression(*left);
            let evaluated_right: Object = eval_expression(*right);
            eval_infix_expression(op, evaluated_left, evaluated_right)
        }
        Expression::If(condition_expr, consequence, alternative) => {
            let condition = eval_expression(*condition_expr);

            match is_truthy(condition) {
                true => eval(Program {
                    statements: consequence,
                }),
                false => match alternative {
                    Some(alt) => eval(Program { statements: alt }),
                    None => Object::Null(&NULL),
                },
            }
        }
        _ => eval_expression(Expression::Ident("todo".to_string())),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null(_) => false,
        Object::Boolean(bool) => bool.value,
        _ => true,
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

fn eval_infix_expression(op: String, left: Object, right: Object) -> Object {
    match (left.clone(), right.clone()) {
        (Object::Integer(left), Object::Integer(right)) => {
            eval_integer_infix_expression(op, left, right)
        }
        (Object::Boolean(left), Object::Boolean(right)) => {
            eval_boolean_infix_expression(op, left, right)
        }
        _ => panic!("Unknown operator: {} {} {}", left, op, right),
    }
}

fn eval_integer_infix_expression(op: String, left: i64, right: i64) -> Object {
    match op.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(bool_to_object(left < right)),
        ">" => Object::Boolean(bool_to_object(left > right)),
        "==" => Object::Boolean(bool_to_object(left == right)),
        "!=" => Object::Boolean(bool_to_object(left != right)),
        _ => panic!("Unknown operator: {} {} {}", left, op, right),
    }
}

fn eval_boolean_infix_expression(op: String, left: &BoolValue, right: &BoolValue) -> Object {
    match op.as_str() {
        "==" => Object::Boolean(bool_to_object(left.value == right.value)),
        "!=" => Object::Boolean(bool_to_object(left.value != right.value)),
        _ => panic!("Unknown operator: {} {} {}", left, op, right),
    }
}

fn bool_to_object(input: bool) -> &'static BoolValue {
    if input {
        &TRUE
    } else {
        &FALSE
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
            5 + 5 + 5 + 5 - 10;
            2 * 2 * 2 * 2 * 2;
            -50 + 100 + -50;
            5 * 2 + 10;
            5 + 2 * 10;
            20 + 2 * -10;
            50 / 2 * 2 + 10;
            2 * (5 + 10);
            3 * 3 * 3 + 10;
            3 * (3 * 3) + 10;
            (5 + 10 * 2 + 15 / 3) * 2 + -10;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 15, "Unexpected number of object");

        let expected_list = vec![5, 10, -5, -10, 10, 32, 0, 20, 25, 0, 60, 30, 37, 37, 50];

        for (i, statement) in program.statements.iter().enumerate() {
            let result = eval_statement(statement.clone());

            match result {
                Object::Integer(integer) => {
                    assert_eq!(integer, expected_list[i], "Unexpected integer value");
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
            1 < 2;
            1 > 2;
            1 < 1;
            1 > 1;
            1 == 1;
            1 != 1;
            1 == 2;
            1 != 2;
            (1 < 2) == true;
            (1 < 2) == false;
            (1 > 2) == true;
            (1 > 2) == false;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 14, "Unexpected number of object");

        let expected_list = vec![
            true, false, true, false, false, false, true, false, false, true, true, false, false,
            true,
        ];

        for (i, statement) in program.statements.iter().enumerate() {
            let result = eval_statement(statement.clone());

            match result {
                Object::Boolean(bool_value) => {
                    assert_eq!(
                        bool_value.value, expected_list[i],
                        "Unexpected boolean value"
                    );
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

        assert_eq!(program.statements.len(), 6, "Unexpected number of object");

        let expected_list = vec![false, true, false, true, false, true];

        for (i, statement) in program.statements.iter().enumerate() {
            let result = eval_statement(statement.clone());

            match result {
                Object::Boolean(bool_value) => {
                    assert_eq!(
                        bool_value.value, expected_list[i],
                        "Unexpected boolean value"
                    );
                }
                _ => panic!("Unexpected object type"),
            }
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "
            if (true) { 10 };
            if (false) { 10 };
            if (1) { 10 };
            if (1 < 2) { 10 };
            if (1 > 2) { 10 };
            if (1 > 2) { 10 } else { 20 };
            if (1 < 2) { 10 } else { 20 };
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        assert_eq!(program.statements.len(), 7, "Unexpected number of object");

        enum IntOrNone {
            IntValue(i64),
            NoneValue,
        }

        let expected_list: Vec<IntOrNone> = vec![
            IntOrNone::IntValue(10),
            IntOrNone::NoneValue,
            IntOrNone::IntValue(10),
            IntOrNone::IntValue(10),
            IntOrNone::NoneValue,
            IntOrNone::IntValue(20),
            IntOrNone::IntValue(10),
        ];

        for (i, statement) in program.statements.iter().enumerate() {
            let result = eval_statement(statement.clone());

            match result {
                Object::Integer(integer) => match expected_list[i] {
                    IntOrNone::IntValue(expected) => {
                        assert_eq!(integer, expected, "Unexpected integer value");
                    }
                    _ => panic!("Unexpected object type"),
                },
                Object::Null(_) => match expected_list[i] {
                    IntOrNone::NoneValue => {}
                    _ => panic!("Unexpected object type"),
                },
                _ => panic!("Unexpected object type"),
            }
        }
    }
}
