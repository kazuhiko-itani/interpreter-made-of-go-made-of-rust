use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

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
        _ => eval_expression(Expression::Ident("todo".to_string())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Parser;
    use crate::tokenize::Lexer;

    #[test]
    fn test_eval() {
        let input = "
            5;
            10;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let result = eval(program);

        assert_eq!(result.len(), 2, "Unexpected number of object");

        let expected_list = vec![5, 10];

        for (i, expected) in expected_list.iter().enumerate() {
            match &result[i] {
                Object::Integer(integer) => {
                    assert_eq!(integer, expected, "Unexpected integer value");
                }
                _ => panic!("Unexpected object type"),
            }
        }
    }
}
