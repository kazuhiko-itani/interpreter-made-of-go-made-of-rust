use crate::ast::{Expression, Program, Statement};
use crate::object::{BoolValue, Environment, Object, FALSE, NULL, TRUE};

pub fn eval(program: Program, env: &mut Environment) -> Object {
    eval_program(program, env)
}

fn eval_program(program: Program, env: &mut Environment) -> Object {
    let mut result = Object::Null(&NULL);

    for statement in program.statements {
        result = eval_statement(statement, env);

        match result {
            Object::ReturnValue(_) => return result,
            Object::Error(_) => return result,
            _ => {}
        }
    }

    result
}

fn eval_block_statements(statements: Vec<Statement>, env: &mut Environment) -> Object {
    let mut result = Object::Null(&NULL);

    for statement in statements {
        result = eval_statement(statement, env);

        match result {
            Object::ReturnValue(_) => return result,
            Object::Error(_) => return result,
            _ => {}
        }
    }

    result
}

fn eval_statement(statement: Statement, env: &mut Environment) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, env),
        Statement::Return(expr) => Object::ReturnValue(Box::new(eval_expression(expr, env))),
        Statement::Let(ident, value) => {
            let evaluated = eval_expression(value, env);
            env.set(ident, evaluated);
            Object::Null(&NULL)
        }
        _ => eval_expression(Expression::Ident("todo".to_string()), env),
    }
}

fn eval_expression(expression: Expression, env: &mut Environment) -> Object {
    match expression {
        Expression::IntegerLiteral(integer) => Object::Integer(integer),
        Expression::StringLiteral(string) => Object::String(string),
        Expression::Boolean(bool) => {
            if bool == "true" {
                Object::Boolean(&TRUE)
            } else {
                Object::Boolean(&FALSE)
            }
        }
        Expression::ArrayLiteral(elements) => {
            let mut evaluated_elements: Vec<Object> = vec![];

            for element in elements {
                let evaluated = eval_expression(element, env);
                if is_error(&evaluated) {
                    return evaluated;
                }

                evaluated_elements.push(evaluated)
            }

            Object::Array(evaluated_elements)
        }
        Expression::IndexExpression(left, index) => {
            let left = eval_expression(*left, env);
            if is_error(&left) {
                return left;
            }

            let index = eval_expression(*index, env);
            if is_error(&index) {
                return index;
            }

            eval_index_expression(left, index)
        }
        Expression::Prefix(op, expr) => {
            let right = eval_expression(*expr, env);
            if is_error(&right) {
                return right;
            }

            match op.as_str() {
                "!" => eval_bang_operator_expression(right),
                "-" => eval_minus_prefix_operator_expression(right),
                _ => new_error(format!("unknown operator: {}", op)),
            }
        }
        Expression::Infix(left, op, right) => {
            let evaluated_left: Object = eval_expression(*left, env);
            if is_error(&evaluated_left) {
                return evaluated_left;
            }

            let evaluated_right: Object = eval_expression(*right, env);
            if is_error(&evaluated_right) {
                return evaluated_right;
            }

            eval_infix_expression(op, evaluated_left, evaluated_right)
        }
        Expression::If(condition_expr, consequence, alternative) => {
            let condition = eval_expression(*condition_expr, env);
            if is_error(&condition) {
                return condition;
            }

            match is_truthy(condition) {
                true => eval_block_statements(consequence, env),
                false => match alternative {
                    Some(alt) => eval_block_statements(alt, env),
                    None => Object::Null(&NULL),
                },
            }
        }
        Expression::Ident(name) => eval_identifier(name, env),
        Expression::Function(args, body) => Object::Function(args, body, env.clone()),
        Expression::Call(function, args) => {
            let function = eval_expression(*function, env);
            if (is_error(&function)) {
                return function;
            }

            let arguments = args
                .into_iter()
                .map(|a| eval_expression(a, env))
                .collect::<Vec<_>>();
            if (arguments.len() == 1) && is_error(&arguments[0]) {
                return arguments[0].clone();
            }

            apply_function(function, arguments)
        }
        _ => eval_expression(Expression::Ident("todo".to_string()), env),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null(_) => false,
        Object::Boolean(bool) => bool.value,
        _ => true,
    }
}

fn eval_identifier(name: String, env: &mut Environment) -> Object {
    if let Some(value) = env.get(&name) {
        value.clone()
    } else {
        new_error(format!("identifier not found: {}", name))
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
        _ => new_error(format!("unknown operator: -{}", right)),
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
        (Object::Integer(left), Object::Boolean(right)) => {
            new_error(format!("type mismatch: {} {} {}", left, op, right))
        }
        (Object::Boolean(left), Object::Integer(right)) => {
            new_error(format!("type mismatch: {} {} {}", left, op, right))
        }
        (Object::String(left), Object::String(right)) => {
            eval_string_infix_expression(op, left, right)
        }
        _ => new_error(format!("unknown operator: {} {} {}", left, op, right)),
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
        _ => new_error((format!("unknown operator: {} {} {}", left, op, right))),
    }
}

fn eval_string_infix_expression(op: String, left: String, right: String) -> Object {
    if op != "+" {
        return new_error(format!("unknown operator: {} {} {}", left, op, right));
    }

    Object::String(left + &right)
}

fn eval_boolean_infix_expression(op: String, left: &BoolValue, right: &BoolValue) -> Object {
    match op.as_str() {
        "==" => Object::Boolean(bool_to_object(left.value == right.value)),
        "!=" => Object::Boolean(bool_to_object(left.value != right.value)),
        _ => new_error(format!("unknown operator: {} {} {}", left, op, right)),
    }
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left.clone(), index) {
        (Object::Array(elements), Object::Integer(index)) => {
            return eval_array_index_expression(elements, index)
        }
        _ => new_error(format!("index operator not supported: {}", left)),
    }
}

fn eval_array_index_expression(array: Vec<Object>, index: i64) -> Object {
    if index < 0 {
        return Object::Null(&NULL);
    }

    let index = index as usize;
    let max = array.len() - 1;

    if index.clone() > max {
        return Object::Null(&NULL);
    }

    array[index].clone()
}

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    match func {
        Object::Function(params, body, env) => {
            let mut extended_env = extend_function_env(env, params, args);
            let evaluated = eval_block_statements(body, &mut extended_env);

            match evaluated {
                Object::ReturnValue(value) => *value,
                _ => evaluated,
            }
        }

        Object::BuiltInFunction(func) => func(args),
        _ => new_error(format!("not a function: {}", func)),
    }
}

// @TODO outer.envのような設計のほうがベターな気がする
fn extend_function_env(env: Environment, params: Vec<String>, args: Vec<Object>) -> Environment {
    let mut store = env.store.clone();
    for (param, arg) in params.into_iter().zip(args.into_iter()) {
        store.insert(param, arg);
    }

    Environment {
        store,
        builtins: env.builtins,
    }
}

fn bool_to_object(input: bool) -> &'static BoolValue {
    if input {
        &TRUE
    } else {
        &FALSE
    }
}

pub fn new_error(format: String) -> Object {
    Object::Error(format!("{}", format))
}

fn is_error(obj: &Object) -> bool {
    match obj {
        Object::Error(_) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins;
    use crate::parse::Parser;
    use crate::tokenize::Lexer;

    #[test]
    fn test_eval_integer() {
        let inputs = vec![
            "5;",
            "10;",
            "-5;",
            "-10;",
            "5 + 5 + 5 + 5 - 10;",
            "2 * 2 * 2 * 2 * 2;",
            "-50 + 100 + -50;",
            "5 * 2 + 10;",
            "5 + 2 * 10;",
            "20 + 2 * -10;",
            "50 / 2 * 2 + 10;",
            "2 * (5 + 10);",
            "3 * 3 * 3 + 10;",
            "3 * (3 * 3) + 10;",
            "(5 + 10 * 2 + 15 / 3) * 2 + -10;",
        ];

        let expected_list = vec![5, 10, -5, -10, 10, 32, 0, 20, 25, 0, 60, 30, 37, 37, 50];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

            match result {
                Object::Integer(integer) => {
                    assert_eq!(integer, expected_list[i], "Unexpected integer value");
                }
                _ => panic!("Unexpected object type"),
            }
        }
    }

    #[test]
    fn test_eval_string() {
        let input = "\"Hello World!\"";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let mut env = create_env();
        let result = eval(program, &mut env);

        match result {
            Object::String(string) => {
                assert_eq!(string, "Hello World!", "Unexpected string value");
            }
            _ => panic!("Unexpected object type, {}", result),
        }
    }

    #[test]
    fn test_string_cancatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let mut env = create_env();
        let result = eval(program, &mut env);

        match result {
            Object::String(string) => {
                assert_eq!(string, "Hello World!", "Unexpected string value");
            }
            _ => panic!("Unexpected object type, {}", result),
        }
    }

    #[test]
    fn test_eval_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let expected_list = vec![1, 4, 6];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let mut env = create_env();
        let result = eval(program, &mut env);

        match result {
            Object::Array(array) => {
                for (i, value) in array.iter().enumerate() {
                    match value {
                        Object::Integer(integer) => {
                            assert_eq!(integer, &expected_list[i], "Unexpected integer value");
                        }
                        _ => panic!("Unexpected object type"),
                    }
                }
            }
            _ => panic!("Unexpected object type, {}", result),
        }
    }

    #[test]
    fn test_eval_array_index_expressions() {
        let inputs = vec![
            "[1, 2, 3][0];",
            "[1, 2, 3][1];",
            "let i = 0; [1][i];",
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            "let myArray = [1, 2, 3] let i = myArray[0]; myArray[i];",
        ];

        let expected_list = vec![1, 2, 1, 6, 2];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

            match result {
                Object::Integer(integer) => {
                    assert_eq!(integer, expected_list[i], "Unexpected integer value");
                }
                _ => panic!("Unexpected object type. {}", result),
            }
        }

        let wrong_inputs = vec!["[1, 2, 3][3];", "[1, 2, 3][-1];"];

        for (_, input) in wrong_inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

            match result {
                Object::Null(_) => {}
                _ => panic!("Unexpected object type. {}", result),
            }
        }
    }

    #[test]
    fn test_eval_builtin_functions() {
        let inputs = vec![
            "len(\"\");",
            "len(\"four\");",
            "len(\"hello world\");",
            "len([1, 2, 3])",
            "first([1, 2, 3])",
            "last([1, 2, 3])",
        ];

        let expected_list = vec![0, 4, 11, 3, 1, 3];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

            match result {
                Object::Integer(integer) => {
                    assert_eq!(integer, expected_list[i], "Unexpected integer value");
                }
                _ => panic!("Unexpected object type. {}", result),
            }
        }

        let wrong_inputs = vec!["len(1);", "len(\"one\", \"two\");", "first(1)", "last(1)"];

        let expected_error_list = vec![
            "argument to `len` not supported, got 1",
            "wrong number of arguments. got=2, want=1",
            "argument to `first` must be ARRAY, got 1",
            "argument to `last` must be ARRAY, got 1",
        ];

        for (i, input) in wrong_inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

            match result {
                Object::Error(msg) => {
                    assert_eq!(msg, expected_error_list[i], "Unexpected integer value");
                }
                _ => panic!("Unexpected object type. {}", result),
            }
        }
    }

    #[test]
    fn test_eval_builtin_rest() {
        let input = "rest([1, 2, 3])";

        let expected = vec![2, 3];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let mut env = create_env();
        let result = eval(program, &mut env);

        match result {
            Object::Array(array) => {
                for (i, value) in array.iter().enumerate() {
                    match value {
                        Object::Integer(integer) => {
                            assert_eq!(integer, &expected[i], "Unexpected integer value");
                        }
                        _ => panic!("Unexpected object type"),
                    }
                }
            }
            _ => panic!("Unexpected object type. {}", result),
        }
    }

    #[test]
    fn test_eval_builtin_push() {
        let input = "push([1, 2, 3], 4)";

        let expected = vec![1, 2, 3, 4];

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let mut env = create_env();
        let result = eval(program, &mut env);

        match result {
            Object::Array(array) => {
                for (i, value) in array.iter().enumerate() {
                    match value {
                        Object::Integer(integer) => {
                            assert_eq!(integer, &expected[i], "Unexpected integer value");
                        }
                        _ => panic!("Unexpected object type"),
                    }
                }
            }
            _ => panic!("Unexpected object type. {}", result),
        }
    }

    #[test]
    fn test_eval_bool() {
        let inputs = vec![
            "true;",
            "false;",
            "1 < 2;",
            "1 > 2;",
            "1 < 1;",
            "1 > 1;",
            "1 == 1;",
            "1 != 1;",
            "1 == 2;",
            "1 != 2;",
            "(1 < 2) == true;",
            "(1 < 2) == false;",
            "(1 > 2) == true;",
            "(1 > 2) == false;",
        ];

        let expected_list = vec![
            true, false, true, false, false, false, true, false, false, true, true, false, false,
            true,
        ];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

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
    fn test_return_eval() {
        let inputs = vec![
            "return 10;",
            "return 10; 9;",
            "return 2 * 5; 9;",
            "9; return 2 * 5; 9;",
            "if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }",
            "if (10 > 1) {
                if (0 > 1) {
                    return 10;
                } else {
                    return 5;
                }
                return 1;
            }",
        ];

        let expected_list = vec![10, 10, 10, 10, 10, 5];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            let mut env = create_env();
            let result = eval(program, &mut env);

            println!("{}", result);

            match result {
                Object::ReturnValue(obj) => match *obj {
                    Object::Integer(integer) => {
                        assert_eq!(integer, expected_list[i], "Unexpected integer value");
                    }
                    _ => panic!("Unexpected object type. actual {}", obj),
                },
                _ => panic!("Unexpected object type. actual {}", result),
            }
        }
    }

    #[test]
    fn test_bang_operator_eval() {
        let inputs = vec!["!true;", "!false;", "!5;", "!!true;", "!!false;", "!!5;"];

        let expected_list = vec![false, true, false, true, false, true];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

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
        let inputs = vec![
            "if (true) { 10 };",
            "if (false) { 10 };",
            "if (1) { 10 };",
            "if (1 < 2) { 10 };",
            "if (1 > 2) { 10 };",
            "if (1 > 2) { 10 } else { 20 };",
            "if (1 < 2) { 10 } else { 20 };",
        ];

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

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

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

    #[test]
    fn test_error_handling() {
        let inputs = vec![
            "5 + true",
            "5 + true; 5;",
            "-true",
            "true + false",
            "5; true + false; 5;",
            "if (10 > 1) { true + false }",
            "foobar",
            "\"Hello\" - \"World\"",
        ];

        let expected_list = vec![
            "type mismatch: 5 + true",
            "type mismatch: 5 + true",
            "unknown operator: -true",
            "unknown operator: true + false",
            "unknown operator: true + false",
            "unknown operator: true + false",
            "identifier not found: foobar",
            "unknown operator: Hello - World",
        ];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

            match result {
                Object::Error(msg) => {
                    assert_eq!(msg, expected_list[i], "Unexpected error message");
                }
                _ => panic!("Unexpected object type. {}", result),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            "let a = 5; a;",
            "let a = 5 * 5; a;",
            "let a = 5; let b = a; b;",
            "let a = 5; let b = a; let c = a + b + 5; c;",
        ];

        let expected_list = vec![5, 25, 5, 15];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

            match result {
                Object::Integer(integer) => {
                    assert_eq!(integer, expected_list[i], "Unexpected integer");
                }
                _ => panic!("Unexpected object type. {}", result),
            }
        }
    }

    #[test]
    fn test_function_parsing() {
        let input = "fn(x, y) { x + y; }";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let mut env = create_env();
        let result = eval(program, &mut env);

        match result {
            Object::Function(args, body, _) => {
                assert_eq!(args[0], "x", "Unexpected arg");
                assert_eq!(args[1], "y", "Unexpected arg");

                assert_eq!(body.len(), 1, "Unexpected body length");
                assert_eq!(body[0].to_string(), "(x + y)", "Unexpected body");
            }
            _ => panic!("Unexpected object type. {}", result),
        }
    }

    #[test]
    fn test_function_application() {
        let inputs = vec![
            "let identity = fn(x) { x; }; identity(5);",
            "let identity = fn(x) { return x; }; identity(5);",
            "let double = fn(x) { x * 2; }; double(5);",
            "let add = fn(x, y) { x + y; }; add(5, 5);",
            "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            "fn(x) { x; }(5)",
        ];

        let expected_list = vec![5, 5, 10, 10, 20, 5];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let mut env = create_env();
            let result = eval(program, &mut env);

            match result {
                Object::Integer(integer) => {
                    assert_eq!(integer, expected_list[i], "Unexpected integer");
                }
                _ => panic!("Unexpected object type. {}", result),
            }
        }
    }

    #[test]
    fn test_closure() {
        let input = "
        let newAdder = fn(x) {
            fn(y) { x + y };
        };
        let addTwo = newAdder(2);
        addTwo(2);
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let mut env = create_env();
        let result = eval(program, &mut env);

        match result {
            Object::Integer(integer) => {
                assert_eq!(integer, 4, "Unexpected integer");
            }
            _ => panic!("Unexpected object type. {}", result),
        }
    }

    fn create_env() -> Environment {
        let mut env = Environment::new();

        env.register_builtin("len", builtins::builtin_len);
        env.register_builtin("first", builtins::builtin_first);
        env.register_builtin("last", builtins::builtin_last);
        env.register_builtin("rest", builtins::builtin_rest);
        env.register_builtin("push", builtins::builtin_push);

        env
    }
}
