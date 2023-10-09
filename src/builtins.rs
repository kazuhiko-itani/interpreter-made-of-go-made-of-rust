use crate::evaluator::new_error;
use crate::object::Object;

pub fn builtin_len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::String(string) => Object::Integer(string.len() as i64),
        Object::Array(array) => Object::Integer(array.len() as i64),
        _ => new_error(format!("argument to `len` not supported, got {}", args[0])),
    }
}

pub fn builtin_first(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(array) => array[0].clone(),
        _ => new_error(format!(
            "argument to `first` must be ARRAY, got {}",
            args[0]
        )),
    }
}

pub fn builtin_last(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return new_error(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ));
    }

    match &args[0] {
        Object::Array(array) => array[array.len() - 1].clone(),
        _ => new_error(format!("argument to `last` must be ARRAY, got {}", args[0])),
    }
}
