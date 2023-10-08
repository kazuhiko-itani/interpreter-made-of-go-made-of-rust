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
        _ => new_error(format!("argument to `len` not supported, got {}", args[0])),
    }
}
