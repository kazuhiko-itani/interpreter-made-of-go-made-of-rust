use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

mod ast;
mod builtins;
mod evaluator;
mod object;
mod parse;
mod tokenize;

struct Repl {}

impl Repl {
    fn new() -> Self {
        Repl {}
    }

    fn start(&self) -> Result<()> {
        let mut rl = DefaultEditor::new()?;
        let mut env = object::Environment::new();
        self.set_builtin_func_to_env(&mut env);

        loop {
            let readline = rl.readline(">> ");

            match readline {
                Ok(line) => {
                    self.parse_input(&line, &mut env);
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                    println!("Goodbye!");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }

        Ok(())
    }

    fn parse_input(&self, input: &str, env: &mut object::Environment) {
        let lexer = tokenize::Lexer::new(input);
        let mut parser = parse::Parser::new(lexer);

        let program = parser.parse_program();

        if parser.has_errors() {
            for err in parser.errors() {
                println!("{}", err);
            }
        } else {
            // println!("{:?}", parser.string(&program));
            let result = evaluator::eval(program, env);
            println!("{}", result);
        }
    }

    fn set_builtin_func_to_env(&self, env: &mut object::Environment) {
        env.register_builtin("len", builtins::builtin_len);
        env.register_builtin("first", builtins::builtin_first);
        env.register_builtin("last", builtins::builtin_last);
        env.register_builtin("rest", builtins::builtin_rest);
        env.register_builtin("push", builtins::builtin_push);
    }
}

fn main() {
    let repl = Repl::new();
    repl.start().unwrap();
}
