use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

mod ast;
mod parse;
mod tokenize;

struct Repl {}

impl Repl {
    fn new() -> Self {
        Repl {}
    }

    fn start(&self) -> Result<()> {
        let mut rl = DefaultEditor::new()?;

        loop {
            let readline = rl.readline(">> ");

            match readline {
                Ok(line) => {
                    self.parse_input(&line);
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

    fn parse_input(&self, input: &str) {
        let lexer = tokenize::Lexer::new(input);
        let mut parser = parse::Parser::new(lexer);

        let program = parser.parse_program();

        if parser.has_errors() {
            for err in parser.errors() {
                println!("{}", err);
            }
        } else {
            println!("{:?}", parser.string(&program));
        }
    }
}

fn main() {
    let repl = Repl::new();
    repl.start().unwrap();
}
