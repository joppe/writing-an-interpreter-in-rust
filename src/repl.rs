use std::{
    cell::RefCell,
    io::{stdin, stdout, Write},
    rc::Rc,
};

use crate::{environment::Environment, eval::eval, lexer::Lexer, parser::Parser};

pub fn start() {
    let environment: Rc<RefCell<Environment>> = Rc::new(RefCell::new(Environment::new()));

    loop {
        let input = ask_input(">> ");

        if input.trim() == "exit" {
            break;
        }

        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            for error in parser.errors() {
                println!("\t{}", error);
            }

            continue;
        }

        let evaluated = eval(program, Rc::clone(&environment));
        println!("{}", evaluated);
    }
}

fn ask_input(prompt: &str) -> String {
    let mut stdout = stdout();
    let stdin = stdin();

    print!("{}", prompt);

    let mut input = String::new();

    // Flush the buffer to ensure the prompt is displayed
    stdout.flush().unwrap();
    stdin.read_line(&mut input).unwrap();

    input
}
