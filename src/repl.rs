use std::io::{stdin, stdout, Write};

use crate::{lexer, token};

pub fn start() {
    loop {
        let input = ask_input(">> ");

        if input.trim() == "exit" {
            break;
        }

        let mut lexer = lexer::Lexer::new(input);

        loop {
            let token = lexer.next_token();

            if token == token::Token::Eof {
                break;
            }

            println!("{:?}", token);
        }
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
