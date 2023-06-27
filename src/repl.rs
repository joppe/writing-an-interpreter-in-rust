use std::io::{stdin, stdout, Write};

use crate::{lexer, token};

pub fn start() {
    loop {
        let input = ask_input();

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

fn ask_input() -> String {
    let mut stdout = stdout();
    let stdin = stdin();

    print!(">> ");

    let mut input = String::new();
    stdout.flush().unwrap();
    stdin.read_line(&mut input).unwrap();

    input
}
