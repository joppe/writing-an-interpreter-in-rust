use writing_an_interpreter_in_rust::repl;

fn main() {
    let username = std::env::var("LOGNAME").unwrap_or_else(|_| "anonymous".to_string());

    println!("Welcome {} to the Monkey programming language!", username);
    println!("Feel free to type in commands");

    repl::start();
}
