#[macro_use]
extern crate lazy_static;

mod lexer;
mod parser;
mod repl;

fn main() {
    repl::init();
}
