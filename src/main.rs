#[macro_use]
extern crate lazy_static;

mod evaluator;
mod lexer;
mod parser;
mod repl;

fn main() {
    repl::init();
}
