#[macro_use]
extern crate lazy_static;

#[allow(clippy::new_ret_no_self)]
mod evaluator;

#[macro_use]
mod lexer;
mod parser;
mod repl;

fn main() {
    repl::init();
}
