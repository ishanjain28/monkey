use crate::lexer::Lexer;
use std::io::{self, BufRead, Write};

const PROMPT: &'static str = ">> ";

pub fn init() {
    let stdin = io::stdin();
    let read_handle = stdin.lock();
    let stdout = io::stdout();
    let write_handle = stdout.lock();

    start(read_handle, write_handle);
}

fn start<R: BufRead, W: Write>(mut ip: R, mut out: W) {
    loop {
        out.write_fmt(format_args!("{}", PROMPT)).unwrap();
        out.flush().unwrap();
        let mut s = String::new();
        ip.read_line(&mut s).unwrap();
        let tokens = Lexer::new(&s);

        for token in tokens {
            println!("{:?}", token);
        }
    }
}
