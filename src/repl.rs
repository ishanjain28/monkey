use crate::{
    lexer::Lexer,
    parser::{Error as ParserError, Parser},
};
use std::io::{self, BufRead, Result as IoResult, Write};

const PROMPT: &[u8] = b">> ";

pub fn init() {
    let stdin = io::stdin();
    let read_handle = stdin.lock();
    let stdout = io::stdout();
    let write_handle = stdout.lock();

    start(read_handle, write_handle);
}

fn start<R: BufRead, W: Write>(mut ip: R, mut out: W) {
    loop {
        out.write_all(PROMPT).unwrap();
        out.flush().unwrap();
        let mut s = String::new();
        ip.read_line(&mut s).unwrap();

        let tokens = Lexer::new(&s);
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            print_parser_errors(&mut out, &parser.errors).unwrap();
            continue;
        }
        match program {
            Some(prg) => {
                for stmt in &prg.statements {
                    out.write_fmt(format_args!("{}\n", stmt)).unwrap();
                }
            }
            None => (),
        };
    }
}

fn print_parser_errors<W: Write>(mut out: W, errors: &[ParserError]) -> IoResult<()> {
    for error in errors {
        out.write_fmt(format_args!("\t{}\n", error)).unwrap();
    }
    out.flush()
}
