use {
    crate::{
        evaluator::{tree_walker::TreeWalker, Environment, Evaluator},
        lexer::Lexer,
        parser::{ast::Node, Error as ParserError, Parser},
    },
    std::{
        cell::RefCell,
        io::{self, BufRead, Result as IoResult, Write},
        rc::Rc,
        sync::mpsc,
        time::Duration,
    },
};

const PROMPT: &[u8] = b">> ";

pub fn init() {
    let stdin = io::stdin();
    let read_handle = stdin.lock();
    let stdout = io::stdout();
    let write_handle = stdout.lock();

    start(read_handle, write_handle);
}

fn start<R: BufRead, W: Write>(mut ip: R, mut out: W) {
    out.write_all(b"Welcome to Monkey! Press Ctrl+C twice and enter to quit\n")
        .unwrap();

    let (send, recv) = mpsc::channel();

    let mut should_quit = false;
    ctrlc::set_handler(move || {
        send.send(()).expect("error in sending signal to channel");
    })
    .expect("error in setting Ctrl+C handler");

    let environment = Rc::new(RefCell::new(Environment::new()));
    loop {
        out.write_all(PROMPT).unwrap();
        out.flush().unwrap();
        if recv.recv_timeout(Duration::from_millis(5)).is_ok() {
            if should_quit {
                std::process::exit(0);
            }
            should_quit = true;
            out.write_all(b"\r                                                          \r")
                .unwrap();
            continue;
        } else {
            should_quit = false;
        }

        let mut s = String::new();
        ip.read_line(&mut s).unwrap();

        let tokens = Lexer::new(&s);
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program();
        if !parser.errors.is_empty() {
            print_parser_errors(&mut out, &parser.errors).unwrap();
            continue;
        }
        let program = program.unwrap();
        let evaluator = TreeWalker::new();
        let obj = evaluator.eval(Node::Program(program), environment.clone());
        if let Some(node) = obj {
            out.write_fmt(format_args!("{}\n", node.inspect())).unwrap();
            out.flush().unwrap();
        }
    }
}

fn print_parser_errors<W: Write>(mut out: W, errors: &[ParserError]) -> IoResult<()> {
    for error in errors {
        out.write_fmt(format_args!("\t{}\n", error)).unwrap();
    }
    out.flush()
}
