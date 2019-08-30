use crate::lexer::Lexer;

pub trait Node {
    fn token_literal(&self) -> String;
}
pub trait Statement: Node {
    fn parse(&self, &mut Lexer) -> Box<dyn Statement>;
}

pub trait Expression: Node {}

pub struct Let {
    name: Identifier,
    // value: dyn Expression,
}

impl Node for Let {
    fn token_literal(&self) -> String {
        "let".to_owned()
    }
}

impl Statement for Let {
    fn parse(&self, lexer: &mut Lexer) -> Vec<dyn Statement> {
        vec![Let {
            name: Identifier { name: "potato" },
            // value: "",
        }]
    }
}

pub struct Identifier {
    name: &'static str,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        "IDENT".to_owned()
    }
}
impl Expression for Identifier {}
