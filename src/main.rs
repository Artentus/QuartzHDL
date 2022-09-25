#![feature(trait_alias)]

mod ast;
mod fmt;
mod lexer;
mod parser;

use langbox::*;
use lexer::*;
use std::rc::Rc;

type SharedString = Rc<str>;

const TEST_FILE: &str = include_str!("../test.qrz");

fn main() -> std::io::Result<()> {
    let mut file_server = FileServer::new();
    let file = file_server.register_file_memory("<test>", TEST_FILE);

    let lexer = QuartzLexer::new(file, &file_server);
    let tokens: Vec<_> = lexer
        .filter(|t| {
            if let QuartzToken::Comment(_) = &t.kind {
                false
            } else {
                true
            }
        })
        .collect();

    match parser::design().run(TokenStream::new(&tokens)) {
        ParseResult::Match { value: design, .. } => {
            for (i, item) in design.iter().enumerate() {
                if i > 0 {
                    print!("\n\n");
                }
                print!("{}", item);
            }
        }
        ParseResult::NoMatch => panic!("input did not match"),
        ParseResult::Err(err) => panic!("parse error: {:?}", err),
    }

    Ok(())
}
