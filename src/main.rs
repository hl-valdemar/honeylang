use std::process::exit;

use crate::parser::ast::AstNode;

mod lexer;
mod parser;

fn main() {
    println!("\n::[[ Lexer ]]::\n");

    let (tokens, errors) = lexer::scan("examples/simple_const_declarations.hon");

    if errors.has_errors() {
        println!("{}", errors);
        exit(1);
    }

    println!("Scanned {} tokens:\n\n{}", tokens.len(), tokens);

    println!("\n::[[ Parser ]]::\n");

    let (ast, errors) = parser::parse(&tokens);
    if errors.has_errors() {
        println!("{}", errors);
        exit(1);
    }

    let AstNode::Program { declarations } = ast.clone() else {
        panic!("Expected program after parsing");
    };

    println!("Parsed {} declarations:\n\n{}", declarations.len(), ast);
}
