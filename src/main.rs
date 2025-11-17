use std::process::exit;

use crate::parser::ast::AstNode;

mod lexer;
mod parser;
mod semantic;

use clap::Parser;

#[derive(Parser)]
#[command(name = "honey")]
#[command(about = "A programming language", long_about = None)]
struct Cli {
    /// Input file to process
    #[arg(short, long)]
    input: String,
    // /// Output file
    // #[arg(short, long)]
    // output: String,
}

#[cfg(debug_assertions)]
fn main() {
    let cli = Cli::parse();
    let filename: &'static str = Box::leak(cli.input.into_boxed_str());

    println!("\n::[[ Scanning ]]::\n");

    let (tokens, errors) = lexer::scan(filename);

    if errors.has_errors() {
        println!("{}", errors);
        exit(1);
    }

    println!("Scanned {} tokens:\n\n{}", tokens.len(), tokens);

    println!("\n::[[ Parsing ]]::\n");

    let (ast, errors) = parser::parse(&tokens);
    if errors.has_errors() {
        println!("{}", errors);
        exit(1);
    }

    let AstNode::Program { declarations } = ast.clone() else {
        panic!("Expected program after parsing");
    };

    println!("Parsed {} declarations:\n\n{}", declarations.len(), ast);

    println!("\n::[[ Semantic ]]::\n");

    let (ast, symtab, errors) = semantic::analyze(&ast);
    if errors.has_errors() {
        println!("{}", errors);
        exit(1);
    }

    println!("Collected {} symbols:\n\n{}", symtab.len(), symtab);

    println!("\n::[[ Codegen ]]::\n");
    println!("the void stares back");
}

#[cfg(not(debug_assertions))]
fn main() {
    let cli = Cli::parse();
    let filename: &'static str = Box::leak(cli.input.into_boxed_str());

    let (tokens, errors) = lexer::scan(filename);

    if errors.has_errors() {
        println!("{}", errors);
        exit(1);
    }

    let (ast, errors) = parser::parse(&tokens);
    if errors.has_errors() {
        println!("{}", errors);
        exit(1);
    }
}
