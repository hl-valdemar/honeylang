use owo_colors::OwoColorize;

use crate::{parser::ast::AstNode, semantic::symtab::SymbolTable};

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for symbol in self.symbols.values() {
            let type_fmt = if let Some(t) = symbol.type_ {
                &format!("{}", t)
            } else {
                "unresolved"
            };

            let value_fmt = match &symbol.value {
                AstNode::Ident(name) => &format!("{}", name),
                AstNode::Num(number) => &format!("{}", number),
                AstNode::Bool(val) => &format!("{}", val),
                _ => "unresolved",
            };

            writeln!(
                f,
                "({}) {}: {} = {}",
                symbol.kind.green(),
                symbol.name,
                type_fmt.cyan(),
                value_fmt.to_string().purple(),
            )?;
        }

        Ok(())
    }
}
