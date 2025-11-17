use crate::parser::ast::{AstNode, ConstDeclKind, ResolvedType};

use std::collections::HashMap;

mod display;

pub struct SymbolTable {
    symbols: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn has_symbol(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn register(&mut self, name: &str, symbol: Symbol) -> Option<Symbol> {
        self.symbols.insert(name.to_string(), symbol)
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }
}

pub struct Symbol {
    name: String,
    kind: ConstDeclKind,
    type_: Option<ResolvedType>,
    value: AstNode,
    eval_state: EvalState,
}

impl Symbol {
    pub fn new(
        name: &str,
        kind: &ConstDeclKind,
        type_: Option<ResolvedType>,
        value: &AstNode,
    ) -> Self {
        Self {
            name: name.to_string(),
            kind: *kind,
            type_: type_,
            value: value.clone(),
            eval_state: EvalState::Unevaluated,
        }
    }
}

pub enum EvalState {
    Unevaluated,
    Evaluating,
    Evaluated,
}
