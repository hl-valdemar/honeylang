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

    pub fn contains(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn register(&mut self, name: &str, symbol: Symbol) -> Option<Symbol> {
        self.symbols.insert(name.to_string(), symbol)
    }

    pub fn symbols(&self, kind: ConstDeclKind) -> Vec<String> {
        self.symbols
            .values()
            .filter(|sym| sym.kind == kind)
            .map(|sym| sym.name.clone())
            .collect()
    }

    pub fn names(&self) -> Vec<String> {
        self.symbols.keys().cloned().collect()
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.symbols.get_mut(name)
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }
}

#[derive(Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: ConstDeclKind,
    pub type_: Option<ResolvedType>,
    pub value: AstNode,
    pub eval_state: EvalState,
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

#[derive(Clone, Copy, PartialEq)]
pub enum EvalState {
    Unevaluated,
    Evaluating,
    Evaluated,
}
