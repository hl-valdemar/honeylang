// PIPELINE:
// 1. symbol collection
// 2. constant evaluation
// 3. default type resolution
// 4. type inference (update AST)
// 5. constant inlining

use crate::{
    parser::ast::AstNode,
    semantic::{
        error::{ErrorList, FatalError, RecoverableError, SemanticError}, symtab::{Symbol, SymbolTable},
    },
};

mod error;
mod symtab;

pub fn analyze(program: &AstNode) -> (AstNode, SymbolTable, ErrorList) {
    let mut ctx = SemanticContext::new(program);
    let (ast, errors) = ctx.analyze();
    (ast, ctx.symtab, errors)
}

pub struct SemanticContext {
    program: AstNode,
    symtab: SymbolTable,
}

impl SemanticContext {
    pub fn new(program: &AstNode) -> Self {
        Self {
            program: program.clone(),
            symtab: SymbolTable::new(),
        }
    }

    pub fn analyze(&mut self) -> (AstNode, ErrorList) {
        let mut errors = ErrorList::new();

        // 1. collect symbols
        if let Err(error) = self.collect_symbols() {
            match error {
                SemanticError::Recoverable(_) => errors.push(error),
                SemanticError::Fatal(error) => match error {
                    FatalError::ExpectedProgram { found } => {
                        panic!("expected program for root node, found '{:?}'", found)
                    }
                },
            }
        }

        // TODO:
        // 2. constant evaluation
        // 3. default type resolution
        // 4. type inference (update AST)
        // 5. constant inlining

        (self.program.clone(), errors)
    }

    fn collect_symbols(&mut self) -> Result<(), SemanticError> {
        if let AstNode::Program { declarations } = &self.program {
            for decl in declarations {
                let AstNode::ConstDecl {
                    name,
                    kind,
                    type_,
                    value,
                } = decl
                else {
                    return Err(SemanticError::Recoverable(
                        RecoverableError::ExpectedConstDecl {
                            found: decl.clone(),
                        },
                    ));
                };

                // lookup name
                if self.symtab.has_symbol(name) {
                    return Err(SemanticError::Recoverable(RecoverableError::DuplicateName(
                        name.to_owned(),
                    )));
                }

                // resolve the type
                let resolved_type = type_.as_ref().map(|t| t.resolve());

                // setup entry
                let sym = Symbol::new(name, kind, resolved_type, value);

                self.symtab.register(name, sym);
            }
        } else {
            return Err(SemanticError::Fatal(FatalError::ExpectedProgram {
                found: self.program.clone(),
            }));
        }

        Ok(())
    }
}
