// PIPELINE:
// 1. symbol collection
// 2. constant evaluation
// 3. default type resolution
// 4. type inference (update AST)
// 5. constant inlining

use crate::{
    parser::ast::{
        AstNode, BinaryOpKind, ConstDeclKind, Number, ResolvedNumber, ResolvedType, UnaryOpKind,
    },
    semantic::{
        error::{ErrorList, FatalError, RecoverableError, SemanticError},
        symtab::{EvalState, Symbol, SymbolTable},
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
                SemanticError::Recoverable(recoverable) => errors.push(recoverable),
                SemanticError::Fatal(fatal) => panic!("{}", fatal),
            }
        }

        // 2. evaluate constants
        if let Err(error) = self.evaluate_consts() {
            match error {
                SemanticError::Recoverable(recoverable) => errors.push(recoverable),
                SemanticError::Fatal(fatal) => panic!("{}", fatal),
            }
        }

        // TODO:
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
                if self.symtab.contains(name) {
                    return Err(SemanticError::Recoverable(
                        RecoverableError::DuplicateSymbol(name.to_owned()),
                    ));
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

    fn evaluate_consts(&mut self) -> Result<(), SemanticError> {
        // collect const names
        let const_names: Vec<String> = self.symtab.symbols(ConstDeclKind::Const);

        // evaluate by name
        for name in const_names {
            self.evaluate_symbol(&name)?;
        }

        Ok(())
    }

    fn evaluate_symbol(&mut self, name: &str) -> Result<(), SemanticError> {
        // get current state
        let (state, kind, value, sym_type) = {
            let symbol = self.symtab.get(name).ok_or_else(|| {
                SemanticError::Recoverable(RecoverableError::UndefinedSymbol(name.to_string()))
            })?;

            (
                symbol.eval_state,
                symbol.kind,
                symbol.value.clone(),
                symbol.type_,
            )
        };

        match state {
            EvalState::Evaluated => return Ok(()),
            EvalState::Evaluating => {
                return Err(SemanticError::Fatal(
                    FatalError::CircularComptimeDependency {
                        sym_name: name.to_string(),
                        sym_kind: kind,
                    },
                ));
            }
            EvalState::Unevaluated => {
                // mark as evaluating
                if let Some(symbol) = self.symtab.get_mut(name) {
                    symbol.eval_state = EvalState::Evaluating;
                }
            }
        }

        // evaluate expression
        let evaluated_value = self.evaluate_expr(&value, sym_type)?;

        // update symbol with result
        if let Some(symbol) = self.symtab.get_mut(name) {
            symbol.value = evaluated_value;
            symbol.eval_state = EvalState::Evaluated;
        }

        Ok(())
    }

    fn find_common_type(&self, type1: ResolvedType, type2: ResolvedType) -> Option<ResolvedType> {
        if type1 == type2 {
            return Some(type1);
        }

        if self.can_promote_type(type1, type2) {
            return Some(type2);
        }

        if self.can_promote_type(type2, type1) {
            return Some(type1);
        }

        None
    }

    fn can_promote_type(&self, from_type: ResolvedType, to_type: ResolvedType) -> bool {
        use ResolvedType::*;

        // integer to float promotion (always allowed)
        match to_type {
            F32 | F64 => match from_type {
                U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => return true,
                _ => {}
            },
            _ => {}
        }

        // signed integer to wider signed integer
        (match from_type {
            I8 => matches!(to_type, I16 | I32 | I64),
            I16 => matches!(to_type, I32 | I64),
            I32 => matches!(to_type, I64),
            _ => false,
        } || match from_type {
            // unsigned integer to wider unsigned integer
            U8 => matches!(to_type, U16 | U32 | U64),
            U16 => matches!(to_type, U32 | U64),
            U32 => matches!(to_type, U64),
            _ => false,
        } || match from_type {
            // float to wider float
            F32 => matches!(to_type, F64),
            _ => false,
        })
    }

    fn infer_type_from_resolved_number(&self, num: ResolvedNumber) -> ResolvedType {
        match num {
            ResolvedNumber::U8(_) => ResolvedType::U8,
            ResolvedNumber::U16(_) => ResolvedType::U16,
            ResolvedNumber::U32(_) => ResolvedType::U32,
            ResolvedNumber::U64(_) => ResolvedType::U64,
            ResolvedNumber::I8(_) => ResolvedType::I8,
            ResolvedNumber::I16(_) => ResolvedType::I16,
            ResolvedNumber::I32(_) => ResolvedType::I32,
            ResolvedNumber::I64(_) => ResolvedType::I64,
            ResolvedNumber::F32(_) => ResolvedType::F32,
            ResolvedNumber::F64(_) => ResolvedType::F64,
        }
    }

    fn evaluate_expr(
        &mut self,
        expr: &AstNode,
        context_type: Option<ResolvedType>,
    ) -> Result<AstNode, SemanticError> {
        match expr {
            AstNode::Ident(name) => {
                // check if symbol needs type inference from context
                let (needs_conversion, current_value) = {
                    let symbol = self.symtab.get(name).ok_or_else(|| {
                        SemanticError::Recoverable(RecoverableError::UndefinedSymbol(
                            name.to_string(),
                        ))
                    })?;

                    let needs_conversion = symbol.type_.is_none() && context_type.is_some();
                    let current_value = if symbol.eval_state == EvalState::Evaluated {
                        Some(symbol.value.clone())
                    } else {
                        None
                    };

                    (needs_conversion, current_value)
                };

                // if no type and context provided, infer type from context
                if needs_conversion {
                    // first, set the type
                    if let Some(symbol) = self.symtab.get_mut(name) {
                        symbol.type_ = context_type;
                    }

                    // if already evaluated, convert the value
                    if let Some(value) = current_value {
                        if let AstNode::Num(Number::Resolved(num)) = value {
                            // do the conversion
                            let converted =
                                self.convert_number_to_type(num, context_type.unwrap())?;

                            // update value
                            if let Some(symbol) = self.symtab.get_mut(name) {
                                symbol.value = AstNode::Num(Number::Resolved(converted));
                            }
                        }
                    }
                }

                // evaluate symbol (with inferred type)
                self.evaluate_symbol(name)?;

                // return evaluated value
                let symbol = self.symtab.get(name).unwrap();
                Ok(symbol.value.clone())
            }

            AstNode::Num(Number::Unresolved(s)) => {
                // use context type if provided, otherwise use default
                let resolved = if let Some(ctx_type) = context_type {
                    self.resolve_number_to_type(s, ctx_type)?
                } else {
                    self.resolve_number_literal(s)?
                };
                Ok(AstNode::Num(Number::Resolved(resolved)))
            }

            AstNode::Num(Number::Resolved(num)) => {
                // if context type provided and different, convert
                if let Some(ctx_type) = context_type {
                    let current_type = self.infer_type_from_resolved_number(*num);
                    if ctx_type != current_type {
                        let converted = self.convert_number_to_type(*num, ctx_type)?;
                        return Ok(AstNode::Num(Number::Resolved(converted)));
                    }
                }
                Ok(expr.clone())
            }

            AstNode::Bool(_) => Ok(expr.clone()),

            AstNode::BinaryOp { op, left, right } => {
                // first evaluate without context to get natural types
                let left_val_untyped = self.evaluate_expr(left, None)?;
                let right_val_untyped = self.evaluate_expr(right, None)?;

                // get the types
                let left_type = self.infer_type_from_node(&left_val_untyped)?;
                let right_type = self.infer_type_from_node(&right_val_untyped)?;

                // find common type (with promotion)
                let mut operation_type =
                    self.find_common_type(left_type, right_type)
                        .ok_or_else(|| {
                            SemanticError::Recoverable(RecoverableError::TypeMismatch {
                                left: format!("{:?}", left_type),
                                right: format!("{:?}", right_type),
                            })
                        })?;

                // if context type provided and wider, use that
                if let Some(ctx_type) = context_type {
                    if let Some(common) = self.find_common_type(operation_type, ctx_type) {
                        operation_type = common;
                    }
                }

                // re-evaluate with common type
                let left_val = self.evaluate_expr(left, Some(operation_type))?;
                let right_val = self.evaluate_expr(right, Some(operation_type))?;

                // perform the operation
                self.apply_binary_op(*op, left_val, right_val)
            }

            AstNode::UnaryOp { op, operand } => {
                let operand_val = self.evaluate_expr(operand, context_type)?;
                self.apply_unary_op(*op, operand_val)
            }

            _ => {
                todo!("Handle other expression types")
            }
        }
    }

    fn infer_type_from_node(&self, node: &AstNode) -> Result<ResolvedType, SemanticError> {
        match node {
            AstNode::Bool(_) => Ok(ResolvedType::Bool),
            AstNode::Num(Number::Resolved(num)) => Ok(self.infer_type_from_resolved_number(*num)),
            _ => Err(SemanticError::Fatal(FatalError::CannotInferType)),
        }
    }

    fn resolve_number_to_type(
        &self,
        s: &str,
        target_type: ResolvedType,
    ) -> Result<ResolvedNumber, SemanticError> {
        use ResolvedType::*;

        if s.contains('.') {
            // float
            let val: f64 = s
                .parse()
                .map_err(|_| SemanticError::Recoverable(RecoverableError::InvalidNumberLiteral))?;

            match target_type {
                F32 => Ok(ResolvedNumber::F32(val as f32)),
                F64 => Ok(ResolvedNumber::F64(val)),
                _ => Err(SemanticError::Recoverable(
                    RecoverableError::InvalidTypeConversion {
                        from: "float literal".to_string(),
                        to: format!("{:?}", target_type),
                    },
                )),
            }
        } else {
            // integer
            let val: i64 = s
                .parse()
                .map_err(|_| SemanticError::Recoverable(RecoverableError::InvalidNumberLiteral))?;

            match target_type {
                I8 => Ok(ResolvedNumber::I8(val as i8)),
                I16 => Ok(ResolvedNumber::I16(val as i16)),
                I32 => Ok(ResolvedNumber::I32(val as i32)),
                I64 => Ok(ResolvedNumber::I64(val)),
                U8 => Ok(ResolvedNumber::U8(val as u8)),
                U16 => Ok(ResolvedNumber::U16(val as u16)),
                U32 => Ok(ResolvedNumber::U32(val as u32)),
                U64 => Ok(ResolvedNumber::U64(val as u64)),
                F32 => Ok(ResolvedNumber::F32(val as f32)),
                F64 => Ok(ResolvedNumber::F64(val as f64)),
                _ => Err(SemanticError::Recoverable(
                    RecoverableError::InvalidTypeConversion {
                        from: "integer literal".to_string(),
                        to: format!("{:?}", target_type),
                    },
                )),
            }
        }
    }

    fn convert_number_to_type(
        &self,
        num: ResolvedNumber,
        target_type: ResolvedType,
    ) -> Result<ResolvedNumber, SemanticError> {
        use ResolvedNumber::*;
        use ResolvedType as RT;

        // extract numeric value and convert
        match num {
            I64(v) => match target_type {
                RT::I8 => Ok(I8(v as i8)),
                RT::I16 => Ok(I16(v as i16)),
                RT::I32 => Ok(I32(v as i32)),
                RT::I64 => Ok(I64(v)),
                RT::F32 => Ok(F32(v as f32)),
                RT::F64 => Ok(F64(v as f64)),
                _ => Err(SemanticError::Recoverable(
                    RecoverableError::InvalidTypeConversion {
                        from: format!("{:?}", num),
                        to: format!("{:?}", target_type),
                    },
                )),
            },
            F64(v) => match target_type {
                RT::F32 => Ok(F32(v as f32)),
                RT::F64 => Ok(F64(v)),
                _ => Err(SemanticError::Recoverable(
                    RecoverableError::InvalidTypeConversion {
                        from: format!("{:?}", num),
                        to: format!("{:?}", target_type),
                    },
                )),
            },
            _ => Ok(num),
        }
    }

    fn apply_unary_op(&self, op: UnaryOpKind, operand: AstNode) -> Result<AstNode, SemanticError> {
        match op {
            UnaryOpKind::ArithmeticNeg => {
                // extract resolved number
                let resolved = match operand {
                    AstNode::Num(Number::Resolved(num)) => num,
                    AstNode::Num(Number::Unresolved(s)) => self.resolve_number_literal(&s)?,
                    _ => {
                        return Err(SemanticError::Recoverable(
                            RecoverableError::InvalidOperandType {
                                expected: "numeric type".to_string(),
                                found: format!("{:?}", operand),
                            },
                        ));
                    }
                };

                // perform negation
                let negated = match resolved {
                    ResolvedNumber::I8(v) => ResolvedNumber::I8(-v),
                    ResolvedNumber::I16(v) => ResolvedNumber::I16(-v),
                    ResolvedNumber::I32(v) => ResolvedNumber::I32(-v),
                    ResolvedNumber::I64(v) => ResolvedNumber::I64(-v),
                    ResolvedNumber::F32(v) => ResolvedNumber::F32(-v),
                    ResolvedNumber::F64(v) => ResolvedNumber::F64(-v),

                    // cannot negate unsigned types
                    ResolvedNumber::U8(_)
                    | ResolvedNumber::U16(_)
                    | ResolvedNumber::U32(_)
                    | ResolvedNumber::U64(_) => {
                        return Err(SemanticError::Recoverable(
                            RecoverableError::CannotNegateUnsignedType,
                        ));
                    }
                };

                Ok(AstNode::Num(Number::Resolved(negated)))
            }

            UnaryOpKind::LogicalNot => match operand {
                AstNode::Bool(v) => Ok(AstNode::Bool(!v)),
                _ => Err(SemanticError::Recoverable(
                    RecoverableError::InvalidOperandType {
                        expected: "bool".to_string(),
                        found: format!("{:?}", operand),
                    },
                )),
            },
        }
    }

    fn apply_binary_op(
        &self,
        op: BinaryOpKind,
        left: AstNode,
        right: AstNode,
    ) -> Result<AstNode, SemanticError> {
        match op {
            // arithmetic operations
            BinaryOpKind::Add | BinaryOpKind::Sub | BinaryOpKind::Mul | BinaryOpKind::Div => {
                self.apply_arithmetic_op(op, left, right)
            }

            // comparative operations
            BinaryOpKind::Less
            | BinaryOpKind::Greater
            | BinaryOpKind::LessEqual
            | BinaryOpKind::GreaterEqual
            | BinaryOpKind::Equal
            | BinaryOpKind::Different => self.apply_comparison_op(op, left, right),

            // logical operations
            BinaryOpKind::And | BinaryOpKind::Or => self.apply_logical_op(op, left, right),
        }
    }

    fn apply_arithmetic_op(
        &self,
        op: BinaryOpKind,
        left: AstNode,
        right: AstNode,
    ) -> Result<AstNode, SemanticError> {
        // extract numeric values
        let (left_num, right_num) = self.extract_numbers(left, right)?;

        // perform operation based on numeric type
        let result = match (left_num, right_num) {
            // signed integer operations
            (ResolvedNumber::I8(l), ResolvedNumber::I8(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::I8)?
            }
            (ResolvedNumber::I16(l), ResolvedNumber::I16(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::I16)?
            }
            (ResolvedNumber::I32(l), ResolvedNumber::I32(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::I32)?
            }
            (ResolvedNumber::I64(l), ResolvedNumber::I64(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::I64)?
            }

            // unsigned integer operations
            (ResolvedNumber::U8(l), ResolvedNumber::U8(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::U8)?
            }
            (ResolvedNumber::U16(l), ResolvedNumber::U16(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::U16)?
            }
            (ResolvedNumber::U32(l), ResolvedNumber::U32(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::U32)?
            }
            (ResolvedNumber::U64(l), ResolvedNumber::U64(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::U64)?
            }

            // floating point operations
            (ResolvedNumber::F32(l), ResolvedNumber::F32(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::F32)?
            }
            (ResolvedNumber::F64(l), ResolvedNumber::F64(r)) => {
                self.compute_arithmetic(op, l, r).map(ResolvedNumber::F64)?
            }

            // type mismatch
            _ => {
                return Err(SemanticError::Recoverable(RecoverableError::TypeMismatch {
                    left: format!("{:?}", left_num),
                    right: format!("{:?}", right_num),
                }));
            }
        };

        Ok(AstNode::Num(Number::Resolved(result)))
    }

    fn apply_comparison_op(
        &self,
        op: BinaryOpKind,
        left: AstNode,
        right: AstNode,
    ) -> Result<AstNode, SemanticError> {
        // handle boolean equality
        if matches!(op, BinaryOpKind::Equal | BinaryOpKind::Different) {
            if let (AstNode::Bool(l), AstNode::Bool(r)) = (&left, &right) {
                let result = match op {
                    BinaryOpKind::Equal => l == r,
                    BinaryOpKind::Different => l != r,
                    _ => unreachable!(),
                };
                return Ok(AstNode::Bool(result));
            }
        }

        // extract numeric values
        let (left_num, right_num) = self.extract_numbers(left, right)?;

        // perform comparison based on numeric types
        let result = match (left_num, right_num) {
            // signed integer comparisons
            (ResolvedNumber::I8(l), ResolvedNumber::I8(r)) => self.compute_comparison(op, l, r),
            (ResolvedNumber::I16(l), ResolvedNumber::I16(r)) => self.compute_comparison(op, l, r),
            (ResolvedNumber::I32(l), ResolvedNumber::I32(r)) => self.compute_comparison(op, l, r),
            (ResolvedNumber::I64(l), ResolvedNumber::I64(r)) => self.compute_comparison(op, l, r),

            // unsigned integer comparisons
            (ResolvedNumber::U8(l), ResolvedNumber::U8(r)) => self.compute_comparison(op, l, r),
            (ResolvedNumber::U16(l), ResolvedNumber::U16(r)) => self.compute_comparison(op, l, r),
            (ResolvedNumber::U32(l), ResolvedNumber::U32(r)) => self.compute_comparison(op, l, r),
            (ResolvedNumber::U64(l), ResolvedNumber::U64(r)) => self.compute_comparison(op, l, r),

            // float comparisons
            (ResolvedNumber::F32(l), ResolvedNumber::F32(r)) => self.compute_comparison(op, l, r),
            (ResolvedNumber::F64(l), ResolvedNumber::F64(r)) => self.compute_comparison(op, l, r),

            // type mismatch
            _ => {
                return Err(SemanticError::Recoverable(RecoverableError::TypeMismatch {
                    left: format!("{:?}", left_num),
                    right: format!("{:?}", right_num),
                }));
            }
        };

        Ok(AstNode::Bool(result))
    }

    fn apply_logical_op(
        &self,
        op: BinaryOpKind,
        left: AstNode,
        right: AstNode,
    ) -> Result<AstNode, SemanticError> {
        let left_bool = match left {
            AstNode::Bool(b) => b,
            _ => {
                return Err(SemanticError::Recoverable(
                    RecoverableError::InvalidOperandType {
                        expected: "bool".to_string(),
                        found: format!("{:?}", left),
                    },
                ));
            }
        };

        let right_bool = match right {
            AstNode::Bool(b) => b,
            _ => {
                return Err(SemanticError::Recoverable(
                    RecoverableError::InvalidOperandType {
                        expected: "bool".to_string(),
                        found: format!("{:?}", right),
                    },
                ));
            }
        };

        let result = match op {
            BinaryOpKind::And => left_bool && right_bool,
            BinaryOpKind::Or => left_bool || right_bool,
            _ => unreachable!(),
        };

        Ok(AstNode::Bool(result))
    }

    /// Extract resolved numbers from AstNodes.
    fn extract_numbers(
        &self,
        left: AstNode,
        right: AstNode,
    ) -> Result<(ResolvedNumber, ResolvedNumber), SemanticError> {
        let left_num = match left {
            AstNode::Num(Number::Resolved(num)) => num,
            AstNode::Num(Number::Unresolved(s)) => {
                // try to resolve the number
                self.resolve_number_literal(&s)?
            }
            _ => {
                return Err(SemanticError::Recoverable(
                    RecoverableError::InvalidOperandType {
                        expected: "numeric type".to_string(),
                        found: format!("{:?}", left),
                    },
                ));
            }
        };

        let right_num = match right {
            AstNode::Num(Number::Resolved(num)) => num,
            AstNode::Num(Number::Unresolved(s)) => self.resolve_number_literal(&s)?,
            _ => {
                return Err(SemanticError::Recoverable(
                    RecoverableError::InvalidOperandType {
                        expected: "numeric type".to_string(),
                        found: format!("{:?}", right),
                    },
                ));
            }
        };

        Ok((left_num, right_num))
    }

    fn resolve_number_literal(&self, s: &str) -> Result<ResolvedNumber, SemanticError> {
        if s.contains('.') {
            // float literal (default to f64)
            let val: f64 = s
                .parse()
                .map_err(|_| SemanticError::Recoverable(RecoverableError::InvalidNumberLiteral))?;
            Ok(ResolvedNumber::F64(val))
        } else {
            // integer literal (default to i64)
            let val: i64 = s
                .parse()
                .map_err(|_| SemanticError::Recoverable(RecoverableError::InvalidNumberLiteral))?;
            Ok(ResolvedNumber::I64(val))
        }
    }

    fn compute_arithmetic<T>(&self, op: BinaryOpKind, left: T, right: T) -> Result<T, SemanticError>
    where
        T: std::ops::Add<Output = T>
            + std::ops::Sub<Output = T>
            + std::ops::Mul<Output = T>
            + std::ops::Div<Output = T>
            + PartialEq
            + Default
            + Copy,
    {
        let result = match op {
            BinaryOpKind::Add => left + right,
            BinaryOpKind::Sub => left - right,
            BinaryOpKind::Mul => left * right,
            BinaryOpKind::Div => {
                if right == T::default() {
                    return Err(SemanticError::Fatal(FatalError::DivisionByZero));
                }
                left / right
            }
            _ => unreachable!(),
        };
        Ok(result)
    }

    fn compute_comparison<T>(&self, op: BinaryOpKind, left: T, right: T) -> bool
    where
        T: PartialOrd + PartialEq,
    {
        match op {
            BinaryOpKind::Less => left < right,
            BinaryOpKind::Greater => left > right,
            BinaryOpKind::LessEqual => left <= right,
            BinaryOpKind::GreaterEqual => left >= right,
            BinaryOpKind::Equal => left == right,
            BinaryOpKind::Different => left != right,
            _ => unreachable!(),
        }
    }
}
