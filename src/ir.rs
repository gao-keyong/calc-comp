use core::fmt;
use std::{cell::RefCell, fmt::write, path::Prefix, process::id};

use id_arena::Arena;

use crate::ast::*;

type ValueId = id_arena::Id<Value>;

#[derive(Debug, Clone)]
pub enum IrType {
    Void,
    Int,
}

pub trait ValueTrait {
    fn name(&self) -> String;
    fn set_name(&mut self, name: String);
    fn ty(&self) -> IrType;
    fn set_ty(&mut self, ty: IrType);
}

pub trait IrGenerator {
    fn to_ir(&self, context: &mut Context);
}

impl IrGenerator for TransUnit {
    fn to_ir(&self, context: &mut Context) {
        self.block.to_ir(context);
    }
}

impl IrGenerator for Block {
    fn to_ir(&self, context: &mut Context) {
        for stmt in &self.stmts {
            stmt.to_ir(context);
        }
    }
}

impl IrGenerator for Stmt {
    fn to_ir(&self, context: &mut Context) {
        match self {
            Stmt::ExprStmt(expr) => {
                let value_id = expr.to_ir(context);
                let mem_id = context.global_variables["mem"];
                let store_inst = Value::Instruction(InstructionValue::StoreInst(StoreInst {
                    source: value_id,
                    destination: mem_id,
                }));
                let store_inst_id = context.values.borrow_mut().alloc(store_inst);
                context.instructions.push(store_inst_id);
            }
            Stmt::PrintStmt(expr) => {
                let value_id = expr.to_ir(context);
                let print_inst = Value::Instruction(InstructionValue::PrintIntInst(PrintIntInst {
                    value: value_id,
                }));
                let print_inst_id = context.values.borrow_mut().alloc(print_inst);
                context.instructions.push(print_inst_id);
            }
        }
    }
}

impl Expr {
    fn to_ir(&self, context: &mut Context) -> ValueId {
        match self {
            Expr::Primary(primary_expr) => primary_expr.as_ref().to_ir(context),
            Expr::Infix(infix_expr) => infix_expr.as_ref().to_ir(context),
            Expr::Prefix(prefix_expr) => prefix_expr.as_ref().to_ir(context),
        }
    }
}

impl PrimaryExpr {
    fn to_ir(&self, context: &mut Context) -> ValueId {
        match self {
            // let index = context.values.
            Self::Mem => {
                let load_inst = Value::Instruction(InstructionValue::LoadInst(LoadInst {
                    name: context.generate_local_name(),
                    ty: IrType::Int,
                    source: context.global_variables["mem"],
                }));
                let load_inst_id = context.values.borrow_mut().alloc(load_inst);
                context.instructions.push(load_inst_id);
                load_inst_id
            }
            Self::Int(val) => {
                let value = Value::Constant(ConstantValue::Int(*val));
                let id = context.values.borrow_mut().alloc(value);
                id
            }
            Self::Expr(expr) => expr.as_ref().to_ir(context),
        }
    }
}

impl InfixExpr {
    fn to_ir(&self, context: &mut Context) -> ValueId {
        let lhs_value_id = self.lhs.as_ref().to_ir(context);
        let rhs_value_id = self.rhs.as_ref().to_ir(context);
        let operator = match self.op {
            InfixOp::Plus => BinaryOp::Plus,
            InfixOp::Minus => BinaryOp::Minus,
            InfixOp::Multiply => BinaryOp::Multiply,
            InfixOp::Divide => BinaryOp::Divide,
        };
        let value = Value::Instruction(InstructionValue::BinaryOperator(BinaryOperator {
            name: context.generate_local_name(),
            ty: IrType::Int,
            operator: operator,
            left_operand: lhs_value_id,
            right_operand: rhs_value_id,
        }));
        let id = context.values.borrow_mut().alloc(value);
        context.instructions.push(id);
        id
    }
}

impl PrefixExpr {
    fn to_ir(&self, context: &mut Context) -> ValueId {
        let expr_value_id = self.expr.as_ref().to_ir(context);
        match self.op {
            PrefixOp::Plus => expr_value_id,
            PrefixOp::Minus => {
                let zero_value = Value::Constant(ConstantValue::Int(0));
                let zero_value_id = context.values.borrow_mut().alloc(zero_value);
                let value = Value::Instruction(InstructionValue::BinaryOperator(BinaryOperator {
                    name: context.generate_local_name(),
                    ty: IrType::Int,
                    operator: BinaryOp::Minus,
                    left_operand: zero_value_id,
                    right_operand: expr_value_id,
                }));
                let id = context.values.borrow_mut().alloc(value);
                context.instructions.push(id);
                id
            }
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Global(GlobalValue),
    Instruction(InstructionValue),
    Constant(ConstantValue),
}

#[derive(Debug)]
pub struct GlobalValue {
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug)]
pub enum InstructionValue {
    BinaryOperator(BinaryOperator),
    LoadInst(LoadInst),
    StoreInst(StoreInst),
    AllocaInst(AllocaInst),
    PrintIntInst(PrintIntInst),
}

#[derive(Debug)]
pub struct BinaryOperator {
    pub name: String,
    pub ty: IrType,
    pub operator: BinaryOp,
    pub left_operand: ValueId,
    pub right_operand: ValueId,
}

#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub struct LoadInst {
    pub name: String,
    pub ty: IrType,
    pub source: ValueId,
}

#[derive(Debug)]
pub struct StoreInst {
    // pub name: String,
    // pub ty: IrType,
    pub source: ValueId,
    pub destination: ValueId,
}

#[derive(Debug)]
pub struct AllocaInst {
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug)]
pub enum ConstantValue {
    Int(i64),
}

#[derive(Debug)]
pub struct PrintIntInst {
    // pub name: String,
    // pub ty: IrType,
    pub value: ValueId,
}

// 为了让这些 Value 都实现 ValueTrait，我写了一些宏，这样可以减少一些重复。然后得到这样的代码：

macro_rules! impl_value_trait {
    ($type_name:ident) => {
        impl ValueTrait for $type_name {
            fn name(&self) -> String {
                self.name.clone()
            }
            fn set_name(&mut self, name: String) {
                self.name = name;
            }
            fn ty(&self) -> IrType {
                self.ty.clone()
            }
            fn set_ty(&mut self, ty: IrType) {
                self.ty = ty;
            }
        }
    };
}

impl_value_trait!(GlobalValue);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global(gv) => write!(f,"@{}", gv.name),
            Self::Instruction(inst) => {
                match inst{
                    InstructionValue::BinaryOperator(bo) => write!(f, "%{}", bo.name),
                    InstructionValue::LoadInst(li) => write!(f, "%{}", li.name),
                    _=>unreachable!()
                }
            },
            Self::Constant(cv) => {
                if let ConstantValue::Int(v) = cv{
                    write!(f, "{}", v)
                }else {
                    unreachable!()
                }
            }
        }
    }
}

pub struct Context {
    pub values: RefCell<Arena<Value>>,
    pub instructions: Vec<ValueId>,
    pub next_id: usize,
    pub global_variables: std::collections::HashMap<String, ValueId>,
}

impl Context {
    pub fn new() -> Self {
        let mut context = Self {
            values: RefCell::new(Arena::new()),
            instructions: vec![],
            next_id: 1,
            global_variables: std::collections::HashMap::new(),
        };
        context.create_global_variable("mem".to_string());
        context
    }
    pub fn generate_local_name(&mut self) -> String {
        let name = self.next_id.to_string();
        self.next_id += 1;
        name
    }
    fn create_global_variable(&mut self, symbol: String) -> ValueId {
        let value = Value::Global(GlobalValue {
            name: symbol.clone(),
            ty: IrType::Int,
        });
        let id = self.values.borrow_mut().alloc(value);
        self.global_variables.insert(symbol, id);
        id
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // writeln!(f, "Values: {:?}", self.values.borrow())?;
        // writeln!(f, "Instructions: {:?}", self.instructions)
        let values_borrow = self.values.borrow();
        for inst in &self.instructions {
            if let Value::Instruction(inst_value) = values_borrow.get(*inst).unwrap() {
                match inst_value {
                    InstructionValue::StoreInst(inst) => {
                        let source = values_borrow.get(inst.source).unwrap();
                        let destination = values_borrow.get(inst.destination).unwrap();
                        writeln!(
                            f,
                            "StoreInst {{ source: {:?}, destination: {:?} }}",
                            source, destination
                        )?;
                    }
                    InstructionValue::LoadInst(inst) => {
                        let source = values_borrow.get(inst.source).unwrap();
                        writeln!(
                            f,
                            "LoadInst {{ name: {:?}, source: {:?}}}",
                            inst.name, source
                        )?;
                    }
                    InstructionValue::PrintIntInst(inst) => {
                        let value = values_borrow.get(inst.value).unwrap();
                        writeln!(f, "PrintIntInst {{ value: {:?} }}", value)?;
                    }
                    InstructionValue::BinaryOperator(inst) => {
                        let left_operand = values_borrow.get(inst.left_operand).unwrap();
                        let right_operand = values_borrow.get(inst.right_operand).unwrap();
                        writeln!(
                            f, 
                            "BinaryOperator {{ name: {:?}, left_operand: {:?}, op: {:?}, right_operand: {:?}}}",
                            inst.name, left_operand, inst.operator, right_operand)?;
                    }
                    _ => unreachable!(),
                }
            } else {
                unreachable!()
            }
        }
        Ok(())
    }
}
