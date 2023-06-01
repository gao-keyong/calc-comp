use crate::ir::*;

pub trait LlvmEmitter {
    fn emit_ir(&self) -> String;
}

impl LlvmEmitter for Context {
    fn emit_ir(&self) -> String {
        let mut llvm_ir = String::new();
        llvm_ir.push_str(r#"
@.str = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1

define dso_local void @print(i64 noundef %0) #0{
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([6 x i8], [6 x i8]* @.str, i64 0, i64 0), i64 noundef %3)
  ret void
}

declare i32 @printf(i8*, ...)
"#);

        // Emit global variable declarations
        for (name, id) in &self.global_variables {
            llvm_ir += format!("@{} = global i64 0\n", name).as_str();
        }
        llvm_ir += "define void @main() {\n";
        for inst_id in &self.instructions {
            llvm_ir += "  ";
            let values_borrow = self.values.borrow();
            if let Value::Instruction(inst_value) = values_borrow.get(*inst_id).unwrap() {
                match inst_value {
                    InstructionValue::StoreInst(inst) => {
                        llvm_ir += format!(
                            "store i64 {}, i64* {}",
                            values_borrow.get(inst.source).unwrap(),
                            values_borrow.get(inst.destination).unwrap()
                        )
                        .as_str();
                    }
                    InstructionValue::LoadInst(inst) => {
                        llvm_ir += format!(
                            "%{} = load i64, i64* {}",
                            inst.name,
                            values_borrow.get(inst.source).unwrap()
                        )
                        .as_str();
                    }
                    InstructionValue::BinaryOperator(inst) => {
                        let op_ir = match inst.operator {
                            BinaryOp::Plus => "add",
                            BinaryOp::Minus => "sub",
                            BinaryOp::Multiply => "mul",
                            BinaryOp::Divide => "sdiv",
                        };

                        llvm_ir += format!(
                            "%{} = {} i64 {}, {}",
                            inst.name,
                            op_ir,
                            values_borrow.get(inst.left_operand).unwrap(),
                            values_borrow.get(inst.right_operand).unwrap()
                        )
                        .as_str();
                    }
                    InstructionValue::PrintIntInst(inst) => {
                        llvm_ir += format!(
                            "call void @print(i64 {})",
                            values_borrow.get(inst.value).unwrap()
                        )
                        .as_str();
                    }
                    _ => unreachable!(),
                }
            }
            llvm_ir += "\n";
        }
        llvm_ir += "  ret void\n}";
        llvm_ir
    }
}
