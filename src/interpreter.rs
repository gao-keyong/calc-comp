use crate::ast::{self, Expr, InfixExpr, InfixOp, PrefixExpr, PrefixOp, PrimaryExpr, Stmt};

struct Env {
    mem: i64,
}

impl Env {
    fn new() -> Self {
        Env { mem: 0 }
    }
    fn get_mem(&self) -> i64 {
        self.mem
    }
    fn set_mem(&mut self, val: i64) {
        self.mem = val;
    }
}

pub fn interpret(tu: &ast::TransUnit) {
    let mut env = Env::new();
    let stmts = &tu.block.stmts;
    for stmt in stmts {
        match stmt {
            Stmt::ExprStmt(expr) => {
                let ret = eval(expr, &mut env);
                env.set_mem(ret);
            }
            Stmt::PrintStmt(expr) => {
                let ret = eval(expr, &mut env);
                println!("{}", ret);
            }
        }
    }
}

fn eval(expr: &Expr, env: &mut Env) -> i64 {
    match expr {
        Expr::Primary(primary_expr) => eval_primary(primary_expr, env),
        Expr::Prefix(prefix_expr) => eval_prefix(prefix_expr, env),
        Expr::Infix(infix_expr) => eval_infix(infix_expr, env),
    }
}

fn eval_prefix(expr: &PrefixExpr, env: &mut Env) -> i64 {
    match expr.op {
        PrefixOp::Plus => eval(&expr.expr, env),
        PrefixOp::Minus => eval(&expr.expr, env),
    }
}

fn eval_primary(expr: &PrimaryExpr, env: &mut Env) -> i64 {
    match expr {
        PrimaryExpr::Mem => env.get_mem(),
        PrimaryExpr::Int(val) => *val,
        PrimaryExpr::Expr(e) => eval(e, env),
    }
}

fn eval_infix(expr: &InfixExpr, env: &mut Env) -> i64 {
    let lhs = eval(&expr.lhs, env);
    let rhs = eval(&expr.rhs, env);
    match expr.op {
        InfixOp::Plus => lhs + rhs,
        InfixOp::Minus => lhs - rhs,
        InfixOp::Multiply => lhs * rhs,
        InfixOp::Divide => lhs / rhs,
    }
}
