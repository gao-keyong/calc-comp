use std::unreachable;

use lazy_static::*;
use pest::iterators::Pair;
use pest::pratt_parser::*;
use pest::Parser;

use crate::ast::*;

#[derive(pest_derive::Parser)]
#[grammar = "calc.pest"]
pub struct CalculatorParser;

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrattParser::new()
            // postfix_op = { "not_implemented" }
            .op(Op::postfix(postfix_op))
            // add_op = { "+" | "-" }
            .op(Op::infix(add_op, Left))
            // mul_op = { "*" | "/" }
            .op(Op::infix(mul_op, Left))
            // prefix_op = { "+" | "-" }
            .op(Op::prefix(prefix_op))
    };
}

pub fn parse(src: &str) -> Result<TransUnit, pest::error::Error<Rule>> {
    let mut grammar_pairs = CalculatorParser::parse(Rule::grammar, src)?;
    let tu = parse_grammar(grammar_pairs.next().unwrap())?;
    Ok(tu)
}

// grammar = { trans_unit ~ EOI }
fn parse_grammar(pair: Pair<Rule>) -> Result<TransUnit, pest::error::Error<Rule>> {
    let tu = pair.into_inner().next().unwrap();
    parse_trans_unit(tu)
}

// trans_unit = { block }
fn parse_trans_unit(pair: Pair<Rule>) -> Result<TransUnit, pest::error::Error<Rule>> {
    let block = parse_block(pair.into_inner().next().unwrap())?;
    Ok(TransUnit { block })
}

// block = { stmt* }
fn parse_block(pair: Pair<Rule>) -> Result<Block, pest::error::Error<Rule>> {
    let mut stmts: Vec<Stmt> = Vec::new();
    for stmt in pair.into_inner() {
        stmts.push(parse_stmt(stmt)?);
    }
    Ok(Block { stmts })
}

// stmt = { expr_stmt | print_stmt }
fn parse_stmt(pair: Pair<Rule>) -> Result<Stmt, pest::error::Error<Rule>> {
    let stmt = pair.into_inner().next().unwrap();
    match stmt.as_rule() {
        Rule::expr_stmt => Ok(parse_expr_stmt(stmt)?),
        Rule::print_stmt => Ok(parse_print_stmt(stmt)?),
        _ => unreachable!(),
    }
}

// print_stmt = { "print" ~ expr ~ ";" }
fn parse_print_stmt(pair: Pair<Rule>) -> Result<Stmt, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    let expr = parse_expr(inner)?;
    Ok(Stmt::PrintStmt(expr))
}

// expr_stmt = { expr ~ ";" }
fn parse_expr_stmt(pair: Pair<Rule>) -> Result<Stmt, pest::error::Error<Rule>> {
    let expr = parse_expr(pair.into_inner().next().unwrap());
    Ok(Stmt::ExprStmt(expr?))
}

// primary_expr = { MEM | INT | "(" ~ expr ~ ")" }
fn parse_primary_expr(pair: Pair<Rule>) -> Result<PrimaryExpr, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::MEM => Ok(PrimaryExpr::Mem),
        Rule::INT => Ok(PrimaryExpr::Int(inner.as_str().parse::<i64>().unwrap())),
        Rule::expr => Ok(PrimaryExpr::Expr(Box::new(parse_expr(inner).unwrap()))),
        _ => unreachable!(),
    }
}

// expr = { prefix_op* ~ primary_expr ~ postfix_op* ~ (infix_op ~ prefix_op* ~ primary_expr ~ postfix_op* )* }
fn parse_expr(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let inner = pair.into_inner();
    let expr = PRATT_PARSER
        .map_primary(|primary| {
            Ok(Expr::Primary(Box::new(
                parse_primary_expr(primary).unwrap(),
            )))
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::mul_op => Ok(Expr::Infix(Box::new(InfixExpr {
                lhs: Box::new(lhs.unwrap()),
                op: parse_mul_op(op).unwrap(),
                rhs: Box::new(rhs.unwrap()),
            }))),
            Rule::add_op => Ok(Expr::Infix(Box::new(InfixExpr {
                lhs: Box::new(lhs.unwrap()),
                op: parse_add_op(op).unwrap(),
                rhs: Box::new(rhs.unwrap()),
            }))),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_str() {
            "+" => Ok(Expr::Prefix(Box::new(PrefixExpr {
                op: PrefixOp::Plus,
                expr: Box::new(rhs.unwrap()),
            }))),
            "-" => Ok(Expr::Prefix(Box::new(PrefixExpr {
                op: PrefixOp::Minus,
                expr: Box::new(rhs.unwrap()),
            }))),
            _ => unreachable!(),
        })
        .parse(inner);
    expr
}

fn parse_mul_op(pair: Pair<Rule>) -> Result<InfixOp, pest::error::Error<Rule>> {
    match pair.as_str() {
        "*" => Ok(InfixOp::Multiply),
        "/" => Ok(InfixOp::Divide),
        _ => unreachable!(),
    }
}

fn parse_add_op(pair: Pair<Rule>) -> Result<InfixOp, pest::error::Error<Rule>> {
    match pair.as_str() {
        "+" => Ok(InfixOp::Plus),
        "-" => Ok(InfixOp::Minus),
        _ => unreachable!(),
    }
}
