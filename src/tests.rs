use super::eval::*;

fn expect_ok(input: &str, output: f64) {
    let equation = input.to_string();
    let evaluation = eval(equation);
    assert!(matches!(evaluation, Ok(_)));
    let num = evaluation.unwrap();
    assert_eq!(num, output);
}

#[test]
fn simple_sum() {
    expect_ok("10 + 10", 20.0);
}

#[test]
fn unary_operator() {
    expect_ok("-10", -10.0);
}

#[test]
fn predence_order() {
    expect_ok("5 * 10 ^ 2 + 2", 502.0);
}

#[test]
fn parentheses() {
    expect_ok("(10 + 10) * 2", 40.0);
}

#[test]
fn unexpected_end() {
    let equation = "3 * ".to_string();
    let evaluation = eval(equation);
    assert!(matches!(evaluation, Err(ParseError::UnexpectedEnd)));
}

#[test]
fn unexpected_token() {
    let equation = " * 3".to_string();
    let evaluation = eval(equation);
    assert!(matches!(evaluation, Err(ParseError::UnexpectedToken(..))));
}

#[test]
fn unclosed_parentheses() {
    let equation = "(10 + 10 * 2".to_string();
    let evaluation = eval(equation);
    assert!(matches!(evaluation, Err(ParseError::UnexpectedEnd)));
}

#[test]
fn never_opened_parentheses() {
    let equation = "10 + 10) * 2".to_string();
    let evaluation = eval(equation);
    assert!(matches!(evaluation, Err(ParseError::UnexpectedToken(..))));
}
