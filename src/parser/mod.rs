use std::{fmt::{Debug, Display}, iter::Peekable};

use itertools::Itertools;

use crate::{common::CError, err, err_, lexer::{Token, TokenType}};
use crate::common::Span;

pub type AST = ASTBlock;
#[derive(PartialEq, Eq, Debug)]
pub struct ASTNode {
	data: ASTNodeData,
	span: Span,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ASTNodeData {
	Statement(ASTStatement),
	Block(ASTBlock),
}

#[derive(PartialEq, Eq, Debug)]
pub enum ASTType {
	Literal(Token),
}

#[derive(PartialEq, Eq, Debug)]
pub enum ASTBlock {
	If {
		condition: ASTExpression,
		statements: Vec<ASTNode>,
	},
	Loop {
		statements: Vec<ASTNode>,
	},
	For {
		declaration: Declaration,
		condition: ASTExpression,
		increment: Option<ASTStatement>,
		statements: Vec<ASTNode>,
	},
	While {
		condition: ASTExpression,
		statements: Vec<ASTNode>,
	},
	Function {
		name: Token,
		arguments: Vec<(Token, ASTType)>,
		return_type: Option<ASTType>,
		statements: Vec<ASTNode>,
	},
	Root {
		statements: Vec<ASTNode>,
	},
}

#[derive(PartialEq, Eq, Debug)]
pub struct Declaration {
	binding: DeclarationType,
	identifier: Token,
	typ: Option<ASTType>,
	value: ASTExpression,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ASTStatement {
	Expression(ASTExpression),
	Declaration(Declaration),
	Break,
	Continue,
	Return(Option<ASTExpression>),
}

#[derive(PartialEq, Eq, Debug)]
pub enum DeclarationType {
	Var, Val, Cfg
}

#[derive(PartialEq, Eq, Debug)]
pub enum ASTExpression {
	Leaf(Token),
	UnaryOperator {
		operator: Token,
		operand: Box<ASTExpression>,
	},
	BinaryOperator {
		left: Box<ASTExpression>,
		operator: Token,
		right: Box<ASTExpression>,
	},
	FunctionCall {
		function: Box<ASTExpression>,
		arguments: Vec<ASTExpression>,
	}
}

#[derive(PartialEq, Eq, Debug)]
enum ASTExpressionBuilder {
	Leaf(Token),
	UnaryOperator {
		operator: Token,
		operand: Box<ASTExpressionBuilder>,
		is_paren: bool,
	},
	BinaryOperator {
		left: Box<ASTExpressionBuilder>,
		operator: Token,
		right: Box<ASTExpressionBuilder>,
		is_paren: bool,
	},
	FunctionCall {
		function: Box<ASTExpressionBuilder>,
		arguments: Vec<ASTExpressionBuilder>,
	}
}
impl ASTExpressionBuilder {
	fn set_paren(&mut self){
		match self {
			ASTExpressionBuilder::UnaryOperator { is_paren, .. } | ASTExpressionBuilder::BinaryOperator { is_paren, .. } =>
				*is_paren = true,
			_ => {},
		}
	}
}
impl Into<ASTExpression> for ASTExpressionBuilder {
	fn into(self) -> ASTExpression {
		match self {
			ASTExpressionBuilder::Leaf(token) => ASTExpression::Leaf(token),
			ASTExpressionBuilder::UnaryOperator { operator, operand, .. } =>
				ASTExpression::UnaryOperator { operator, operand: Box::new((*operand).into()) },
			ASTExpressionBuilder::BinaryOperator { left, operator, right, .. } =>
				ASTExpression::BinaryOperator { left: Box::new((*left).into()), operator, right: Box::new((*right).into()) },
			ASTExpressionBuilder::FunctionCall { function, arguments } =>
				ASTExpression::FunctionCall { function: Box::new((*function).into()), arguments: arguments.into_iter().map(|n| n.into()).collect() },
		}
	}
}

fn display_statements(statements: &Vec<ASTNode>) -> String {
	statements.iter().map(|s| format!("{s}")).join("\n")
}

fn indent_string(x:String) -> String {
	x.split('\n').map(|s| format!("  {s}")).join("\n")
}

impl Display for ASTBlock {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ASTBlock::If { condition, statements } =>
				write!(f, "if {condition} {{\n{}\n}}", indent_string(display_statements(statements))),
			ASTBlock::Loop { statements } =>
				write!(f, "loop {{\n{}\n}}", indent_string(display_statements(statements))),
			ASTBlock::For { declaration, condition, increment, statements } =>
				write!(f, "for({declaration};{condition};{}){{\n{}\n}}", match increment {
					Some(increment) => format!("{increment}"),
					_ => "".to_string(),
				}, indent_string(display_statements(statements))),
			ASTBlock::While { condition, statements } =>
				write!(f, "while({condition}){{\n{}\n}}", indent_string(display_statements(statements))),
			ASTBlock::Function { name, arguments, return_type, statements } =>
				write!(f, "fn {name}({}){} {{\n{}\n}}",
					arguments.iter().map(|(name, typ)| format!("{name}: {typ}")).join(", "),
					match return_type {
						Some(typ) => format!(": {typ}"),
						None => "".to_string(),
					},
					indent_string(display_statements(statements))
				),
			ASTBlock::Root { statements } =>
				write!(f, "{}", display_statements(statements)),
		}
	}
}

impl Display for ASTStatement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ASTStatement::Expression(expr) => write!(f, "{expr}"),
			ASTStatement::Declaration(declaration) => write!(f, "{declaration}"),
			ASTStatement::Break => write!(f, "break"),
			ASTStatement::Continue => write!(f, "continue"),
			ASTStatement::Return(expr) => match expr {
				Some(expr) => write!(f, "return {expr}"),
				None => write!(f, "return"),
			},
		}
	}
}

impl Display for ASTExpression {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ASTExpression::Leaf(token) => write!(f, "{token}"),
			ASTExpression::UnaryOperator { operator, operand } => write!(f, "({operator} {operand})"),
			ASTExpression::BinaryOperator { left, operator, right } if operator.variant == TokenType::operator_access =>
				write!(f, "{left}.{right}"),
			ASTExpression::BinaryOperator { left, operator, right } => write!(f, "({left} {operator} {right})"),
			ASTExpression::FunctionCall { function, arguments } =>
				write!(f, "{function}({})", arguments.iter().map(|a| format!("{a}")).join(", ")),
		}
	}
}

impl Display for Declaration {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let typ = match &self.typ {
			Some(typ) => format!(": {typ}"),
			None => "".to_string(),
		};
		write!(f, "{} {}{typ} = {}", match self.binding {
			DeclarationType::Var => "var",
			DeclarationType::Val => "val",
			DeclarationType::Cfg => "cfg",
		}, self.identifier, self.value)
	}
}

impl Display for ASTType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ASTType::Literal(token) => Display::fmt(token, f)
		}
	}
}

impl Display for ASTNode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		Display::fmt(&self.data, f)
	}
}

impl Display for ASTNodeData {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ASTNodeData::Block(block) => Display::fmt(block, f),
			ASTNodeData::Statement(statement) => Display::fmt(statement, f),
		}
	}
}


///operator must be an operator
fn operator_priority(operator:TokenType) -> u8 {
	use TokenType as TT;
	match operator {
		TT::operator_access => 7,
		TT::operator_increment => 6,
		TT::operator_not => 6,
		TT::operator_multiply => 5,
		TT::operator_divide => 5,
		TT::operator_modulo => 5,
		TT::operator_add => 4,
		TT::operator_minus => 4,
		TT::operator_equal_to => 3,
		TT::operator_loose_equal_to => 3,
		TT::operator_not_equal_to => 3,
		TT::operator_greater_than => 3,
		TT::operator_less_than => 3,
		TT::operator_greater_than_eq => 3,
		TT::operator_less_than_eq => 3,
		TT::operator_and => 2,
		TT::operator_or => 1,
		TT::operator_assignment => 0,
		_ => unreachable!(),
	}
}

fn get_leaf_node_or_paren_nodes(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Result<ASTExpressionBuilder, CError> {
	use TokenType as TT;
	tokens.peeking_take_while(|t| t.variant == TT::newline).count(); //skip all newlines
	let token = tokens.next().ok_or(err_!("Unexpected EOF", None))?;
	match token.variant {
		TT::identifier | TT::link | TT::number | TT::string => Ok(ASTExpressionBuilder::Leaf(token)),
		TT::parenthesis_open => {
			let mut expr = get_expression_inner(tokens, true)?;
			require_type(tokens, TokenType::parenthesis_close)?;
			expr.set_paren();
			Ok(expr)
		},
		t => err!(format!("Unexpected token: expected a leaf node or the start of an expression, got {t:?}"), token.span),
	}
}

fn insert_binary_operator(expr: ASTExpressionBuilder, operator: Token, right: ASTExpressionBuilder) -> ASTExpressionBuilder {
	match expr {
		ASTExpressionBuilder::Leaf(_) =>
			ASTExpressionBuilder::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right), is_paren: false },
		ASTExpressionBuilder::UnaryOperator { operator: ref unary, is_paren, .. }
		if is_paren || operator_priority(operator.variant.clone()) <= operator_priority(unary.variant.clone()) =>
			ASTExpressionBuilder::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right), is_paren: false },
		ASTExpressionBuilder::UnaryOperator { operator: unary, operand, .. } =>
			ASTExpressionBuilder::UnaryOperator { operator: unary, operand: Box::new(insert_binary_operator(*operand, operator, right)), is_paren: false },
		ASTExpressionBuilder::BinaryOperator { operator: ref existing, is_paren, .. }
		if is_paren || operator_priority(existing.variant.clone()) >= operator_priority(operator.variant.clone()) =>
			ASTExpressionBuilder::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right), is_paren: false },
		ASTExpressionBuilder::BinaryOperator { left, operator: existing, right: left_right, .. } =>
			ASTExpressionBuilder::BinaryOperator { left: left, operator: existing, right: Box::new(insert_binary_operator(*left_right, operator, right)), is_paren: false },
		ASTExpressionBuilder::FunctionCall { .. } =>
			ASTExpressionBuilder::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right), is_paren: false },
	}
}

fn insert_function_call(expr: ASTExpressionBuilder, arguments: Vec<ASTExpressionBuilder>) -> ASTExpressionBuilder {
	match expr {
		ASTExpressionBuilder::Leaf(_) | ASTExpressionBuilder::FunctionCall { .. } =>
			ASTExpressionBuilder::FunctionCall { function: Box::new(expr), arguments },
		ASTExpressionBuilder::UnaryOperator { is_paren, .. } | ASTExpressionBuilder::BinaryOperator { is_paren, .. } if is_paren =>
			ASTExpressionBuilder::FunctionCall { function: Box::new(expr), arguments },
		ASTExpressionBuilder::UnaryOperator { operator: unary, operand, .. } =>
			ASTExpressionBuilder::UnaryOperator { operator: unary, operand: Box::new(insert_function_call(*operand, arguments)), is_paren: false },
		ASTExpressionBuilder::BinaryOperator { left, operator: existing, right: left_right, .. } =>
			ASTExpressionBuilder::BinaryOperator { left: left, operator: existing, right: Box::new(insert_function_call(*left_right, arguments)), is_paren: false },
	}
}

/// Left-to-right parsing. O(n) time complexity.
fn get_expression_inner(tokens:&mut Peekable<impl Iterator<Item = Token>>, allow_line_breaks: bool) -> Result<ASTExpressionBuilder, CError> {
	use TokenType as TT;
	match tokens.peek() {
		Some(_) => {
			let mut nest_level = 0;
			let mut expr: Option<ASTExpressionBuilder> = None;
			loop {
				let Some(next) = tokens.peek() else { break };
				match next.variant {
					TT::parenthesis_open => match expr {
						Some(e) => {
							let mut arguments = vec![];
							tokens.next(); //consume the (
							while let Some(next) = tokens.peek() {
								if next.variant == TokenType::parenthesis_close {
									tokens.next(); //consume and leave
									break;
								} else if next.variant == TokenType::punctuation_comma {
									tokens.next(); //consume the comma and continue
								}
								arguments.push(get_expression_inner(tokens, true)?);
							}
							expr = Some(insert_function_call(e, arguments));
						},
						None => {
							nest_level += 1;
							tokens.next();
						}
					},
					TT::parenthesis_close => {
						nest_level -= 1;
						if nest_level < 0 { break }
						tokens.next();
					},
					TT::operator_minus if expr.is_none() => {
						let operator = tokens.next().unwrap();
						let right = get_leaf_node_or_paren_nodes(tokens)?;
						expr = Some(ASTExpressionBuilder::UnaryOperator { operator, operand: Box::new(right), is_paren: false });
					},
					TT::operator_not | TT::operator_increment => {
						let operator = tokens.next().unwrap();
						if expr.is_some() { return err!("Expected operator or end of expression, not unary operator", operator.span) }
						let right = get_leaf_node_or_paren_nodes(tokens)?;
						expr = Some(ASTExpressionBuilder::UnaryOperator { operator, operand: Box::new(right), is_paren: false });
					},
					TT::operator_assignment |
					TT::operator_equal_to |
					TT::operator_loose_equal_to |
					TT::operator_not_equal_to |
					TT::operator_greater_than |
					TT::operator_less_than |
					TT::operator_greater_than_eq |
					TT::operator_less_than_eq |
					TT::operator_and |
					TT::operator_or |
					TT::operator_add |
					TT::operator_multiply |
					TT::operator_divide |
					TT::operator_modulo |
					TT::operator_access |
					TT::operator_minus => {
						let left = expr.take().ok_or(err_!("Unexpected binary operator with no preceding expression", next.span.clone()))?;
						let operator = tokens.next().unwrap();
						let right = get_leaf_node_or_paren_nodes(tokens)?;
						expr.replace(insert_binary_operator(left, operator, right));
					},
					TT::identifier | TT::link | TT::number | TT::string => {
						if expr.is_some() { return err!("Expected operator or end of expression, not another expression", next.span.clone()); }
						expr.replace(ASTExpressionBuilder::Leaf(tokens.next().unwrap()));
					},
					TT::newline if allow_line_breaks => { tokens.next(); },
					_ => { break },
				}
			}
			match expr {
				Some(expr) => Ok(expr),
				None => match tokens.peek() {
					Some(next) => err!("Invalid expression", next.span.clone()),
					None => err!("Invalid expression", None),
				},
			}
		},
		None => err!("Expected expression, got end of file", None),
	}
}

fn get_expression(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Result<ASTExpression, CError> {
	get_expression_inner(tokens, false).map(|x| x.into())
}

fn get_expression_allow_line_breaks(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Result<ASTExpression, CError> {
	get_expression_inner(tokens, true).map(|x| x.into())
}

fn get_type_optional(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Result<Option<ASTType>, CError> {
	use TokenType as TT;
	match tokens.peek() {
		Some(Token { variant: TT::punctuation_colon, .. }) => {
			let colon = tokens.next().unwrap();
			let token = tokens.next().ok_or(err_!("expected an identifier, got end of input", colon.span))?;
			if token.variant != TT::identifier { return err!(format!("expected an identifier, got \"{}\"", token.text), token.span); }
			Ok(Some(ASTType::Literal(token)))
		},
		_ => Ok(None),
	}
}

fn get_tokens_until_eol(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Vec<Token> {
	let mut paren_nest_level = 0;
	let mut brace_nest_level = 0;
	let out: Vec<Token> = tokens.peeking_take_while(move |tk| {
		use TokenType as TT;
		match tk.variant {
			TT::parenthesis_open => paren_nest_level += 1,
			TT::parenthesis_close => paren_nest_level -= 1,
			TT::brace_open => brace_nest_level += 1,
			TT::brace_close => brace_nest_level -= 1,
			_ => {}
		}
		!(paren_nest_level == 0 && brace_nest_level == 0 && matches!(tk.variant, TT::newline | TT::punctuation_semicolon))
	}).collect();
	//Consume the EOL or semicolon, if it exists
	//If it doesn't, that's fine
	tokens.next();
	out
}

fn require_type(tokens:&mut Peekable<impl Iterator<Item = Token>>, typ: TokenType) -> Result<Token, CError> {
	match tokens.next() {
		Some(t) => if t.variant != typ {
			err!(format!("expected {typ:?}, got \"{}\"", t.text), t.span)
		} else { Ok(t) },
		_ => err!(format!("expected {typ:?}, got end of file"), None),
	}
}
fn require_type_peek(tokens:&mut Peekable<impl Iterator<Item = Token>>, typ: TokenType) -> Result<(), CError> {
	match tokens.peek() {
		Some(token) if token.variant == typ => Ok(()),
		Some(token) => err!(format!("expected {typ:?}, got \"{}\"", token.text), token.span.clone()),
		None => err!(format!("expected {typ:?}, got end of file"), None),
	}
}

pub fn parse(tokens: Vec<Token>) -> Result<AST, CError> {
	Ok(ASTBlock::Root { statements: parse_statements(tokens)? })
}

fn parse_block(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Result<Vec<ASTNode>, CError> {
	use TokenType as TT;
	match tokens.peek() {
		Some(Token { variant: TT::brace_open, .. }) => {
			tokens.next();
			let mut nest_level = 1;
			let block = tokens.peeking_take_while(move |tk| {
				match tk.variant {
					TT::brace_open => nest_level += 1,
					TT::brace_close => nest_level -= 1,
					_ => {},
				}
				nest_level > 0
			}).collect();
			let statements = parse_statements(block)?;
			require_type(tokens, TokenType::brace_close)?;
			Ok(statements)
		},
		Some(_) => parse_statements(get_tokens_until_eol(tokens)),
		None => err!("expected a block, got EOF", None),
	}
}

/// 
fn parse_declaration_statement(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Declaration, CError> {
	use TokenType as TT;
	let token = tokens.next().ok_or(err_!("Expected a declaration statement, got end of file", None))?;
	let binding = match token.variant {
		TT::keyword_cfg => DeclarationType::Cfg,
		TT::keyword_val => DeclarationType::Val,
		TT::keyword_var => DeclarationType::Var,
		_ => return err!(format!("Expected a variable binding, got \"{}\"", token.text), token.span)
	};
	let identifier = tokens.next().ok_or(err_!("expected identifier, got end of file", None))?;
	if identifier.variant != TT::identifier { return err!(format!("expected identifier, got {}", identifier.text), identifier.span) }
	let eq = tokens.next().ok_or(err_!("expected =, got end of file", None))?;
	if eq.variant != TT::operator_assignment { return err!("expected =", eq.span); }
	let value = get_expression(tokens)?;
	Ok(Declaration {
		binding,
		identifier,
		typ: get_type_optional(tokens)?,
		value,
	})
}

fn parse_statements(tokens: Vec<Token>) -> Result<Vec<ASTNode>, CError> {
	let mut tokens = tokens.into_iter().peekable();
	let mut statements = vec![];
	while let Some(tk) = tokens.peek() {
		use TokenType as TT;
		let statement: ASTNodeData = match tk.variant {
			TT::brace_close | TT::brace_open | TT::parenthesis_close | TT::punctuation_colon | TT::punctuation_comma | TT::punctuation_interpolate => return err!("Unexpected token", tk.span.clone()),
			TT::operator_assignment |
			TT::operator_equal_to |
			TT::operator_loose_equal_to |
			TT::operator_not_equal_to |
			TT::operator_greater_than |
			TT::operator_less_than |
			TT::operator_greater_than_eq |
			TT::operator_less_than_eq |
			TT::operator_and |
			TT::operator_or |
			TT::operator_add |
			TT::operator_multiply |
			TT::operator_divide |
			TT::operator_access |
			TT::operator_modulo => return err!("Unexpected binary operator with no preceding expression", tk.span.clone()),
			TT::punctuation_semicolon => return err!("Duplicate or unnecessary semicolon", tk.span.clone()),

			TT::newline => {tokens.next(); continue},

			TT::keyword_cfg | TT::keyword_val | TT::keyword_var => {
				ASTNodeData::Statement(ASTStatement::Declaration(parse_declaration_statement(&mut tokens)?))
			},
			TT::keyword_break => {
				tokens.next();
				ASTNodeData::Statement(ASTStatement::Break)
			}
			TT::keyword_continue => {
				tokens.next();
				ASTNodeData::Statement(ASTStatement::Continue)
			},
			TT::keyword_loop => {
				tokens.next();
				ASTNodeData::Block(ASTBlock::Loop { statements: parse_block(&mut tokens)? })
			},
			TT::keyword_return => {
				tokens.next();
				ASTNodeData::Statement(ASTStatement::Return(match tokens.peek() {
					None | Some(Token { variant: TokenType::punctuation_semicolon | TokenType::newline, .. }) => None,
					_ => Some(get_expression(&mut tokens)?),
				}))
			},
			TT::keyword_if => {
				tokens.next();
				require_type(&mut tokens, TokenType::parenthesis_open)?;
				let condition = get_expression_allow_line_breaks(&mut tokens)?;
				require_type(&mut tokens, TokenType::parenthesis_close)?;
				let statements = parse_block(&mut tokens)?;
				ASTNodeData::Block(ASTBlock::If { condition, statements })
			},
			TT::keyword_fn => {
				tokens.next();
				let name = require_type(&mut tokens, TokenType::identifier)?;
				ASTNodeData::Block(ASTBlock::Function {
					name,
					arguments: {
						let mut arguments = vec![];
						require_type(&mut tokens, TokenType::parenthesis_open)?;
						while let Some(token) = tokens.next() {
							match token.variant {
								TT::identifier => {
									let span = token.span.clone();
									let typ = get_type_optional(&mut tokens)?.ok_or(err_!("Please specify the type for this token", span))?;
									arguments.push((token, typ));
									if tokens.peek().is_some_and(|t| t.variant == TokenType::punctuation_comma) {
										tokens.next();
									}
								},
								TT::parenthesis_close => break,
								_ => return err!("Unexpected token: Expected an argument, or end of arguments", token.span),
							}
						}
						arguments
					},
					return_type: get_type_optional(&mut tokens)?,
					statements: parse_block(&mut tokens)?,
				})
			},
			TT::keyword_for => {
				tokens.next();
				require_type(&mut tokens, TokenType::parenthesis_open)?;
				let declaration = parse_declaration_statement(&mut tokens)?;
				require_type(&mut tokens, TokenType::punctuation_semicolon)?;
				let condition = get_expression(&mut tokens)?;
				require_type(&mut tokens, TokenType::punctuation_semicolon)?;
				let increment = match tokens.peek() {
					Some(Token { variant: TokenType::parenthesis_close, .. }) => None,
					Some(_) => Some(ASTStatement::Expression(get_expression_allow_line_breaks(&mut tokens)?)),
					None => return err!("Unexpected end of file", None),
				};
				require_type(&mut tokens, TokenType::parenthesis_close)?;
				let statements = parse_block(&mut tokens)?;
				ASTNodeData::Block(ASTBlock::For { declaration, condition, increment, statements })
			},
			TT::keyword_while => {
				tokens.next();
				require_type(&mut tokens, TokenType::parenthesis_open)?;
				let condition = get_expression_allow_line_breaks(&mut tokens)?;
				require_type(&mut tokens, TokenType::parenthesis_close)?;
				let statements = parse_block(&mut tokens)?;
				ASTNodeData::Block(ASTBlock::While { condition, statements })
			},
			TT::identifier | TT::link | TT::number | TT::string |
			TT::operator_minus | TT::operator_not | TT::operator_increment |
			TT::parenthesis_open =>
				ASTNodeData::Statement(ASTStatement::Expression(get_expression(&mut tokens)?)),
		};
		statements.push(ASTNode { data: statement, span: 0..0 });
	}
	Ok(statements)
}


#[cfg(test)]
mod tests {
	use crate::lexer::test_utils::TokenBuilder;
	use crate::parser::*;
	use pretty_assertions::assert_eq;

	#[test]
	fn parse_test(){
		// print("Hello, world!")
		// printflush(`message1`)


		// fn factorial(x: num): num {
		//   if(x <= 1) return 1
		//   return x * factorial(x - 1)
		// }
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.ident("print"), b.popen(), b.str("Hello, world!"), b.pclose(), b.newline(),
				b.ident("printflush"), b.popen(), b.link("message1"), b.pclose(), b.newline(),
				b.newline(),
				b.kfn(), b.ident("factorial"), b.popen(),
				b.ident("x"), b.colon(), b.ident("num"),
				b.pclose(), b.colon(), b.ident("num"), b.bopen(), b.newline(),
				b.kif(), b.popen(), b.ident("x"), b.lt(), b.num("1"), b.pclose(),
				b.kreturn(), b.num("1"), b.newline(),
				b.kreturn(), b.ident("x"), b.mult(), b.ident("factorial"), b.popen(), b.ident("x"), b.minus(), b.num("1"), b.pclose(), b.newline(),
				b.bclose(),
				b.newline_last(),
			]),
			Ok(ASTBlock::Root { statements: vec![
				ASTNode {
					data: ASTNodeData::Statement(
						ASTStatement::Expression(ASTExpression::FunctionCall {
							function: Box::new(ASTExpression::Leaf(b.ident("print"))),
							arguments: vec![
								ASTExpression::Leaf(b.str("Hello, world!"))
							]
						})
					),
					span: 0..0
				},
				ASTNode {
					data: ASTNodeData::Statement(
						ASTStatement::Expression(ASTExpression::FunctionCall {
							function: Box::new(ASTExpression::Leaf(b.ident("printflush"))),
							arguments: vec![
								ASTExpression::Leaf(b.link("message1"))
							]
						})
					),
					span: 0..0
				},
				ASTNode {
					data: ASTNodeData::Block(ASTBlock::Function {
						name: b.ident("factorial"),
						arguments: vec![(b.ident("x"), ASTType::Literal(b.ident("num")))],
						return_type: Some(ASTType::Literal(b.ident("num"))),
						statements: vec![
							ASTNode {
								data: ASTNodeData::Block(ASTBlock::If {
									condition: ASTExpression::BinaryOperator {
										left: Box::new(ASTExpression::Leaf(b.ident("x"))),
										operator: b.lt(),
										right: Box::new(ASTExpression::Leaf(b.num("1"))),
									},
									statements: vec![
										ASTNode {
											data: ASTNodeData::Statement(ASTStatement::Return(Some(ASTExpression::Leaf(b.num("1"))))),
											span: 0..0
										}
									]
								}),
								span: 0..0,
							},
							ASTNode {
								data: ASTNodeData::Statement(ASTStatement::Return(Some(
									ASTExpression::BinaryOperator {
										left: Box::new(ASTExpression::Leaf(b.ident("x"))),
										operator: b.mult(),
										right: Box::new(ASTExpression::FunctionCall {
											function: Box::new(ASTExpression::Leaf(b.ident("factorial"))),
											arguments: vec![ASTExpression::BinaryOperator {
												left: Box::new(ASTExpression::Leaf(b.ident("x"))),
												operator: b.minus(),
												right: Box::new(ASTExpression::Leaf(b.num("1"))),
											}]
										}),
									}
								))),
								span: 0..0
							}
						],
					}),
					span: 0..0
				}
			]})
		)
	}
}
