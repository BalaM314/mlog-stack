use std::{fmt::{Debug, Display}, iter::Peekable};

use itertools::Itertools;

use crate::{common::CError, err, err_, lexer::{Token, TokenType}};
use crate::common::Span;

pub type AST = ASTBlock;
#[derive(PartialEq, Eq, Debug)]
pub struct ASTNode {
	pub data: ASTNodeData,
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
		else_statements: Vec<ASTNode>,
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
		parameters: Vec<(Token, ASTType)>,
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
	/// Token must have one of these types: identifier, link, number, string
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
	},
	/// values.len() must always be 1 less than strings.len()
	/// if the string is "", then strings must be vec![""] and values must be vec![]
	/// if the initial string was "${0}", then strings must be vec!["",""] and values must be vec![0]
	TemplateString {
		/// Must be of type TokenType::string_fragment
		strings: Vec<Token>,
		values: Vec<ASTExpression>,
	},
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
	},
	/// values.len() must always be 1 less than strings.len()
	/// if the string is "", then strings must be vec![""] and values must be vec![]
	/// if the initial string was "${0}", then strings must be vec!["",""] and values must be vec![0]
	TemplateString {
		/// Must be of type TokenType::string_fragment
		strings: Vec<Token>,
		values: Vec<ASTExpressionBuilder>,
	},
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
			ASTExpressionBuilder::TemplateString { strings, values } =>
				ASTExpression::TemplateString { strings, values: values.into_iter().map(|n| n.into()).collect() },
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
			ASTBlock::If { condition, statements, else_statements } if !else_statements.is_empty() =>
				write!(f,
					"if {condition} {{\n{}\n}} else {{\n{}\n}}",
					indent_string(display_statements(statements)),
					indent_string(display_statements(else_statements))
				),
			ASTBlock::If { condition, statements, .. } =>
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
			ASTBlock::Function { name, parameters: arguments, return_type, statements } =>
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
			ASTExpression::TemplateString { strings, values } => {
				let mut values = values.iter();
				for str in strings {
					write!(f, "{str}")?;
					if let Some(value) = values.next() {
						write!(f, "${{{value}}}")?;
					}
				}
				Ok(())
			}
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

enum Associative {
	Right, Left, Either,
}

fn operator_associative(operator:TokenType) -> Associative {
	use TokenType as TT;
	match operator {
		TT::operator_exponentiate |
		TT::operator_assignment |
		TT::operator_assignment_add |
		TT::operator_assignment_subtract |
		TT::operator_assignment_multiply |
		TT::operator_assignment_divide =>
			Associative::Right,
		TT::operator_access | TT::operator_minus |
		TT::operator_divide | TT::operator_integer_divide |
		TT::operator_modulo | TT::operator_euclidian_modulo |
		TT::operator_shift_left | TT::operator_shift_right | TT::operator_shift_right_unsigned =>
			Associative::Left,
		TT::operator_add | TT::operator_multiply |
		TT::operator_bitwise_and | TT::operator_bitwise_or | TT::operator_bitwise_xor =>
			Associative::Either,
		_ => Associative::Left,
	}
}

///operator must be an operator
fn operator_priority_gt_function_call(operator:TokenType) -> bool {
	return operator == TokenType::operator_access;
}

///operator must be an operator
fn operator_priority(operator:TokenType) -> u8 {
	use TokenType as TT;
	match operator {
		TT::operator_access => 9,
		//function call => 8.5
		TT::operator_increment |
		TT::operator_not |
		TT::operator_bitwise_flip => 8,
		TT::operator_exponentiate => 8,
		TT::operator_multiply |
		TT::operator_divide |
		TT::operator_integer_divide |
		TT::operator_modulo |
		TT::operator_euclidian_modulo => 7,
		TT::operator_add |
		TT::operator_minus => 6,
		TT::operator_shift_left |
		TT::operator_shift_right |
		TT::operator_shift_right_unsigned => 5,
		TT::operator_greater_than |
		TT::operator_less_than |
		TT::operator_greater_than_eq |
		TT::operator_less_than_eq => 4,
		TT::operator_equal_to |
		TT::operator_loose_equal_to |
		TT::operator_not_equal_to => 3,
		TT::operator_logical_and |
		TT::operator_bitwise_and => 2,
		TT::operator_bitwise_xor |
		TT::operator_logical_or |
		TT::operator_bitwise_or => 1,
		TT::operator_assignment |
		TT::operator_assignment_add |
		TT::operator_assignment_subtract |
		TT::operator_assignment_multiply |
		TT::operator_assignment_divide => 0,
		_ => unreachable!(),
	}
}

fn skip_newlines(tokens:&mut Peekable<impl Iterator<Item = Token>>){
	tokens.peeking_take_while(|t| t.variant == TokenType::newline).count(); //skip all newlines
}

fn get_leaf_node_or_paren_nodes(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Result<ASTExpressionBuilder, CError> {
	use TokenType as TT;
	skip_newlines(tokens);
	let token = tokens.peek().ok_or(err_!("Unexpected EOF", None))?;
	match token.variant {
		TT::identifier | TT::link | TT::number | TT::string => Ok(ASTExpressionBuilder::Leaf(tokens.next().unwrap())),
		TT::string_fragment => get_string_fragment(tokens),
		TT::parenthesis_open => {
			tokens.next().unwrap();
			let mut expr = get_expression_inner(tokens, true)?;
			require_type(tokens, TokenType::parenthesis_close)?;
			expr.set_paren();
			Ok(expr)
		},
		t => err!(format!("Unexpected token: expected a leaf node or the start of an expression, got {t:?}"), tokens.next().unwrap().span),
	}
}

//TODO remove clone on TokenType, which is Copy
fn insert_binary_operator(expr: ASTExpressionBuilder, operator: Token, right: ASTExpressionBuilder) -> ASTExpressionBuilder {
	match expr {
		ASTExpressionBuilder::Leaf(_) | ASTExpressionBuilder::TemplateString { .. } =>
			ASTExpressionBuilder::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right), is_paren: false },
		ASTExpressionBuilder::UnaryOperator { operator: ref unary, is_paren, .. }
		if is_paren || operator_priority(operator.variant.clone()) <= operator_priority(unary.variant.clone()) =>
			ASTExpressionBuilder::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right), is_paren: false },
		ASTExpressionBuilder::UnaryOperator { operator: unary, operand, .. } =>
			ASTExpressionBuilder::UnaryOperator { operator: unary, operand: Box::new(insert_binary_operator(*operand, operator, right)), is_paren: false },
		ASTExpressionBuilder::BinaryOperator { operator: ref existing, is_paren, .. }
		if is_paren || match operator_associative(existing.variant.clone()) { //is it correct to use existing here, or should it be the new operator?
			Associative::Left | Associative::Either => u8::ge, //the existing operator has higher priority than the new one
			Associative::Right => u8::gt //the existing operator has lower priority than the new one
		}(&operator_priority(existing.variant.clone()), &operator_priority(operator.variant.clone())) =>
			ASTExpressionBuilder::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right), is_paren: false },
		ASTExpressionBuilder::BinaryOperator { left, operator: existing, right: left_right, .. } =>
			ASTExpressionBuilder::BinaryOperator { left: left, operator: existing, right: Box::new(insert_binary_operator(*left_right, operator, right)), is_paren: false },
		ASTExpressionBuilder::FunctionCall { .. } =>
			ASTExpressionBuilder::BinaryOperator { left: Box::new(expr), operator, right: Box::new(right), is_paren: false },
	}
}

fn insert_function_call(expr: ASTExpressionBuilder, arguments: Vec<ASTExpressionBuilder>) -> ASTExpressionBuilder {
	match expr {
		ASTExpressionBuilder::Leaf(_) | ASTExpressionBuilder::FunctionCall { .. } | ASTExpressionBuilder::TemplateString { .. } =>
			ASTExpressionBuilder::FunctionCall { function: Box::new(expr), arguments },
		ASTExpressionBuilder::UnaryOperator { is_paren, .. } | ASTExpressionBuilder::BinaryOperator { is_paren, .. } if is_paren =>
			ASTExpressionBuilder::FunctionCall { function: Box::new(expr), arguments },
		ASTExpressionBuilder::UnaryOperator { operator: unary, operand, .. } =>
			ASTExpressionBuilder::UnaryOperator { operator: unary, operand: Box::new(insert_function_call(*operand, arguments)), is_paren: false },
		ASTExpressionBuilder::BinaryOperator { is_paren, ref operator, .. } if is_paren || operator_priority_gt_function_call(operator.variant) =>
			ASTExpressionBuilder::FunctionCall { function: Box::new(expr), arguments },
		ASTExpressionBuilder::BinaryOperator { left, operator, right: left_right, .. } =>
			ASTExpressionBuilder::BinaryOperator { left, operator, right: Box::new(insert_function_call(*left_right, arguments)), is_paren: false },
	}
}

fn get_string_fragment(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Result<ASTExpressionBuilder, CError> {
	use TokenType as TT;
	let mut strings = vec![tokens.next().unwrap()];
	let mut values = vec![];
	while let Some(Token { variant: TT::interpolation_start, .. }) = tokens.peek() {
		tokens.next().unwrap();
		values.push(get_expression_inner(tokens, true)?);
		require_type(tokens, TokenType::interpolation_end)?;
		strings.push(require_type_p(tokens, |t| t == TokenType::string_fragment || t == TokenType::string, "string")?);
	}
	if values.is_empty() {
		eprintln!("{:?}", tokens.peek().unwrap().span);
		dbg!(strings);
		unreachable!()
	}
	Ok(ASTExpressionBuilder::TemplateString { strings, values })
}

/// Left-to-right parsing. O(n log n) time complexity.
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
						if let Some(expr) = &mut expr { expr.set_paren() }
					}
					TT::operator_minus if expr.is_none() => {
						let operator = tokens.next().unwrap();
						let right = get_leaf_node_or_paren_nodes(tokens)?;
						expr = Some(ASTExpressionBuilder::UnaryOperator { operator, operand: Box::new(right), is_paren: false });
					},
					TT::operator_not | TT::operator_increment | TT::operator_bitwise_flip => {
						let operator = tokens.next().unwrap();
						if expr.is_some() { return err!("Expected operator or end of expression, not unary operator", operator.span) }
						let right = get_leaf_node_or_paren_nodes(tokens)?;
						expr = Some(ASTExpressionBuilder::UnaryOperator { operator, operand: Box::new(right), is_paren: false });
					},
					TT::operator_access |
					TT::operator_exponentiate |
					TT::operator_multiply |
					TT::operator_divide |
					TT::operator_integer_divide |
					TT::operator_modulo |
					TT::operator_euclidian_modulo |
					TT::operator_add |
					TT::operator_minus |
					TT::operator_shift_left |
					TT::operator_shift_right |
					TT::operator_shift_right_unsigned |
					TT::operator_greater_than |
					TT::operator_less_than |
					TT::operator_greater_than_eq |
					TT::operator_less_than_eq |
					TT::operator_equal_to |
					TT::operator_loose_equal_to |
					TT::operator_not_equal_to |
					TT::operator_logical_and |
					TT::operator_bitwise_and |
					TT::operator_bitwise_xor |
					TT::operator_logical_or |
					TT::operator_bitwise_or |
					TT::operator_assignment |
					TT::operator_assignment_add |
					TT::operator_assignment_subtract |
					TT::operator_assignment_multiply |
					TT::operator_assignment_divide => {
						let left = expr.take().ok_or(err_!("Unexpected binary operator with no preceding expression", next.span.clone()))?;
						let operator = tokens.next().unwrap();
						let right = get_leaf_node_or_paren_nodes(tokens)?;
						expr.replace(insert_binary_operator(left, operator, right));
					},
					TT::identifier | TT::link | TT::number | TT::string | TT::string_fragment if expr.is_some() => return err!("Expected operator or end of expression, not another expression", next.span.clone()),
					TT::identifier | TT::link | TT::number | TT::string => {
						if expr.is_some() { return err!("Expected operator or end of expression, not another expression", next.span.clone()); }
						expr.replace(ASTExpressionBuilder::Leaf(tokens.next().unwrap()));
					},
					TT::string_fragment => expr = Some(get_string_fragment(tokens)?),
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
			if token.variant != TT::identifier { return err!(format!("expected a type, got \"{}\"", token.text), token.span); }
			Ok(Some(ASTType::Literal(token)))
		},
		_ => Ok(None),
	}
}

/// This function just reads tokens from input until the condition is satisfied
/// or the end is reached.
/// It does not check if the tokens make any sense, or if there are unmatched parentheses.
fn get_tokens_until_eol(tokens:&mut Peekable<impl Iterator<Item = Token>>) -> Vec<Token> {
	use TokenType as TT;

	let mut paren_nest_level = 0;
	let mut brace_nest_level = 0;
	let mut out = vec![];
	let continue_to_else_after_newline = tokens.peek().is_some_and(|t| t.variant == TT::keyword_if);
	loop {
		out.extend(tokens.peeking_take_while(move |tk| {
			match tk.variant {
				TT::parenthesis_open => paren_nest_level += 1,
				TT::parenthesis_close => paren_nest_level -= 1,
				TT::brace_open => brace_nest_level += 1,
				TT::brace_close => brace_nest_level -= 1,
				_ => {}
			}
			!(paren_nest_level == 0 && brace_nest_level == 0 && matches!(tk.variant, TT::newline | TT::punctuation_semicolon))
		}));
		if continue_to_else_after_newline {
			//skip and return newlines or semicolons
			out.extend(tokens.peeking_take_while(
				|tk| matches!(tk.variant, TT::newline | TT::punctuation_semicolon)
			));
			//Special case:
			//If the tokens started with if,
			//and there is an else after the newlines, keep reading
			//example:
			//
			//if(a) b
			//else if(c) d
			//else e
			//
			//Everything from b to e is one block
			match tokens.peek() {
				//look at the next line if it starts with an else
				Some(Token { variant: TT::keyword_else, .. }) => continue,
				//otherwise, return
				_ => return out
			}
		} else {
			tokens.next(); //skip a token, it is either a newline, semicolon, or nothing
			return out;
		}
	}
}

fn require_type(tokens:&mut Peekable<impl Iterator<Item = Token>>, typ: TokenType) -> Result<Token, CError> {
	match tokens.next() {
		Some(t) => if t.variant != typ {
			err!(format!("expected {typ:?}, got \"{}\", ({:?})", t.text, t.variant), t.span)
		} else { Ok(t) },
		_ => err!(format!("expected {typ:?}, got end of file"), None),
	}
}
fn require_type_p(tokens:&mut Peekable<impl Iterator<Item = Token>>, typ: impl Fn(TokenType) -> bool, typ_name: &'static str) -> Result<Token, CError> {
	match tokens.next() {
		Some(t) => if !typ(t.variant) {
			err!(format!("expected {typ_name}, got \"{}\", ({:?})", t.text, t.variant), t.span)
		} else { Ok(t) },
		_ => err!(format!("expected {typ_name}, got end of file"), None),
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
			TT::brace_close | TT::brace_open | TT::parenthesis_close | TT::punctuation_colon | TT::punctuation_comma | TT::interpolation_start | TT::interpolation_end | TT::keyword_else =>
				return err!("Unexpected token", tk.span.clone()),
			TT::operator_access |
			TT::operator_exponentiate |
			TT::operator_multiply |
			TT::operator_divide |
			TT::operator_integer_divide |
			TT::operator_modulo |
			TT::operator_euclidian_modulo |
			TT::operator_add |
			TT::operator_shift_left |
			TT::operator_shift_right |
			TT::operator_shift_right_unsigned |
			TT::operator_greater_than |
			TT::operator_less_than |
			TT::operator_greater_than_eq |
			TT::operator_less_than_eq |
			TT::operator_equal_to |
			TT::operator_loose_equal_to |
			TT::operator_not_equal_to |
			TT::operator_logical_and |
			TT::operator_bitwise_and |
			TT::operator_bitwise_xor |
			TT::operator_logical_or |
			TT::operator_bitwise_or |
			TT::operator_assignment |
			TT::operator_assignment_add |
			TT::operator_assignment_subtract |
			TT::operator_assignment_multiply |
			TT::operator_assignment_divide => return err!("Unexpected binary operator with no preceding expression", tk.span.clone()),
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
				skip_newlines(&mut tokens);
				let else_statements = match tokens.peek() {
					Some(Token { variant: TokenType::keyword_else, .. }) => {
						tokens.next();
						parse_block(&mut tokens)?
					},
					_ => Vec::new(),
				};
				ASTNodeData::Block(ASTBlock::If { condition, statements, else_statements })
			},
			TT::keyword_fn => {
				tokens.next();
				let name = require_type(&mut tokens, TokenType::identifier)?;
				ASTNodeData::Block(ASTBlock::Function {
					name,
					parameters: {
						let mut parameters = vec![];
						require_type(&mut tokens, TokenType::parenthesis_open)?;
						while let Some(token) = tokens.next() {
							match token.variant {
								TT::identifier => {
									let span = token.span.clone();
									let typ = get_type_optional(&mut tokens)?.ok_or(err_!(format!("Please specify the type for the parameter {}", token.text), span))?;
									parameters.push((token, typ));
									if tokens.peek().is_some_and(|t| t.variant == TokenType::punctuation_comma) {
										tokens.next();
									}
								},
								TT::parenthesis_close => break,
								_ => return err!("Unexpected token: Expected an argument, or end of arguments", token.span),
							}
						}
						parameters
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
			TT::identifier | TT::link | TT::number | TT::string | TT::string_fragment |
			TT::operator_minus | TT::operator_not | TT::operator_increment | TT::operator_bitwise_flip |
			TT::parenthesis_open =>
				ASTNodeData::Statement(ASTStatement::Expression(get_expression(&mut tokens)?)),
		};
		statements.push(ASTNode { data: statement, span: 0..0 });
	}
	Ok(statements)
}


pub mod test_utils {
	#![allow(dead_code)]
	
	pub mod expr {

		use crate::lexer::*;
		use crate::parser::*;

		pub fn binary(left:ASTExpression, operator: Token, right: ASTExpression) -> ASTExpression {
			ASTExpression::BinaryOperator { left: Box::new(left), operator, right: Box::new(right) }
		}
		pub fn unary(operator: Token, operand: ASTExpression) -> ASTExpression {
			ASTExpression::UnaryOperator { operator, operand: Box::new(operand) }
		}
		pub fn func(function:ASTExpression, arguments: Vec<ASTExpression>) -> ASTExpression {
			ASTExpression::FunctionCall { function: Box::new(function), arguments }
		}
		pub fn template_string(strings:Vec<&str>, values: Vec<ASTExpression>) -> ASTExpression {
			let last_i = strings.len() - 1;
			ASTExpression::TemplateString {
				strings: strings.iter().enumerate().map(|(i, ref str)| Token {
					text: if i == 0 { format!("\"{str}") } else if i == last_i { format!("{str}\"") } else { str.to_string() },
					variant: if i == last_i { TokenType::string } else { TokenType::string_fragment },
					span: 0..0
				}).collect(),
				values
			}
		}
		pub fn leaf(token: Token) -> ASTExpression {
			ASTExpression::Leaf(token)
		}
		pub fn num(num: &'static str) -> ASTExpression {
			ASTExpression::Leaf(Token { text: num.to_string(), variant: TokenType::number, span: 0..0 })
		}
		/// Adds the double quotes.
		pub fn str(str: &'static str) -> ASTExpression {
			ASTExpression::Leaf(Token { text: format!("\"{str}\""), variant: TokenType::string, span: 0..0 })
		}
		pub fn ident(str: &'static str) -> ASTExpression {
			ASTExpression::Leaf(Token { text: str.to_string(), variant: TokenType::identifier, span: 0..0 })
		}
	}
	pub mod node {
		use crate::parser::*;

		pub fn expr_statement(expr: ASTExpression) -> ASTNode {
			ASTNode { data: ASTNodeData::Statement(ASTStatement::Expression(expr)), span: 0..0 }
		}
		pub fn root(statement:ASTNode) -> AST {
			ASTBlock::Root { statements: vec![ statement ] }
		}
		pub fn root_expr(expr: ASTExpression) -> Result<AST, CError> {
			Ok(root(expr_statement(expr)))
		}
	}
}

#[cfg(test)]
mod tests {
	use crate::lexer::test_utils::*;
	use crate::parser::test_utils::expr as e;
	use crate::parser::test_utils::node as n;
	use crate::parser::*;
	use pretty_assertions::assert_eq;

	#[test]
	fn parse_expr_literal_number(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.num("55"),
			]),
			n::root_expr(e::num("55"))
		);
	}
	#[test]
	fn parse_expr_literal_string(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.str("abc"),
			]),
			n::root_expr(e::str("abc"))
		);
	}
	#[test]
	fn parse_expr_literal_link(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.link("message1"),
			]),
			n::root_expr(e::leaf(b.link("message1")))
		);
	}
	#[test]
	fn parse_expr_literal_ident(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.ident("qwerty"),
			]),
			n::root_expr(e::ident("qwerty"))
		);
	}
	#[test]
	fn parse_expr_binary_simple_1(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.num("55"),
				b.add(),
				b.num("3")
			]),
			n::root_expr(
				e::binary(e::num("55"), b.add(), e::num("3"))
			)
		);
	}
	#[test]
	fn parse_expr_binary_simple_2(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.ident("x"),
				b.token("*=", TokenType::operator_assignment_multiply),
				b.num("3")
			]),
			n::root_expr(
				e::binary(e::ident("x"), b.token("*=", TokenType::operator_assignment_multiply), e::num("3"))
			)
		);
	}
	#[test]
	fn parse_expr_binary_multiple(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.ident("x"),
				b.add(),
				b.ident("y"),
				b.div(),
				b.ident("z"),
			]),
			n::root_expr(
				e::binary(e::ident("x"), b.add(), e::binary(e::ident("y"), b.div(), e::ident("z")))
			)
		);
	}
	#[test]
	fn parse_expr_binary_multiple_paren(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.popen(),
				b.ident("x"),
				b.add(),
				b.ident("y"),
				b.pclose(),
				b.div(),
				b.ident("z"),
			]),
			n::root_expr(
				e::binary(e::binary(e::ident("x"), b.add(), e::ident("y")), b.div(), e::ident("z"))
			)
		);
	}
	#[test]
	fn parse_expr_unary_simple_1(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.minus(),
				b.ident("x"),
			]),
			n::root_expr(
				e::unary(b.minus(), e::ident("x"))
			)
		);
	}
	#[test]
	fn parse_expr_unary_simple_2(){
		let mut b = TokenBuilder::new();
		let flip = b.token("~", TokenType::operator_bitwise_flip);
		assert_eq!(
			parse(vec![
				flip.clone(),
				b.ident("abc"),
			]),
			n::root_expr(
				e::unary(flip.clone(), e::ident("abc"))
			)
		);
	}
	#[test]
	fn parse_expr_function_call_1(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.ident("x"),
				b.popen(),
				b.num("3"),
				b.pclose(),
			]),
			n::root_expr(
				e::func(e::ident("x"), vec![e::num("3")])
			)
		);
	}
	#[test]
	fn parse_expr_function_call_2(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.ident("x"),
				b.popen(),
				b.num("3"),
				b.comma(),
				b.num("4"),
				b.pclose(),
			]),
			n::root_expr(
				e::func(e::ident("x"), vec![e::num("3"), e::num("4")])
			)
		);
	}
	#[test]
	fn parse_expr_function_call_3(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.popen(),
				// b.popen(),
				b.ident("x"),
				b.period(),
				b.ident("y"),
				// b.pclose(),
				b.pclose(),
				b.popen(),
				// b.popen(),
				b.num("3"),
				b.comma(),
				b.num("4"),
				// b.pclose(),
				b.pclose(),
			]),
			n::root_expr(
				e::func(e::binary(e::ident("x"), b.period(), e::ident("y")), vec![e::num("3"), e::num("4")])
			)
		);
	}
	#[test]
	fn parse_expr_function_call_4(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.popen(),
				// b.popen(),
				b.ident("x"),
				b.add(),
				b.ident("y"),
				// b.pclose(),
				b.pclose(),
				b.popen(),
				// b.popen(),
				b.num("3"),
				// b.pclose(),
				b.pclose(),
			]),
			n::root_expr(
				e::func(e::binary(e::ident("x"), b.add(), e::ident("y")), vec![e::num("3")])
			)
		);
	}

	#[test]
	fn parse_expr_template_string(){
		let mut b = TokenBuilder::new();
		assert_eq!(
			parse(vec![
				b.ident("print"),
				b.popen(),
				b.strf_start("Hello, "),
				b.interp_start(),
				b.num("2"),
				b.add(),
				b.strf_start(""),
				b.interp_start(),
				b.ident("a"),
				b.shr(),
				b.ident("b"),
				b.interp_end(),
				b.strf(""),
				b.interp_start(),
				b.strf_start(""),
				b.interp_start(),
				b.flip(),
				b.ident("x"),
				b.interp_end(),
				b.strf_end(""),
				b.interp_end(),
				b.strf_end(""),
				b.add(),
				b.num("3"),
				b.interp_end(),
				b.strf_end(" world"),
			]),
			n::root_expr(
				e::func(e::ident("print"), vec![
					e::template_string(vec!["Hello, ", " world"], vec![
						e::binary(
							e::binary(e::num("2"), b.add(), e::template_string(
								vec!["", "", ""],
								vec![
									e::binary(e::ident("a"), b.shr(), e::ident("b")),
									e::template_string(vec!["", ""], vec![
										e::unary(b.flip(), e::ident("x"))
									])
								]
							)),
							b.add(),
							e::num("3")
						)
					])
				])
			)
		);
	}

	#[test]
	fn parse_program_example_factorial(){
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
				n::expr_statement(ASTExpression::FunctionCall {
					function: Box::new(e::ident("print")),
					arguments: vec![
						e::str("Hello, world!")
					]
				}),
				n::expr_statement(ASTExpression::FunctionCall {
					function: Box::new(e::ident("printflush")),
					arguments: vec![
						e::leaf(b.link("message1"))
					]
				}),
				ASTNode {
					data: ASTNodeData::Block(ASTBlock::Function {
						name: b.ident("factorial"),
						parameters: vec![(b.ident("x"), ASTType::Literal(b.ident("num")))],
						return_type: Some(ASTType::Literal(b.ident("num"))),
						statements: vec![
							ASTNode {
								data: ASTNodeData::Block(ASTBlock::If {
									condition: e::binary(e::ident("x"), b.lt(), e::num("1")),
									statements: vec![
										ASTNode {
											data: ASTNodeData::Statement(ASTStatement::Return(Some(e::num("1")))),
											span: 0..0
										}
									],
									else_statements: vec![],
								}),
								span: 0..0,
							},
							ASTNode {
								data: ASTNodeData::Statement(ASTStatement::Return(Some(
									e::binary(
										e::ident("x"),
										b.mult(),
										ASTExpression::FunctionCall {
											function: Box::new(e::ident("factorial")),
											arguments: vec![
												e::binary(e::ident("x"), b.minus(), e::num("1"))
											]
										}
									)
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
