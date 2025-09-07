#![allow(non_camel_case_types)]

use std::ops::Range;
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
	pub text: String,
	pub variant: TokenType,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubToken {
	pub text: String,
	pub variant: SubTokenType,
	pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
	newline,
	number,
	identifier,
	string,
	link,
	keyword_if,
	keyword_for,
	keyword_loop,
	keyword_fn,
	keyword_return,
	keyword_var,
	keyword_val,
	keyword_cfg,
	operator_assignment,
	operator_equal_to,
	operator_loose_equal_to,
	operator_not_equal_to,
	operator_greater_than,
	operator_less_than,
	operator_greater_than_eq,
	operator_less_than_eq,
	operator_and,
	operator_or,
	operator_add,
	operator_minus,
	operator_multiply,
	operator_divide,
	operator_modulo,
	parenthesis_open,
	parenthesis_close,
	brace_open,
	brace_close,
	punctuation_colon,
	punctuation_semicolon,
	punctuation_period,
	punctuation_comma,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SubTokenType {
	escape,
	numeric_fragment,
	whitespace,
	newline,
	word,
	comment_singleline,
	operator_assignment,
	operator_equal_to,
	operator_loose_equal_to,
	operator_not_equal_to,
	operator_greater_than,
	operator_less_than,
	operator_greater_than_eq,
	operator_less_than_eq,
	operator_and,
	operator_or,
	operator_add,
	operator_minus,
	operator_multiply,
	operator_divide,
	operator_modulo,
	parenthesis_open,
	parenthesis_close,
	brace_open,
	brace_close,
	quote_double,
	quote_single,
	quote_backtick,
	punctuation_colon,
	punctuation_semicolon,
	punctuation_period,
	punctuation_comma,
}

pub type Span = Range<usize>;

fn get_sub_tokens(input:&str) -> Vec<SubToken> {
	let mut out = vec![];
	let mut chars = input.chars().enumerate().peekable();
	while let Some((i, char)) = chars.next() {
		use SubTokenType as ST;
		let (len, variant) = match char {
			'\\' => (1, ST::escape),
			'+' => (1, ST::operator_add),
			'-' => (1, ST::operator_minus),
			'*' => (1, ST::operator_multiply),
			'%' => (1, ST::operator_modulo),
			'(' => (1, ST::parenthesis_open),
			')' => (1, ST::parenthesis_close),
			'{' => (1, ST::brace_open),
			'}' => (1, ST::brace_close),
			'\'' => (1, ST::quote_single),
			'"' => (1, ST::quote_double),
			'`' => (1, ST::quote_backtick),
			':' => (1, ST::punctuation_colon),
			';' => (1, ST::punctuation_semicolon),
			'.' => (1, ST::punctuation_period),
			' ' => (1, ST::whitespace),
			'\n' => (1, ST::newline),
			'/' => match chars.peek() {
				Some((_, '/')) => {
					chars.next();
					(2, ST::comment_singleline)
				},
				_ => (1, ST::operator_divide),
			},
			'=' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_equal_to)
				},
				_ => (1, ST::operator_assignment),
			},
			'>' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_greater_than_eq)
				},
				_ => (1, ST::operator_greater_than),
			},
			'<' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_less_than_eq)
				},
				_ => (1, ST::operator_less_than),
			},
			'!' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_not_equal_to)
				},
				_ => panic!("Invalid char !")
			},
			'~' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_loose_equal_to)
				},
				_ => panic!("Invalid char ~")
			},
			'&' => match chars.peek() {
				Some((_, '&')) => {
					chars.next();
					(2, ST::operator_and)
				},
				_ => panic!("bitwise and is unimplemented"),
			},
			'|' => match chars.peek() {
				Some((_, '|')) => {
					chars.next();
					(2, ST::operator_or)
				},
				_ => panic!("bitwise or is unimplemented"),
			},
			'a'..='z' | 'A'..='Z' | '_' | '@' => (chars.peeking_take_while(|(_, c)| c.is_alphanumeric() || *c == '_' || *c == '@').count() + 1, ST::word),
			'0'..='9' => (chars.peeking_take_while(|(_, c)| c.is_numeric()).count() + 1, ST::numeric_fragment),
			_ => panic!("Invalid char {char}")
		};
		let span = i..i+len;
		out.push(SubToken { text: input[span.clone()].to_string(), variant, span });
	}
	out
}

pub fn lexer(input:&str) -> Vec<Token> {
	let mut out = vec![];
	let mut sub_tokens = get_sub_tokens(input).into_iter().peekable();
	'process_loop:
	while let Some(st) = sub_tokens.next() {
		use SubTokenType as ST;
		out.push(Token { text: st.text.clone(), span: st.span.clone(), variant: match st.variant {
			ST::operator_assignment => TokenType::operator_assignment,
			ST::operator_equal_to => TokenType::operator_equal_to,
			ST::operator_loose_equal_to => TokenType::operator_loose_equal_to,
			ST::operator_not_equal_to => TokenType::operator_not_equal_to,
			ST::operator_greater_than => TokenType::operator_greater_than,
			ST::operator_less_than => TokenType::operator_less_than,
			ST::operator_greater_than_eq => TokenType::operator_greater_than_eq,
			ST::operator_less_than_eq => TokenType::operator_less_than_eq,
			ST::operator_and => TokenType::operator_and,
			ST::operator_or => TokenType::operator_or,
			ST::operator_add => TokenType::operator_add,
			ST::operator_minus => TokenType::operator_minus,
			ST::operator_multiply => TokenType::operator_multiply,
			ST::operator_divide => TokenType::operator_divide,
			ST::operator_modulo => TokenType::operator_modulo,
			ST::parenthesis_open => TokenType::parenthesis_open,
			ST::parenthesis_close => TokenType::parenthesis_close,
			ST::brace_open => TokenType::brace_open,
			ST::brace_close => TokenType::brace_close,
			ST::punctuation_colon => TokenType::punctuation_colon,
			ST::punctuation_semicolon => TokenType::punctuation_semicolon,
			ST::punctuation_period => TokenType::punctuation_period,
			ST::punctuation_comma => TokenType::punctuation_comma,
			ST::newline => TokenType::newline,
			ST::escape => panic!("Unexpected escape character"),
			ST::numeric_fragment => match sub_tokens.peek() {
				Some(SubToken { variant: ST::punctuation_period, .. }) => {
					sub_tokens.next();
					match sub_tokens.peek() {
						Some(SubToken { variant: ST::numeric_fragment, span: span2, .. }) => {
							let span = st.span.start..span2.end;
							out.push(Token { text: input[span.clone()].to_string(), variant: TokenType::number, span });
							sub_tokens.next();
							continue;
						},
						_ => panic!("Invalid numeric literal: Expected more digits after the decimal point")
					}
				},
				_ => TokenType::number
			},
			ST::whitespace => continue,
			ST::word => match &st.text[..] {
				"if" => TokenType::keyword_if,
				"for" => TokenType::keyword_for,
				"loop" => TokenType::keyword_loop,
				"fn" => TokenType::keyword_fn,
				"return" => TokenType::keyword_return,
				"var" => TokenType::keyword_var,
				"val" => TokenType::keyword_val,
				"cfg" => TokenType::keyword_cfg,
				_ => TokenType::identifier,
			},
			ST::comment_singleline => {
				sub_tokens.peeking_take_while(|st| st.variant != ST::newline).count();
				continue;
			},
			ST::quote_double | ST::quote_single | ST::quote_backtick => {
				while let Some(st2) = sub_tokens.next() {
					if st2.variant == ST::escape { 
						sub_tokens.next(); //if this is none, that's fine, the loop will exit and it will be treated as an unterminated string literal
					}
					if st2.variant == st.variant {
						let span = st.span.start..st2.span.end;
						out.push(Token {
							text: input[span.clone()].to_string(),
							variant: match st.variant {
								ST::quote_backtick => TokenType::link,
								_ => TokenType::string,
							}, span });
						continue 'process_loop;
					}
				}
				panic!("Unterminated string literal");
			},
		}});
	}
	out
}

pub mod test_utils {
	use super::*;

	pub struct TokenBuilder(usize);
	impl TokenBuilder {
		pub fn new() -> Self {
			TokenBuilder(0)
		}
		pub fn token(&mut self, text: impl Into<String>, variant: TokenType) -> Token {
			let text = text.into();
			let start = self.0;
			let end = self.0 + text.len();
			self.0 = end;
			Token { span: start..end, text, variant }
		}
		pub fn token_last(&mut self, text: impl Into<String>, variant: TokenType) -> Token {
			let result = self.token(text, variant);
			self.0 = 0;
			result
		}
		pub fn newline(&mut self) -> Token { self.token("\n", TokenType::newline) }
		pub fn newline_last(&mut self) -> Token { self.token_last("\n", TokenType::newline) }
		pub fn kif(&mut self) -> Token { self.token("if", TokenType::keyword_if) }
		pub fn kfor(&mut self) -> Token { self.token("for", TokenType::keyword_for) }
		pub fn kloop(&mut self) -> Token { self.token("loop", TokenType::keyword_loop) }
		pub fn kfn(&mut self) -> Token { self.token("fn", TokenType::keyword_fn) }
		pub fn kreturn(&mut self) -> Token { self.token("return", TokenType::keyword_return) }
		pub fn kvar(&mut self) -> Token { self.token("var", TokenType::keyword_var) }
		pub fn kval(&mut self) -> Token { self.token("val", TokenType::keyword_val) }
		pub fn kcfg(&mut self) -> Token { self.token("cfg", TokenType::keyword_cfg) }
		pub fn popen(&mut self) -> Token { self.token("(", TokenType::parenthesis_open) }
		pub fn pclose(&mut self) -> Token { self.token(")", TokenType::parenthesis_close) }
		pub fn bopen(&mut self) -> Token { self.token("{", TokenType::brace_open) }
		pub fn bclose(&mut self) -> Token { self.token("}", TokenType::brace_close) }
		pub fn colon(&mut self) -> Token { self.token(":", TokenType::punctuation_colon) }
		pub fn semicolon(&mut self) -> Token { self.token(";", TokenType::punctuation_semicolon) }
		pub fn period(&mut self) -> Token { self.token(".", TokenType::punctuation_period) }
		pub fn assign(&mut self) -> Token { self.token("=", TokenType::operator_assignment) }
		pub fn eq(&mut self) -> Token { self.token("==", TokenType::operator_equal_to) }
		pub fn loose_eq(&mut self) -> Token { self.token("~=", TokenType::operator_loose_equal_to) }
		pub fn ne(&mut self) -> Token { self.token("!=", TokenType::operator_not_equal_to) }
		pub fn gt(&mut self) -> Token { self.token(">", TokenType::operator_greater_than) }
		pub fn lt(&mut self) -> Token { self.token("<", TokenType::operator_less_than) }
		pub fn ge(&mut self) -> Token { self.token(">=", TokenType::operator_greater_than_eq) }
		pub fn le(&mut self) -> Token { self.token("<=", TokenType::operator_less_than_eq) }
		pub fn add(&mut self) -> Token { self.token("+", TokenType::operator_add) }
		pub fn minus(&mut self) -> Token { self.token("-", TokenType::operator_minus) }
		pub fn mult(&mut self) -> Token { self.token("*", TokenType::operator_multiply) }
		pub fn div(&mut self) -> Token { self.token("/", TokenType::operator_divide) }
		pub fn modulo(&mut self) -> Token { self.token("%", TokenType::operator_modulo) }
		pub fn num(&mut self, number:&str) -> Token { self.token(number, TokenType::number) }
		pub fn ident(&mut self, ident:impl Into<String>) -> Token { self.token(ident, TokenType::identifier) }
		pub fn str(&mut self, ident:impl Into<String>) -> Token { self.token(format!("\"{}\"", ident.into()), TokenType::string) }
		pub fn link(&mut self, ident:impl Into<String>) -> Token { self.token(format!("`{}`", ident.into()), TokenType::string) }
	}
}

#[cfg(test)]
mod tests {
	use crate::lexer::*;
	use crate::lexer::SubTokenType as ST;
	use crate::lexer::TokenType as TT;
	use pretty_assertions::assert_eq;

	#[test]
	fn get_sub_tokens_test(){
		assert_eq!(
			get_sub_tokens("1 + 2 - 3"),
			vec![
				SubToken { text: String::from("1"), variant: ST::numeric_fragment, span: 0..1 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 1..2 },
				SubToken { text: String::from("+"), variant: ST::operator_add, span: 2..3 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 3..4 },
				SubToken { text: String::from("2"), variant: ST::numeric_fragment, span: 4..5 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 5..6 },
				SubToken { text: String::from("-"), variant: ST::operator_minus, span: 6..7 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 7..8 },
				SubToken { text: String::from("3"), variant: ST::numeric_fragment, span: 8..9 },
			]
		);
		assert_eq!(
			get_sub_tokens(r#"print("Hello\" world.");"#),
			vec![
				SubToken { text: String::from("print"), variant: ST::word, span: 0..5 },
				SubToken { text: String::from("("), variant: ST::parenthesis_open, span: 5..6 },
				SubToken { text: String::from("\""), variant: ST::quote_double, span: 6..7 },
				SubToken { text: String::from("Hello"), variant: ST::word, span: 7..12 },
				SubToken { text: String::from("\\"), variant: ST::escape, span: 12..13 },
				SubToken { text: String::from("\""), variant: ST::quote_double, span: 13..14 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 14..15 },
				SubToken { text: String::from("world"), variant: ST::word, span: 15..20 },
				SubToken { text: String::from("."), variant: ST::punctuation_period, span: 20..21 },
				SubToken { text: String::from("\""), variant: ST::quote_double, span: 21..22 },
				SubToken { text: String::from(")"), variant: ST::parenthesis_close, span: 22..23 },
				SubToken { text: String::from(";"), variant: ST::punctuation_semicolon, span: 23..24 },
			]
		);
	}

	#[test]
	fn lexer_test(){
		assert_eq!(
			lexer("1 + 2 - 3.45"),
			vec![
				Token { text: String::from("1"), variant: TT::number, span: 0..1 },
				Token { text: String::from("+"), variant: TT::operator_add, span: 2..3 },
				Token { text: String::from("2"), variant: TT::number, span: 4..5 },
				Token { text: String::from("-"), variant: TT::operator_minus, span: 6..7 },
				Token { text: String::from("3.45"), variant: TT::number, span: 8..12 },
			]
		);
		assert_eq!(
			lexer(r#"print("Hello\" world.");"#),
			vec![
				Token { text: String::from("print"), variant: TT::identifier, span: 0..5 },
				Token { text: String::from("("), variant: TT::parenthesis_open, span: 5..6 },
				Token { text: String::from(r#""Hello\" world.""#), variant: TT::string, span: 6..22 },
				Token { text: String::from(")"), variant: TT::parenthesis_close, span: 22..23 },
				Token { text: String::from(";"), variant: TT::punctuation_semicolon, span: 23..24 },
			]
		);
		assert_eq!(
			lexer("1.2 + 3 // this 'is a comment+\n4 - 5"),
			vec![
				Token { text: String::from("1.2"), variant: TT::number, span: 0..3 },
				Token { text: String::from("+"), variant: TT::operator_add, span: 4..5 },
				Token { text: String::from("3"), variant: TT::number, span: 6..7 },
				Token { text: String::from("\n"), variant: TT::newline, span: 30..31 },
				Token { text: String::from("4"), variant: TT::number, span: 31..32 },
				Token { text: String::from("-"), variant: TT::operator_minus, span: 33..34 },
				Token { text: String::from("5"), variant: TT::number, span: 35..36 },
			]
		);
	}
}
