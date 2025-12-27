#![allow(non_camel_case_types)]

use std::fmt::Display;
use itertools::Itertools;
use crate::common::{CError, Span};
use crate::err;

#[derive(Debug, Clone)]
pub struct Token {
	pub text: String,
	pub variant: TokenType,
	pub span: Span,
}

impl PartialEq for Token {
	fn eq(&self, other: &Self) -> bool {
		//ignore span
		self.text == other.text && self.variant == other.variant
	}
}
impl Eq for Token {}

impl Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.text)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubToken {
	pub text: String,
	pub variant: SubTokenType,
	pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
	newline,
	number,
	identifier,
	string,
	/// Indicates a part of an interpolated string.
	/// example:
	/// ```
	/// "aaa${2+3}bbb"
	/// string_fragment “aaa
	/// interpolation_start ${
	/// number 2
	/// operator_add +
	/// number 3
	/// interpolation_end }
	/// string_fragment bbb”
	/// ```
	string_fragment,
	interpolation_start,
	interpolation_end,
	link,
	keyword_if,
	keyword_else,
	keyword_for,
	keyword_loop,
	keyword_while,
	keyword_fn,
	keyword_return,
	keyword_break,
	keyword_continue,
	keyword_var,
	keyword_val,
	keyword_cfg,
	
	//special operators: these have no mlog equivalent
	operator_access,
	operator_increment,
	operator_assignment,
	operator_assignment_add,
	operator_assignment_subtract,
	operator_assignment_multiply,
	operator_assignment_divide,
	//other operators are natively supported by mlog

	//Basic arithmetic
	operator_add,
	operator_minus,
	operator_multiply,
	operator_divide,
	operator_integer_divide,
	operator_modulo,
	operator_euclidian_modulo,
	operator_exponentiate,

	//Logical operations
	operator_loose_equal_to,
	operator_not_equal_to,
	operator_logical_and,
	operator_less_than,
	operator_less_than_eq,
	operator_greater_than,
	operator_greater_than_eq,
	operator_equal_to,
	operator_logical_or, //this operator has a direct mlog equivalent
	operator_not, //this operator has a direct mlog equivalent

	//Bitwise operations
	operator_shift_left,
	operator_shift_right,
	operator_shift_right_unsigned,
	operator_bitwise_or,
	operator_bitwise_and,
	operator_bitwise_xor,
	operator_bitwise_flip,

	parenthesis_open,
	parenthesis_close,
	bracket_open,
	bracket_close,
	brace_open,
	brace_close,
	punctuation_colon,
	punctuation_semicolon,
	punctuation_comma,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubTokenType {
	escape,
	numeric_fragment,
	numeric_e,
	numeric_x,
	numeric_b,
	numeric_o,
	whitespace,
	newline,
	word,
	comment_singleline,
	comment_start,
	comment_end,

	//special operators: these have no mlog equivalent
	operator_access,
	operator_increment,
	operator_assignment,
	operator_assignment_add,
	operator_assignment_subtract,
	operator_assignment_multiply,
	operator_assignment_divide,
	//other operators are natively supported by mlog

	//Basic arithmetic
	operator_add,
	operator_minus, //unary
	operator_multiply,
	operator_divide,
	operator_integer_divide,
	operator_modulo,
	operator_euclidian_modulo,
	operator_exponentiate,

	//Logical operations
	operator_loose_equal_to,
	operator_not_equal_to,
	operator_logical_and,
	operator_less_than,
	operator_less_than_eq,
	operator_greater_than,
	operator_greater_than_eq,
	operator_equal_to,
	operator_logical_or, //this operator has a direct mlog equivalent
	operator_not, //this operator has a direct mlog equivalent //unary

	//Bitwise operations
	operator_shift_left,
	operator_shift_right,
	operator_shift_right_unsigned,
	operator_bitwise_or,
	operator_bitwise_and,
	operator_bitwise_xor,
	operator_bitwise_flip, //unary

	parenthesis_open,
	parenthesis_close,
	bracket_open,
	bracket_close,
	brace_open,
	brace_close,
	quote_double,
	quote_single,
	quote_backtick,
	punctuation_colon,
	punctuation_semicolon,
	punctuation_comma,
	punctuation_dollar,
}

fn get_sub_tokens(input:&str) -> Result<Vec<SubToken>, CError> {
	let mut out: Vec<SubToken> = vec![];
	let mut chars = input.chars().enumerate().peekable();
	while let Some((i, char)) = chars.next() {
		use SubTokenType as ST;
		let (len, variant) = match char {
			'\\' => (1, ST::escape),
			'+' => match chars.peek() {
				Some((_, '+')) => {
					chars.next();
					(2, ST::operator_increment)
				}
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_assignment_add)
				}
				_ => (1, ST::operator_add),
			},
			'-' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_assignment_subtract)
				}
				_ => (1, ST::operator_minus),
			}
			'*' => match chars.peek() {
				Some((_, '/')) => {
					chars.next();
					(2, ST::comment_end)
				}
				Some((_, '*')) => {
					chars.next();
					(2, ST::operator_exponentiate)
				}
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_assignment_multiply)
				}
				_ => (1, ST::operator_multiply)
			},
			'%' => match chars.peek() {
				Some((_, '%')) => {
					chars.next();
					(2, ST::operator_euclidian_modulo)
				}
				_ => (1, ST::operator_modulo)
			},
			'(' => (1, ST::parenthesis_open),
			')' => (1, ST::parenthesis_close),
			'[' => (1, ST::bracket_open),
			']' => (1, ST::bracket_close),
			'{' => (1, ST::brace_open),
			'}' => (1, ST::brace_close),
			'\'' => (1, ST::quote_single),
			'"' => (1, ST::quote_double),
			'`' => (1, ST::quote_backtick),
			':' => (1, ST::punctuation_colon),
			';' => (1, ST::punctuation_semicolon),
			'.' => (1, ST::operator_access),
			',' => (1, ST::punctuation_comma),
			' ' => (1, ST::whitespace),
			'\n' => (1, ST::newline),
			'/' => match chars.peek() {
				Some((_, '/')) => {
					chars.next();
					(2, ST::comment_singleline)
				},
				Some((_, '*')) => {
					chars.next();
					(2, ST::comment_start)
				},
				Some((_, '.')) => {
					chars.next();
					(2, ST::operator_integer_divide)
				},
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_assignment_divide)
				}
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
				Some((_, '>')) => {
					chars.next();
					match chars.peek() {
						Some((_, '>')) => {
							chars.next();
							(3, ST::operator_shift_right_unsigned)
						}
						_ => (2, ST::operator_shift_right)
					}
				},
				_ => (1, ST::operator_greater_than),
			},
			'<' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_less_than_eq)
				},
				Some((_, '<')) => {
					chars.next();
					(2, ST::operator_shift_left)
				},
				_ => (1, ST::operator_less_than),
			},
			'!' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_not_equal_to)
				},
				_ => (1, ST::operator_not),
			},
			'~' => match chars.peek() {
				Some((_, '=')) => {
					chars.next();
					(2, ST::operator_loose_equal_to)
				},
				_ => (1, ST::operator_bitwise_flip),
			},
			'&' => match chars.peek() {
				Some((_, '&')) => {
					chars.next();
					(2, ST::operator_logical_and)
				},
				_ => (1, ST::operator_bitwise_and),
			},
			'|' => match chars.peek() {
				Some((_, '|')) => {
					chars.next();
					(2, ST::operator_logical_or)
				},
				_ => (1, ST::operator_bitwise_or),
			},
			'^' => (1, ST::operator_bitwise_xor),
			'e' if out.last().is_some_and(|s| s.variant == ST::numeric_fragment) => (1, ST::numeric_e),
			'x' if out.last().is_some_and(|s| s.variant == ST::numeric_fragment) => (1, ST::numeric_x),
			'b' if out.last().is_some_and(|s| s.variant == ST::numeric_fragment) => (1, ST::numeric_b),
			'o' if out.last().is_some_and(|s| s.variant == ST::numeric_fragment) => (1, ST::numeric_o),
			'$' => (1, ST::punctuation_dollar),
			'a'..='z' | 'A'..='Z' | '_' | '@' => (chars.peeking_take_while(|(_, c)| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '@')).count() + 1, ST::word),
			'0'..='9' => (chars.peeking_take_while(|(_, c)| c.is_numeric()).count() + 1, ST::numeric_fragment),
			_ => return err!(format!("Invalid char {char}"), i..i+1),
		};
		let span = i..i+len;
		out.push(SubToken { text: input[span.clone()].to_string(), variant, span });
	}
	Ok(out)
}

pub fn lexer(input:&str) -> Result<Vec<Token>, CError> {
	let mut out = vec![];
	let mut sub_tokens = get_sub_tokens(input)?.into_iter().peekable();

	//Some state management is necessary to parse interpolated strings
	enum BraceType {
		StringInterpolationSingle,
		StringInterpolationDouble,
		Unknown,
	}
	let mut brace_nest_stack: Vec<(BraceType, Span)> = vec![];
	macro_rules! process_string {
		($st_variant:expr, $st_span:expr, $st_offset:literal, $sub_tokens:expr, $process_loop:lifetime) => {
			while let Some(st2) = $sub_tokens.next() {
				if st2.variant == ST::escape { 
					$sub_tokens.next(); //if this is none, that's fine, the loop will exit and it will be treated as an unterminated string literal
				}
				if $st_variant != ST::quote_backtick && st2.variant == ST::punctuation_dollar &&
				matches!($sub_tokens.peek(), Some(SubToken { variant: ST::brace_open, .. })) {
					let string_fragment_span = ($st_span.start + $st_offset)..st2.span.start;
					out.push(Token {
						text: input[string_fragment_span.clone()].to_string(),
						variant: TokenType::string_fragment,
						span: string_fragment_span,
					});
					let brace = $sub_tokens.next().unwrap();
					let token_span = st2.span.start..brace.span.end;
					brace_nest_stack.push((match $st_variant {
						ST::quote_single => BraceType::StringInterpolationSingle,
						ST::quote_double => BraceType::StringInterpolationDouble,
						_ => unreachable!(),
					}, brace.span));
					out.push(Token {
						text: input[token_span.clone()].to_string(),
						variant: TokenType::interpolation_start,
						span: token_span
					});
					continue $process_loop;
				}
				if st2.variant == $st_variant {
					let span = ($st_span.start + $st_offset)..st2.span.end;
					out.push(Token {
						text: input[span.clone()].to_string(),
						variant: match $st_variant {
							ST::quote_backtick => TokenType::link,
							_ => TokenType::string,
						}, span });
					continue $process_loop;
				}
			}
			let whole_string = $st_span.start..input.len();
			return err!("Unterminated string literal", $st_span, whole_string);
		}
	}

	'process_loop:
	while let Some(st) = sub_tokens.next() {
		use SubTokenType as ST;
		out.push(Token { text: st.text.clone(), span: st.span.clone(), variant: match st.variant {
			
			ST::operator_assignment => TokenType::operator_assignment,
			ST::operator_assignment_add => TokenType::operator_assignment_add,
			ST::operator_assignment_subtract => TokenType::operator_assignment_subtract,
			ST::operator_assignment_multiply => TokenType::operator_assignment_multiply,
			ST::operator_assignment_divide => TokenType::operator_assignment_divide,
			ST::operator_access => TokenType::operator_access,
			ST::operator_increment => TokenType::operator_increment,
			ST::operator_add => TokenType::operator_add,
			ST::operator_minus => TokenType::operator_minus,
			ST::operator_multiply => TokenType::operator_multiply,
			ST::operator_divide => TokenType::operator_divide,
			ST::operator_integer_divide => TokenType::operator_integer_divide,
			ST::operator_modulo => TokenType::operator_modulo,
			ST::operator_euclidian_modulo => TokenType::operator_euclidian_modulo,
			ST::operator_exponentiate => TokenType::operator_exponentiate,
			ST::operator_loose_equal_to => TokenType::operator_loose_equal_to,
			ST::operator_not_equal_to => TokenType::operator_not_equal_to,
			ST::operator_logical_and => TokenType::operator_logical_and,
			ST::operator_less_than => TokenType::operator_less_than,
			ST::operator_less_than_eq => TokenType::operator_less_than_eq,
			ST::operator_greater_than => TokenType::operator_greater_than,
			ST::operator_greater_than_eq => TokenType::operator_greater_than_eq,
			ST::operator_equal_to => TokenType::operator_equal_to,
			ST::operator_logical_or => TokenType::operator_logical_or,
			ST::operator_not => TokenType::operator_not,
			ST::operator_shift_left => TokenType::operator_shift_left,
			ST::operator_shift_right => TokenType::operator_shift_right,
			ST::operator_shift_right_unsigned => TokenType::operator_shift_right_unsigned,
			ST::operator_bitwise_or => TokenType::operator_bitwise_or,
			ST::operator_bitwise_and => TokenType::operator_bitwise_and,
			ST::operator_bitwise_xor => TokenType::operator_bitwise_xor,
			ST::operator_bitwise_flip => TokenType::operator_bitwise_flip,

			ST::parenthesis_open => TokenType::parenthesis_open,
			ST::parenthesis_close => TokenType::parenthesis_close,
			ST::bracket_open => TokenType::bracket_open,
			ST::bracket_close => TokenType::bracket_close,
			ST::brace_open => {
				brace_nest_stack.push((BraceType::Unknown, st.span.clone()));
				TokenType::brace_open
			},
			ST::brace_close => match brace_nest_stack.pop() {
				Some((brace, _)) => match brace {
					BraceType::Unknown => TokenType::brace_close,
					BraceType::StringInterpolationDouble => {
						out.push(Token { text: st.text, variant: TokenType::interpolation_end, span: st.span.clone() });
						process_string!(SubTokenType::quote_double, st.span, 1, sub_tokens, 'process_loop);
					},
					BraceType::StringInterpolationSingle => {
						out.push(Token { text: st.text, variant: TokenType::interpolation_end, span: st.span.clone() });
						process_string!(SubTokenType::quote_single, st.span, 1, sub_tokens, 'process_loop);
					}
				},
				None => err!("Unmatched }", st.span)?,
			},
			ST::punctuation_colon => TokenType::punctuation_colon,
			ST::punctuation_semicolon => TokenType::punctuation_semicolon,
			ST::punctuation_comma => TokenType::punctuation_comma,
			ST::newline => TokenType::newline,
			ST::escape => return err!("Unexpected escape character", st.span),
			ST::punctuation_dollar => match sub_tokens.peek() {
				Some(SubToken { variant: ST::word, span: span2, .. }) => {
					let span = st.span.start..span2.end;
					out.push(Token { text: input[span.clone()].to_string(), variant: TokenType::identifier, span });
					sub_tokens.next();
					continue
				},
				_ => TokenType::identifier,
			},
			ST::numeric_fragment => match sub_tokens.peek() {
				Some(SubToken { variant: ST::operator_access, .. }) => {
					sub_tokens.next();
					match sub_tokens.peek() {
						Some(SubToken { variant: ST::numeric_fragment, span: span2, .. }) => {
							let span = st.span.start..span2.end;
							out.push(Token { text: input[span.clone()].to_string(), variant: TokenType::number, span });
							sub_tokens.next();
							continue;
						},
						_ => return err!("Invalid numeric literal: Expected more digits after the decimal point", st.span)
					}
				},
				Some(SubToken { variant: ST::numeric_e | ST::numeric_b | ST::numeric_o | ST::numeric_x, .. }) => {
					sub_tokens.next();
					match sub_tokens.peek() {
						Some(SubToken { variant: ST::numeric_fragment, span: span2, .. }) => {
							let span = st.span.start..span2.end;
							out.push(Token { text: input[span.clone()].to_string(), variant: TokenType::number, span });
							sub_tokens.next();
							continue;
						},
						Some(x) => return err!(format!("Invalid numeric literal: Expected more digits after the character, got {x:?}"), x.span.clone()),
						None => return err!(format!("Invalid numeric literal: Expected more digits after the character, got end of file"), st.span)
					}
				},
				_ => TokenType::number
			},
			ST::numeric_e | ST::numeric_b | ST::numeric_o | ST::numeric_x => return err!("Invalid numeric literal: numeric literals can only contain one non-numeric character, like b e o x", st.span),
			ST::whitespace => continue,
			ST::word => match &st.text[..] {
				"if" => TokenType::keyword_if,
				"else" => TokenType::keyword_else,
				"for" => TokenType::keyword_for,
				"loop" => TokenType::keyword_loop,
				"while" => TokenType::keyword_while,
				"fn" => TokenType::keyword_fn,
				"return" => TokenType::keyword_return,
				"break" => TokenType::keyword_break,
				"continue" => TokenType::keyword_continue,
				"var" => TokenType::keyword_var,
				"val" => TokenType::keyword_val,
				"cfg" => TokenType::keyword_cfg,
				_ => TokenType::identifier,
			},
			ST::comment_singleline => {
				sub_tokens.peeking_take_while(|st| st.variant != ST::newline).count();
				continue;
			},
			ST::comment_start => {
				sub_tokens.peeking_take_while(|st| st.variant != ST::comment_end).count();
				match sub_tokens.next() {
					None => {
						let whole_comment = st.span.start..input.len();
						return err!("Unterminated multiline comment", st.span, whole_comment);
					},
					_ => continue,
				}
			},
			ST::comment_end => return err!("Unexpected token, no multiline comment to end", st.span),
			ST::quote_double | ST::quote_single | ST::quote_backtick => {
				process_string!(st.variant, st.span, 0, sub_tokens, 'process_loop);
			},
		}});
	}
	if let Some(unmatched) = brace_nest_stack.pop() {
		return err!("Unmatched {", unmatched.1);
	}
	Ok(out)
}

pub mod test_utils {
	#![allow(dead_code)]
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
		pub fn interp_start(&mut self) -> Token { self.token("${", TokenType::interpolation_start) }
		pub fn interp_end(&mut self) -> Token { self.token("}", TokenType::interpolation_end) }
		pub fn popen(&mut self) -> Token { self.token("(", TokenType::parenthesis_open) }
		pub fn pclose(&mut self) -> Token { self.token(")", TokenType::parenthesis_close) }
		pub fn sopen(&mut self) -> Token { self.token("[", TokenType::bracket_open) }
		pub fn sclose(&mut self) -> Token { self.token("]", TokenType::bracket_close) }
		pub fn bopen(&mut self) -> Token { self.token("{", TokenType::brace_open) }
		pub fn bclose(&mut self) -> Token { self.token("}", TokenType::brace_close) }
		pub fn colon(&mut self) -> Token { self.token(":", TokenType::punctuation_colon) }
		pub fn comma(&mut self) -> Token { self.token(",", TokenType::punctuation_comma) }
		pub fn semicolon(&mut self) -> Token { self.token(";", TokenType::punctuation_semicolon) }
		pub fn period(&mut self) -> Token { self.token(".", TokenType::operator_access) }
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
		pub fn flip(&mut self) -> Token { self.token("~", TokenType::operator_bitwise_flip) }
		pub fn mult(&mut self) -> Token { self.token("*", TokenType::operator_multiply) }
		pub fn div(&mut self) -> Token { self.token("/", TokenType::operator_divide) }
		pub fn modulo(&mut self) -> Token { self.token("%", TokenType::operator_modulo) }
		pub fn shl(&mut self) -> Token { self.token("<<", TokenType::operator_shift_left) }
		pub fn shr(&mut self) -> Token { self.token(">>", TokenType::operator_shift_right) }
		pub fn shru(&mut self) -> Token { self.token(">>>", TokenType::operator_shift_right) }
		pub fn num(&mut self, number:&str) -> Token { self.token(number, TokenType::number) }
		pub fn ident(&mut self, ident:impl Into<String>) -> Token { self.token(ident, TokenType::identifier) }
		/// Adds the double quotes.
		pub fn str(&mut self, ident:impl Into<String>) -> Token { self.token(format!("\"{}\"", ident.into()), TokenType::string) }
		pub fn strf(&mut self, ident:impl Into<String>) -> Token { self.token(ident.into(), TokenType::string_fragment) }
		/// Adds a starting double quote.
		pub fn strf_start(&mut self, ident:impl Into<String>) -> Token { self.token(format!("\"{}", ident.into()), TokenType::string_fragment) }
		/// Adds an ending double quote.
		pub fn strf_end(&mut self, ident:impl Into<String>) -> Token { self.token(format!("{}\"", ident.into()), TokenType::string) }
		/// Adds the backticks.
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
			Ok(vec![
				SubToken { text: String::from("1"), variant: ST::numeric_fragment, span: 0..1 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 1..2 },
				SubToken { text: String::from("+"), variant: ST::operator_add, span: 2..3 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 3..4 },
				SubToken { text: String::from("2"), variant: ST::numeric_fragment, span: 4..5 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 5..6 },
				SubToken { text: String::from("-"), variant: ST::operator_minus, span: 6..7 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 7..8 },
				SubToken { text: String::from("3"), variant: ST::numeric_fragment, span: 8..9 },
			])
		);
		assert_eq!(
			get_sub_tokens(r#"print("Hello\" world.");"#),
			Ok(vec![
				SubToken { text: String::from("print"), variant: ST::word, span: 0..5 },
				SubToken { text: String::from("("), variant: ST::parenthesis_open, span: 5..6 },
				SubToken { text: String::from("\""), variant: ST::quote_double, span: 6..7 },
				SubToken { text: String::from("Hello"), variant: ST::word, span: 7..12 },
				SubToken { text: String::from("\\"), variant: ST::escape, span: 12..13 },
				SubToken { text: String::from("\""), variant: ST::quote_double, span: 13..14 },
				SubToken { text: String::from(" "), variant: ST::whitespace, span: 14..15 },
				SubToken { text: String::from("world"), variant: ST::word, span: 15..20 },
				SubToken { text: String::from("."), variant: ST::operator_access, span: 20..21 },
				SubToken { text: String::from("\""), variant: ST::quote_double, span: 21..22 },
				SubToken { text: String::from(")"), variant: ST::parenthesis_close, span: 22..23 },
				SubToken { text: String::from(";"), variant: ST::punctuation_semicolon, span: 23..24 },
			])
		);
	}

	#[test]
	fn lexer_test(){
		assert_eq!(
			lexer("1 + 2 - 3.45"),
			Ok(vec![
				Token { text: String::from("1"), variant: TT::number, span: 0..1 },
				Token { text: String::from("+"), variant: TT::operator_add, span: 2..3 },
				Token { text: String::from("2"), variant: TT::number, span: 4..5 },
				Token { text: String::from("-"), variant: TT::operator_minus, span: 6..7 },
				Token { text: String::from("3.45"), variant: TT::number, span: 8..12 },
			])
		);
		assert_eq!(
			lexer(r#"print("Hello\" world.");"#),
			Ok(vec![
				Token { text: String::from("print"), variant: TT::identifier, span: 0..5 },
				Token { text: String::from("("), variant: TT::parenthesis_open, span: 5..6 },
				Token { text: String::from(r#""Hello\" world.""#), variant: TT::string, span: 6..22 },
				Token { text: String::from(")"), variant: TT::parenthesis_close, span: 22..23 },
				Token { text: String::from(";"), variant: TT::punctuation_semicolon, span: 23..24 },
			])
		);
		assert_eq!(
			lexer("1.2 + 3 // this 'is a comment+\n4 - 5"),
			Ok(vec![
				Token { text: String::from("1.2"), variant: TT::number, span: 0..3 },
				Token { text: String::from("+"), variant: TT::operator_add, span: 4..5 },
				Token { text: String::from("3"), variant: TT::number, span: 6..7 },
				Token { text: String::from("\n"), variant: TT::newline, span: 30..31 },
				Token { text: String::from("4"), variant: TT::number, span: 31..32 },
				Token { text: String::from("-"), variant: TT::operator_minus, span: 33..34 },
				Token { text: String::from("5"), variant: TT::number, span: 35..36 },
			])
		);
	}
}
