use crate::{common::CError, lexer::{Token, TokenType}, parser::{AST, ASTBlock, ASTExpression}};


pub fn compile(program: AST) -> Result<String, CError> {
  match program {
    ASTBlock::Root { statements } => {
      todo!()
    },
    _ => panic!("program must be of type Root"),
  }
}

pub struct IdentGenerator {
  counter: u32,
}
impl IdentGenerator {
  pub fn new() -> Self {
    Self { counter: 0 }
  }
  fn next_ident(&mut self) -> String {
    self.counter += 1;
    return format!("mlogs_{}", self.counter);
  }
}

pub enum OutputName {
  Specified(String),
  Any,
  None
}

fn compile_operator(operator:TokenType) -> &'static str {
  use TokenType as TT;
  match operator {
    TT::operator_exponentiate => "pow",
    TT::operator_multiply => "mul",
    TT::operator_divide => "div",
    TT::operator_integer_divide => "idiv",
    TT::operator_modulo => "mod",
    TT::operator_euclidian_modulo => "emod",
    TT::operator_add => "add",
    TT::operator_minus => "sub",
    TT::operator_shift_left => "shl",
    TT::operator_shift_right => "shr",
    TT::operator_shift_right_unsigned => "ushr",
    TT::operator_greater_than => "greaterThan",
    TT::operator_less_than => "lessThan",
    TT::operator_greater_than_eq => "greaterThanEq",
    TT::operator_less_than_eq => "lessThanEq",
    TT::operator_equal_to => "strictEqual",
    TT::operator_loose_equal_to => "equal",
    TT::operator_not_equal_to => "not",
    TT::operator_logical_and => "land",
    TT::operator_bitwise_and => "and",
    TT::operator_bitwise_xor => "xor",
    TT::operator_logical_or => "or",
    TT::operator_bitwise_or => "or",
    TT::operator_assignment |
    TT::operator_assignment_add |
    TT::operator_assignment_subtract |
    TT::operator_assignment_multiply |
    TT::operator_assignment_divide |
    TT::operator_increment |
    TT::operator_not |
    TT::operator_bitwise_flip |
    TT::operator_access => panic!("Operator {operator:?} cannot be compiled"),
    _ => unreachable!(),
  }
}

pub fn compile_expr(
  expr: &ASTExpression, output_name: OutputName, ident_gen: &mut IdentGenerator
) -> Result<(Vec<String>, Option<String>), CError> {
  use TokenType as TT;
  match expr {
    ASTExpression::Leaf(token) => {
      Ok(match output_name {
        OutputName::Specified(name) => (match token.variant {
          TT::identifier => vec![format!("set {name} {}", token.text)],
          TT::link => vec![format!("set {name} {}", &token.text[1..token.text.len()-1])],
          TT::number => vec![format!("set {name} {}", token.text)],
          TT::string => vec![format!("set {name} {}", token.text)],
          _ => unreachable!()
        }, Some(name)),
        OutputName::Any => (vec![], Some(match token.variant {
          TT::identifier => token.text.to_string(),
          TT::link => token.text[1..token.text.len()-1].to_string(),
          TT::number => token.text.to_string(),
          TT::string => token.text.to_string(),
          _ => unreachable!()
        })),
        OutputName::None => (vec![], None),
      })
    },
    ASTExpression::UnaryOperator { operator, operand } => {
      let name = match output_name {
        OutputName::Specified(n) => n,
        OutputName::Any => ident_gen.next_ident(),
        OutputName::None => return compile_expr(operand, OutputName::None, ident_gen),
      };
      let (mut code, Some(intermediate)) =
        compile_expr(operand, OutputName::Any, ident_gen)? else { unreachable!() };
      code.push(match operator.variant {
        TT::operator_minus => format!("op mul {name} {intermediate} -1"),
        TT::operator_not => format!("op equal {name} {intermediate} false"),
        TT::operator_increment => panic!("not yet implemented"),
        TT::operator_bitwise_flip => format!("op not {name} {intermediate} 0"),
        _ => unreachable!()
      });
      Ok((code, Some(name)))
    },
    ASTExpression::BinaryOperator { left, operator, right } => {
      let name = match output_name {
        OutputName::Specified(n) => n,
        OutputName::Any => ident_gen.next_ident(),
        OutputName::None => {
          let (mut code1, _) = compile_expr(left, OutputName::None, ident_gen)?;
          let (code2, _) = compile_expr(right, OutputName::None, ident_gen)?;
          code1.extend_from_slice(&code2);
          return Ok((code1, None));
        },
      };
      let (mut code1, Some(inter_left)) =
        compile_expr(left, OutputName::Any, ident_gen)? else { unreachable!() };
      let (code2, Some(inter_right)) =
        compile_expr(right, OutputName::Any, ident_gen)? else { unreachable!() };
      code1.extend_from_slice(&code2);
      code1.push(format!("op {} {name} {inter_left} {inter_right}", match operator.variant {
        TT::operator_assignment |
        TT::operator_assignment_add |
        TT::operator_assignment_subtract |
        TT::operator_assignment_multiply |
        TT::operator_assignment_divide |
        TT::operator_access => panic!("not yet implemented"),
        _ => compile_operator(operator.variant)
      }));
      Ok((code1, Some(name)))
    },
    ASTExpression::FunctionCall { function, arguments } => todo!(),
    ASTExpression::TemplateString { strings, values } => todo!(),
  }
}
