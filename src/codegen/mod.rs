use crate::{common::{CError, camel_to_kebab}, err, lexer::{Token, TokenType}, parser::{AST, ASTBlock, ASTExpression}};


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

fn is_namespace_containing_values(left:&str) -> bool {
  matches!(left,
    "Blocks" | "Liquids" | "Items" | "Units" | "Teams" | "Sounds"
  )
}

fn is_lookup_type(left:&str) -> bool {
  matches!(left,
    "Blocks" | "Liquids" | "Items" | "Units" | "Teams"
  )
}

fn compile_lookup_to_argument(left:&str) -> String {
  left[..left.len() - 1].to_ascii_lowercase()
}

/// left must be a valid value namespace
fn compile_namespace_value_access(left:&str, right:&str) -> String {
  match left {
    "Blocks" => match right {
      //ANUKEEEEEEEEEEEEEEEEEE
      "itemBridge" => "bridge-conveyor".to_string(),
      "airblastDrill" => "blast-drill".to_string(),
      "switchBlock" => "switch".to_string(),
      "largeSolarPanel" => "solar-panel-large".to_string(),
      "logicDisplayTile" => "tiled-logic-display".to_string(),
      _ =>
        // Making sure the building name is valid is handled by type checking
        format!("@{}", camel_to_kebab(right))
    },
    "Liquids" => format!("@{}", right),
    "Items" => format!("@{}", camel_to_kebab(right)),
    "Units" => format!("@{}", camel_to_kebab(right)),
    "Teams" => format!("@{}", right),
    "Sounds" => format!("@sfx-{}", camel_to_kebab(right)),
    _ => panic!("compile_namespace_value_access: Invalid namespace"),
  }
}

fn is_namespace_containing_functions(left:&str) -> bool {
  matches!(left,
    "draw" | "control" | "ucontrol" | "ulocate"
  )
}

fn is_senseable(text:&str) -> bool {
  matches!(text, "totalItems" | "firstItem" | "totalLiquids" | "totalPower" | "itemCapacity" | "liquidCapacity" | "powerCapacity" | "powerNetStored" | "powerNetCapacity" | "powerNetIn" | "powerNetOut" | "ammo" | "ammoCapacity" | "currentAmmoType" | "memoryCapacity" | "health" | "maxHealth" | "heat" | "shield" | "armor" | "efficiency" | "progress" | "timescale" | "rotation" | "x" | "y" | "velocityX" | "velocityY" | "shootX" | "shootY" | "cameraX" | "cameraY" | "cameraWidth" | "cameraHeight" | "displayWidth" | "displayHeight" | "bufferSize" | "operations" | "size" | "solid" | "dead" | "range" | "shooting" | "boosting" | "mineX" | "mineY" | "mining" | "speed" | "team" | "type" | "flag" | "controlled" | "controller" | "name" | "payloadCount" | "payloadType" | "totalPayload" | "payloadCapacity" | "id" | "enabled" | "config" | "color" | "scrap" | "copper" | "lead" | "graphite" | "coal" | "titanium" | "thorium" | "silicon" | "plastanium" | "phaseFabric" | "surgeAlloy" | "sporePod" | "sand" | "blastCompound" | "pyratite" | "metaglass" | "beryllium" | "tungsten" | "oxide" | "carbide" | "fissileMatter" | "dormantCyst" | "water" | "slag" | "oil" | "cryofluid" | "arkycite" | "gallium" | "neoplasm" | "ozone" | "hydrogen" | "nitrogen" | "cyanogen")
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

pub fn compile_expr_to_any(
  expr: &ASTExpression, ident_gen: &mut IdentGenerator
) -> Result<(Vec<String>, String), CError> {
  compile_expr(expr, OutputName::Any, ident_gen).map(|(code, name)|
    (code, name.unwrap())
  )
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
      let (mut code, intermediate) = compile_expr_to_any(operand, ident_gen)?;
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
      //this is ugly but there is no better way until https://github.com/rust-lang/rust/issues/87121 or https://github.com/rust-lang/rust/issues/51114
      if operator.variant == TT::operator_access {
        if let ASTExpression::Leaf(left) = &**left {
          if let ASTExpression::Leaf(right) = &**right {
            if is_namespace_containing_functions(&left.text[..]) {
              return err!("Namespace access is invalid in this position: expected a value, not a function", left.span.start..right.span.end);
            } else if is_namespace_containing_values(&left.text[..]) {
              return Ok((vec![], Some(compile_namespace_value_access(&left.text, &right.text))))
            }
          }
        }
      }
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
      let (mut code1, inter_left) = compile_expr_to_any(left, ident_gen)?;
      match operator.variant {
        TT::operator_assignment |
        TT::operator_assignment_add |
        TT::operator_assignment_subtract |
        TT::operator_assignment_multiply |
        TT::operator_assignment_divide => panic!("not yet implemented"),
        TT::operator_access => {
          match &**right {
            ASTExpression::Leaf(Token { variant: TT::identifier, text, span }) => {
              if is_senseable(text) {
                code1.push(format!("sensor {name} {inter_left} @{text}"));
                Ok((code1, Some(name)))
              } else {
                err!(format!("Invalid sensor statement: \"{text}\" is not a senseable property"), span.clone())
              }
            },
            _ => err!("RHS of an access expression must be an identifier", operator.span.clone())
          }
        },
        _ => {
          let (code2, inter_right) = compile_expr_to_any(right, ident_gen)?;
          code1.extend_from_slice(&code2);
          code1.push(format!("op {} {name} {inter_left} {inter_right}", compile_operator(operator.variant)));
          Ok((code1, Some(name)))
        }
      }
    },
    ASTExpression::FunctionCall { function, arguments } => todo!(),
    ASTExpression::ArrayAccess { target, index } => {
      let name = match output_name {
        OutputName::Specified(n) => n,
        OutputName::Any => ident_gen.next_ident(),
        OutputName::None => {
          let (code_for_index, _) = compile_expr(index, OutputName::None, ident_gen)?;
          //Lookup statement handling
          if let ASTExpression::Leaf(Token { text: target, .. }) = &**target {
            if is_lookup_type(target) {
              return Ok((code_for_index, None));
            }
          }
          let (code_for_target, _) = compile_expr(target, OutputName::None, ident_gen)?;
          let mut code = code_for_target;
          code.extend_from_slice(&code_for_index);
          return Ok((code, None));
        },
      };
      let (code_for_index, Some(inter_index)) =
        compile_expr(index, OutputName::Any, ident_gen)? else { unreachable!() };
      //Lookup statement handling
      if let ASTExpression::Leaf(Token { text: target, .. }) = &**target {
        if is_lookup_type(target) {
          let mut code = code_for_index;
          code.push(format!("lookup {} {name} {inter_index}", compile_lookup_to_argument(target)));
          return Ok((code, Some(name)));
        }
      }
      let (code_for_target, Some(inter_target)) =
        compile_expr(target, OutputName::Any, ident_gen)? else { unreachable!() };
      let mut code = code_for_target;
      code.extend_from_slice(&code_for_index);
      code.push(format!("sensor {name} {inter_target} {inter_index}"));
      Ok((code, Some(name)))
    },
    ASTExpression::TemplateString { strings, .. } => {
      return err!("MLOG does not support string concatenation", strings.first().unwrap().span.clone());
    },
  }
}
