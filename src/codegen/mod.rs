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

fn compile_arguments(arguments: &Vec<ASTExpression>, ident_gen: &mut IdentGenerator) -> Result<(Vec<String>, Vec<String>), CError> {
  let (code, names) = arguments.iter().map(|a|
    compile_expr_to_any(a, ident_gen)
  ).collect::<Result<Vec<_>, _>>()?.into_iter().unzip::<_, _, Vec<_>, Vec<String>>();
  Ok((code.into_iter().flatten().collect::<Vec<_>>(), names))
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
    ASTExpression::FunctionCall { function, arguments } => match &**function {
      ASTExpression::Leaf(Token { text, variant, .. }) if *variant == TT::identifier => todo!(),
      ASTExpression::BinaryOperator { left, operator, right } if operator.variant == TT::operator_access =>
        match (&**left, &**right) {
          (
            ASTExpression::Leaf(Token { text: left, variant: TT::identifier, span: lspan }),
            ASTExpression::Leaf(Token { text: right, variant: TT::identifier, span: rspan })
          ) => match &left[..] {
            "draw" => {
              let arg_count = match &right[..] {
                "clear" => 3,
                "color" => 4,
                "col" => 1,
                "stroke" => 1,
                "line" => 4,
                "rect" | "lineRect" => 4,
                "poly" | "linePoly" => 5,
                "triangle" => 6,
                "image" => 5,
                "print" => 3,
                "translate" | "scale" => 2,
                "rotate" => 1,
                _ => return err!("Unknown draw command", rspan.clone()),
              };
              if arguments.len() != arg_count {
                return err!(format!("Incorrect number of arguments for \"draw.{right}\": expected {arg_count} arguments"), rspan.clone());
              }
              let (mut code, inter_names) = compile_arguments(arguments, ident_gen)?;
              let args = inter_names.join(" ");
              if right == "rotate" {
                code.push(format!("draw {right} 0 0 {args}"));
              } else {
                code.push(format!("draw {right} {args}"));
              }
              Ok((code, Some("null".to_string())))
            },
            "control" => {
              let arg_count = match &right[..] {
                "enabled" => 2,
                "shoot" => 4,
                "shootp" => 3,
                "config" => 2,
                "color" => 4,
                _ => return err!("Unknown control command", rspan.clone()),
              };
              if arguments.len() != arg_count {
                return err!(format!("Incorrect number of arguments for \"control.{right}\": expected {arg_count} arguments"), rspan.clone());
              }
              let (mut code, inter_names) = compile_arguments(arguments, ident_gen)?;
              code.push(format!("control {right} {}", inter_names.join(" ")));
              Ok((code, Some("null".to_string())))
            },
            "ucontrol" => {
              let arg_count = match &right[..] {
                "idle" => 0,
                "stop" => 0,
                "unbind" => 0,
                "move" => 2,
                "approach" => 3,
                "boost" => 1,
                "pathfind" => 2,
                "autoPathfind" => 0,
                "target" => 3,
                "targetp" => 2,
                "itemDrop" => 2,
                "itemTake" => 3,
                "payDrop" => 0,
                "payTake" => 1,
                "payEnter" => 0,
                "mine" => 2,
                "flag" => 1,
                "build" => 5,
                "getBlock" => 2,
                "within" => 3,
                _ => return err!("Unknown ucontrol command", rspan.clone()),
              };
              if arguments.len() != arg_count {
                return err!(format!("Incorrect number of arguments for \"ucontrol.{right}\": expected {arg_count} arguments"), rspan.clone());
              }
              let (mut code, inter_names) = compile_arguments(arguments, ident_gen)?;
              match &right[..] {
                "getBlock" => {
                  let name = match output_name {
                    OutputName::Specified(n) => n,
                    OutputName::Any => ident_gen.next_ident(),
                    OutputName::None => return Ok((code, None)),
                  };
                  code.push(format!("ucontrol {right} {} 0 {name}", inter_names.join(" ")));
                  Ok((code, Some("null".to_string())))
                },
                "within" => {
                  let name = match output_name {
                    OutputName::Specified(n) => n,
                    OutputName::Any => ident_gen.next_ident(),
                    OutputName::None => return Ok((code, None)),
                  };
                  code.push(format!("ucontrol {right} {} {name}", inter_names.join(" ")));
                  Ok((code, Some("null".to_string())))
                },
                _ => {
                  code.push(format!("ucontrol {right} {}", inter_names.join(" ")));
                  Ok((code, Some("null".to_string())))
                }
              }
            },
            "ulocate" => {
              let arg_count = match &right[..] {
                "ore" => 1,
                "spawn" => 0,
                "damaged" => 0,
                "building" => 2,
                _ => return err!("Unknown ulocate command", rspan.clone()),
              };
              if arguments.len() != arg_count {
                return err!(format!("Incorrect number of arguments for \"ulocate.{right}\": expected {arg_count} arguments"), rspan.clone());
              }
              let name = match output_name {
                OutputName::Specified(n) => n,
                OutputName::Any => ident_gen.next_ident(),
                OutputName::None => return Ok((match &right[..] {
                  "spawn" | "damaged" => vec![],
                  "ore" | "building" => compile_expr(arguments.last().unwrap(), OutputName::None, ident_gen)?.0,
                  _ => unreachable!()
                }, None)),
              };
              let code = match &right[..] {
                "ore" => {
                  let (mut code, inter_names) = compile_arguments(arguments, ident_gen)?;
                  code.push(format!("ulocate ore core 0 {} {name}@x {name}@y {name} 0", inter_names[0]));
                  code
                },
                "spawn" => {
                  vec![format!("ulocate spawn core 0 0 {name}@x {name}@y {name} 0")]
                },
                "damaged" => {
                  vec![format!("ulocate damaged core 0 0 {name}@x {name}@y 0 {name}")]
                },
                "building" => {
                  let ASTExpression::Leaf(Token { text: group, variant: TT::identifier, .. }) = &arguments[0] else {
                    return err!("Expected a keyword", rspan.clone()); //TODO wrong span
                  };
                  let (mut code, enemy) = match &arguments[1] {
                    ASTExpression::Leaf(Token { text, variant: TT::identifier, .. }) if matches!(&text[..], "enemy" | "ally") =>
                      (Vec::with_capacity(1), match &text[..] {
                        "enemy" => "true",
                        "ally" => "false",
                        _ => unreachable!()
                      }.to_string()),
                    enemy => compile_expr_to_any(&enemy, ident_gen)?,
                  };
                  code.push(format!("ulocate building {group} {enemy} 0 {name}@x {name}@y {name}@found {name}"));
                  code
                },
                _ => unreachable!(),
              };
              Ok((code, Some(name)))
            },
            _ => err!(format!("Invalid function call: unknown namespace {left}"), lspan.clone())
          },
          _ => err!("Invalid function call: invalid access expression", 0..0)
        }
      ASTExpression::FunctionCall { .. } |
      ASTExpression::ArrayAccess { .. } |
      // ^^ these may become valid in the future if a function returns a function pointer
      ASTExpression::UnaryOperator { .. } |
      ASTExpression::TemplateString { .. } |
      ASTExpression::BinaryOperator { .. } |
      ASTExpression::Leaf(_) =>
        err!("Invalid function call: invalid function expression", 0..0), //TODO fix all the ranges, i need a range in ASTExpression
    },
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
