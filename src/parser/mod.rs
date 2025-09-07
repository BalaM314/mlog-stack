use crate::lexer::{Span, Token};


type AST = ASTBlock;
#[derive(PartialEq, Eq, Debug)]
struct ASTNode {
  data: ASTNodeData,
  span: Span,
}

#[derive(PartialEq, Eq, Debug)]
enum ASTNodeData {
  Statement(ASTStatement),
  Block(ASTBlock),
}

#[derive(PartialEq, Eq, Debug)]
enum ASTType {
  Literal(Token),
}

#[derive(PartialEq, Eq, Debug)]
enum ASTBlock {
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
struct Declaration {
  binding: DeclarationType,
  identifier: Token,
  typ: Option<ASTType>,
}

#[derive(PartialEq, Eq, Debug)]
enum ASTStatement {
  Expression(ASTExpression),
  Declaration(Declaration),
  Break,
  Continue,
  Return(ASTExpression),
}

#[derive(PartialEq, Eq, Debug)]
enum DeclarationType {
  Var, Val, Cfg
}

#[derive(PartialEq, Eq, Debug)]
enum ASTExpression {
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

pub fn parse(tokens: Vec<Token>) -> AST {
  todo!()
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
      ASTBlock::Root { statements: vec![
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
                      data: ASTNodeData::Statement(ASTStatement::Return(ASTExpression::Leaf(b.num("1")))),
                      span: 0..0
                    }
                  ]
                }),
                span: 0..0,
              },
              ASTNode {
                data: ASTNodeData::Statement(ASTStatement::Return(
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
                )),
                span: 0..0
              }
            ],
          }),
          span: 0..0
        }
      ]}
    )
  }
}
