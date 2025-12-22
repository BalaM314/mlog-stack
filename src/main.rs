use std::{env::args, error::Error, fs};

use crate::{codegen::{IdentGenerator, compile_expr}, parser::{ASTNode, ASTNodeData, ASTStatement}};


mod lexer;
mod parser;
mod common;
mod codegen;

fn main() {
	match run() {
		Err(err) => eprintln!("{err}"),
		_ => {},
	}
}

fn run() -> Result<(), Box<dyn Error>> {
	let filename = args().nth(1).ok_or("please specify a file to read")?;
	let data = fs::read_to_string(filename)?;
	let output = lexer::lexer(&data).and_then(parser::parse);
	match output {
		Ok(output) => {
			match &output {
				parser::ASTBlock::Root { statements } => {
					match statements.first() {
						Some(ASTNode { data: ASTNodeData::Statement(ASTStatement::Expression(expr)), .. }) => {
							let mut ident_gen = IdentGenerator::new();
							dbg!(compile_expr(expr, codegen::OutputName::Specified("foo".to_string()), &mut ident_gen));
						},
						_ => {}
					}
				},
				_ => {}
			}
			println!("{output}")
		},
		Err(err) => eprintln!("{}", err.show(&data))
	}
	Ok(())

}

