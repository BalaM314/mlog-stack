use std::{error::Error, fs, process::ExitCode};
use clap::Parser;

use crate::common::CError;

mod lexer;
mod parser;
mod common;
mod codegen;


#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
	#[arg()]
	file_name: String,

	#[arg(short, long, default_value = "false")]
	verbose: bool,

	#[arg(short, long, default_value = "-")]
	output: String,
}

fn main() -> ExitCode {
	match run() {
		Err(err) => {
			eprintln!("{err}");
			ExitCode::FAILURE
		},
		_ => ExitCode::SUCCESS,
	}
}

fn run() -> Result<(), Box<dyn Error>> {
	let args = Args::parse();
	let data = fs::read_to_string(args.file_name)?;
	match compile(&data, args.verbose) {
		Ok(output) => {
			if args.output == "-" {
				println!("{output}");
			} else {
				if args.verbose { eprintln!("{output}"); }
				fs::write(args.output, output)?;
			}
			Ok(())
		},
		Err(err) => Err(err.show(&data).into()),
	}
}

fn compile(source:&str, verbose: bool) -> Result<String, CError> {
	if verbose { eprintln!("input:\n---\n{source}\n---\n"); }
	let tokens = lexer::lexer(source)?;
	let ast = parser::parse(tokens)?;
	if verbose { eprintln!("Compiling:\n---\n{ast}\n---\n"); }
	let code = codegen::compile(ast)?;
	Ok(code)
}


