use std::{env::args, error::Error, fs};

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
	println!("input:\n---\n{data}\n---\n");
	let output = lexer::lexer(&data).and_then(parser::parse);
	match output {
		Ok(output) => {
			println!("Compiling:\n---\n{output}\n---\n");
			let result = codegen::compile(output)?;
			println!("{result}");
		},
		Err(err) => eprintln!("{}", err.show(&data))
	}
	Ok(())

}

