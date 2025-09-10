use std::{env::args, error::Error, fs};


mod lexer;
mod parser;
mod common;

fn main() {
	match run() {
		Err(err) => eprintln!("{err}"),
		_ => {},
	}
}

fn run() -> Result<(), Box<dyn Error>> {
	let filename = args().nth(1).ok_or("please specify a file to read")?;
	let data = fs::read_to_string(filename)?;
	let output = parser::parse(lexer::lexer(&data)?)?;
	println!("{output}");
	Ok(())

}

