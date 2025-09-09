use std::{env::args, fs};


mod lexer;
mod parser;

fn main() {
	if let Some(filename) = args().nth(1) {
		match fs::read_to_string(filename) {
			Ok(data) => {
				let output = parser::parse(lexer::lexer(&data));
				println!("{output}");
			},
			Err(err) => eprintln!("{err}"),
		}
	} else {
		eprintln!("please specify a file to read");
	}
}
