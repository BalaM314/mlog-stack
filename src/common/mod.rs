use std::{error::Error, fmt::Display, ops::Range};


pub type Span = Range<usize>;

fn get_line_number(text:&str, span:&Span) -> usize {
  text[..span.start].chars().filter(|c| *c == '\n').count() + 1
}

fn get_line(text:&str, number:usize) -> Option<&str> {
  text.split('\n').nth(number - 1)
}

fn get_line_span(text:&str, span:&Span) -> Span {
  let length_before = text[..span.start].split('\n').last().unwrap().len();
  length_before..length_before + span.end - span.start
}

#[derive(Debug, PartialEq, Eq)]
pub struct CError {
  message: String,
  span_detailed: Option<Span>, //None represents end of line
  span_general: Option<Span>,
}
impl CError {
  pub fn new(message: String, span_detailed: Option<Span>, span_general: Option<Span>) -> Self {
    Self { message, span_detailed, span_general }
  }
  pub fn show(&self, text:&str) -> String {
    match (&self.span_general, &self.span_detailed) {
      (None, None) => format!("{}", self.message),
      (Some(general), _) => {
        let line = get_line_number(text, general);
        let line_span = get_line_span(text, general);
        if line > 1 {
          format!("{}\n{:>5} | {}\n{:>5} | {}\n      | {}{}", self.message, line - 1, get_line(text, line - 1).unwrap(), line, get_line(text, line).unwrap(), " ".repeat(line_span.start), "~".repeat(line_span.len()))
        } else {
          format!("{}\n{line:>5} | {}\n      | {}{}", self.message, get_line(text, line).unwrap(), " ".repeat(line_span.start), "~".repeat(line_span.len()))
        }
      },
      (None, Some(detailed)) => {
        let line = get_line_number(text, detailed);
        let line_span = get_line_span(text, detailed);
        if line > 1 {
          format!("{}\n{:>5} | {}\n{:>5} | {}\n      | {}{}", self.message, line - 1, get_line(text, line - 1).unwrap(), line, get_line(text, line).unwrap(), " ".repeat(line_span.start), "~".repeat(line_span.len()))
        } else {
          format!("{}\n{line:>5} | {}\n      | {}{}", self.message, get_line(text, line).unwrap(), " ".repeat(line_span.start), "~".repeat(line_span.len()))
        }
      },
    }
  }
}
impl Display for CError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}\n{:?}", self.message, self.span_detailed)
  }
}
impl Error for CError {}

#[macro_export]
macro_rules! err_ {
  ($message:expr, None) => {
    crate::common::CError::new($message.into(), Option::None, Option::None)
  };
  ($message:expr, $span_detailed:expr, $span_general:expr) => {
    crate::common::CError::new($message.into(), Option::Some($span_detailed), Option::Some($span_general))
  };
  ($message:expr, $span_detailed:expr) => {
    crate::common::CError::new($message.into(), Option::Some($span_detailed), Option::None)
  };
}
#[macro_export]
macro_rules! err {
  ($message:expr, None) => {
    core::result::Result::Err(crate::common::CError::new($message.into(), Option::None, Option::None))
  };
  ($message:expr, $span_detailed:expr, $span_general:expr) => {
    core::result::Result::Err(crate::common::CError::new($message.into(), Option::Some($span_detailed), Option::Some($span_general)))
  };
  ($message:expr, $span_detailed:expr) => {
    core::result::Result::Err(crate::common::CError::new($message.into(), Option::Some($span_detailed), Option::None))
  };
}

