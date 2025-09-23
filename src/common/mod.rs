use std::{error::Error, fmt::Display, ops::Range};


pub type Span = Range<usize>;

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

