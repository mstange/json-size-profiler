//! Streaming parser for JSON. The main type is [`JsonSession`], which consumes an iterator over
//! bytes and yields [`JsonFragmentWithSpan`] values. [`JsonFragment`] has the following enum variants:
//! `BeginObject`, `ObjectProperty`, `EndObject`, `BeginArray`, `EndArray`, and `PrimitiveValue`.
//!
//! Every yielded value has a [`LocationSpan`] attached to it, saying at which byte offset
//! (and at which line and column) the fragment begins and ends.
//!
//! This API allows gathering statistics about the contents of large JSON documents without ever
//! holding the entire document in memory.
//!
//! ```
//! use json_session::{JsonSession, JsonFragment, JsonPrimitiveValue};
//!
//! # fn main() {
//! let input_str = r#"{"key1": 1234, "key2": [true], "key3": "value" }"#;
//! let expected = &[
//!     JsonFragment::BeginObject,
//!     JsonFragment::ObjectProperty(String::from("key1")),
//!     JsonFragment::PrimitiveValue(JsonPrimitiveValue::Number(1234.0)),
//!     JsonFragment::ObjectProperty(String::from("key2")),
//!     JsonFragment::BeginArray,
//!     JsonFragment::PrimitiveValue(JsonPrimitiveValue::Boolean(true)),
//!     JsonFragment::EndArray,
//!     JsonFragment::ObjectProperty(String::from("key3")),
//!     JsonFragment::PrimitiveValue(JsonPrimitiveValue::String(String::from("value"))),
//!     JsonFragment::EndObject,
//! ];
//! let mut session = JsonSession::new(input_str.as_bytes().iter().cloned());
//! for expected_fragment in expected {
//!     let fragment = session.next().unwrap().unwrap().fragment;
//!     assert_eq!(fragment, *expected_fragment);
//! }
//! assert!(session.next().unwrap().is_none());
//! # }
//! ```
//!
//! [`JsonSession`] checks that the JSON is valid, and only returns a sequence of fragments
//! that describe valid JSON. If an invalid sequence of bytes or tokens is detected,
//! [`JsonSession::next`] yields an error.
//!
//! This crate also exposes the tokenizer and  token types that are used internally, but
//! you can ignore them.

mod session;
mod tokenizer;

pub use session::*;
pub use tokenizer::*;
