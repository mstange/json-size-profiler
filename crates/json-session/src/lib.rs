//! Streaming parser for JSON. This crate provides an iterator which yields values of the following enum variants:
//! `BeginObject`, `ObjectProperty`, `EndObject`, `BeginArray`, `EndArray`, and `PrimitiveValue`.
//!
//! This allows gathering statistics about the contents of large JSON documents without ever
//! holding the entire document in memory.
//!
//! Every `JsonFragmentWithSpan` has location information attached to it, saying at which byte offset
//! (and at which line and column) the relevant fragment began and ended.
//!
//! ```
//! use json_session::{JsonSession, JsonFragment};
//!
//! # fn main() {
//!     let s = r#"{"key1": 1234, "key2": [true], "key3": "value" }"#;
//!     let expected = &[
//!         JsonFragment::BeginObject,
//!         JsonFragment::ObjectProperty(String::from("key1")),
//!         JsonFragment::PrimitiveValue(JsonPrimitiveValue::Number(1234.0)),
//!         JsonFragment::ObjectProperty(String::from("key2")),
//!         JsonFragment::BeginArray,
//!         JsonFragment::PrimitiveValue(JsonPrimitiveValue::Boolean(true)),
//!         JsonFragment::EndArray,
//!         JsonFragment::ObjectProperty(String::from("key3")),
//!         JsonFragment::PrimitiveValue(JsonPrimitiveValue::String(String::from("value"))),
//!         JsonFragment::EndObject,
//!     ];
//!     let mut session = JsonSession::new(s.as_bytes().iter().cloned());
//!     for expected_fragment in expected {
//!         let fragment = session.next().unwrap().unwrap().fragment;
//!         assert_eq!(fragment, *expected_fragment);
//!     }
//!     assert!(session.next().unwrap().is_none());
//! # }
//!
//! ```

mod session;
mod tokenizer;

pub use session::*;
pub use tokenizer::*;
