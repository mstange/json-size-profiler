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
//!     let mut session = JsonSession::new(s.as_bytes().iter().cloned());
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::BeginObject
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::ObjectProperty(String::from("key1"))
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::PrimitiveValue(JsonPrimitiveValue::Number(1234.0));
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::ObjectProperty(String::from("key2"))
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::BeginArray
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::PrimitiveValue(JsonPrimitiveValue::Bool(true));
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::EndArray
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::ObjectProperty(String::from("key3"))
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::PrimitiveValue(JsonPrimitiveValue::String(String::from(value)));
//!     session.next().unwrap().unwrap().fragment; // JsonFragment::EndObject
//!     assert!(session.next().unwrap().is_none());
//! # }
//!
//! ```

mod session;
mod tokenizer;

pub use session::*;
pub use tokenizer::*;
