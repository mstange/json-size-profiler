use crate::JsonParseError;

use super::tokenizer::{JsonParseResult, JsonToken, JsonTokenizer, Location};

/// A [`JsonFragment`] paired with a [`LocationSpan`].
#[derive(Debug, Clone)]
pub struct JsonFragmentWithSpan {
    pub fragment: JsonFragment,
    pub span: LocationSpan,
}

/// A fragment of JSON. This is a bit more high-level than a token.
#[derive(Debug, Clone, PartialEq)]
pub enum JsonFragment {
    /// Corresponds to `{`. Always followed by [`JsonFragment::ObjectProperty`] or [`JsonFragment::EndObject`].
    ///
    /// The span includes only the single opening brace byte.
    BeginObject,

    /// The name of the current property. Always followed by the value of this property.
    ///
    /// The span goes from the starting quote to after the colon.
    ObjectProperty(String),

    /// Corresponds to `}`.
    ///
    /// The span includes only the single closing brace byte.
    EndObject,

    /// Corresponds to `[`. Always followed by the first element or by [`JsonFragment::EndArray`].
    ///
    /// The span includes only the single opening bracket byte.
    BeginArray,

    /// Corresponds to `]`.
    ///
    /// The span includes only the single closing bracket byte.
    EndArray,

    /// Any JSON value that's not an object or an array.
    ///
    /// The span encloses the value.
    PrimitiveValue(JsonPrimitiveValue),
}

impl JsonFragment {
    fn with_span(self, span: LocationSpan) -> JsonFragmentWithSpan {
        JsonFragmentWithSpan {
            fragment: self,
            span,
        }
    }
}

/// The start and end [`Location`] of a fragment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocationSpan {
    pub start: Location,
    pub end: Location,
}

impl LocationSpan {
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }
}

/// A JSON value which is not an object or an array.
#[derive(Debug, Clone, PartialEq)]
pub enum JsonPrimitiveValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Null,
}

/// A pull-based JSON parser which consumes an iterator over bytes and yields
/// a valid sequence of [`JsonFragmentWithSpan`] values.
///
/// This API allows gathering statistics about the contents of large JSON documents without ever
/// holding the entire document in memory.
///
/// [`JsonSession`] checks that the input is valid JSON. If an invalid sequence of tokens
/// is detected, [`JsonSession::next`] yields an error. As a user of [`JsonSession`], you can
/// rely on the fact that the yielded fragments will always describe a well-formed JSON document,
/// at least the part of the document that has been consumed so far. (To clarify, there is no
/// pre-pass which validates the entire document. Validation happens as you go, so
/// [`JsonSession::next`] will happily return fragments as long as it hasn't arrived at the error yet.)
///
/// When the optional feature `fallible-iterator` is used, [`JsonSession`] implements
/// `fallible_iterator::FallibleIterator`.
///
/// # Example
///
/// ```
/// use json_session::{JsonSession, JsonFragment, JsonPrimitiveValue};
///
/// # fn main() {
/// let input_str = r#"{"key1": 1234, "key2": [true], "key3": "value" }"#;
/// let expected = &[
///     JsonFragment::BeginObject,
///     JsonFragment::ObjectProperty(String::from("key1")),
///     JsonFragment::PrimitiveValue(JsonPrimitiveValue::Number(1234.0)),
///     JsonFragment::ObjectProperty(String::from("key2")),
///     JsonFragment::BeginArray,
///     JsonFragment::PrimitiveValue(JsonPrimitiveValue::Boolean(true)),
///     JsonFragment::EndArray,
///     JsonFragment::ObjectProperty(String::from("key3")),
///     JsonFragment::PrimitiveValue(JsonPrimitiveValue::String(String::from("value"))),
///     JsonFragment::EndObject,
/// ];
/// let mut session = JsonSession::new(input_str.as_bytes().iter().cloned());
/// for expected_fragment in expected {
///     let fragment = session.next().unwrap().unwrap().fragment;
///     assert_eq!(fragment, *expected_fragment);
/// }
/// assert!(session.next().unwrap().is_none());
/// # }
/// ```
pub struct JsonSession<I: Iterator<Item = u8>> {
    tokenizer: JsonTokenizer<I>,
    state_stack: Vec<StateStackEntry>,
}

#[derive(Debug, Clone)]
enum StateStackEntry {
    BeforeAnyValue,
    BeforeAnyValueWithToken {
        location: Location,
        token: JsonToken,
    },
    AfterObjectOpen,
    BeforeObjectPropertyKeyWithToken {
        location: Location,
        token: JsonToken,
    },
    AfterObjectPropertyValue,
    AfterArrayOpen,
    AfterArrayItem,
}

impl<I: Iterator<Item = u8>> JsonSession<I> {
    /// Create a new [`JsonSession`] from an iterator over bytes.
    pub fn new(it: I) -> Self {
        JsonSession {
            tokenizer: JsonTokenizer::new(it),
            state_stack: vec![StateStackEntry::BeforeAnyValue],
        }
    }

    /// Get the next [`JsonFragmentWithSpan`].
    ///
    /// Returns:
    ///
    /// - `Ok(Some(...))` in the regular case, with the next fragment.
    /// - `Ok(None)` if the JSON document is complete and the end of the input has been reached.
    /// - `Err(...)` if invalid JSON is detected.
    pub fn next(&mut self) -> JsonParseResult<Option<JsonFragmentWithSpan>> {
        while let Some(entry) = self.state_stack.last().cloned() {
            match entry {
                StateStackEntry::BeforeAnyValue => {
                    let (token, location) = self.tokenizer.next_token_and_location()?;
                    *self.state_stack.last_mut().unwrap() =
                        StateStackEntry::BeforeAnyValueWithToken { token, location };
                }
                StateStackEntry::BeforeAnyValueWithToken { location, token } => {
                    let span = LocationSpan::new(location, self.tokenizer.location());
                    let value = match token {
                        JsonToken::Number(num) => JsonPrimitiveValue::Number(num),
                        JsonToken::True => JsonPrimitiveValue::Boolean(true),
                        JsonToken::False => JsonPrimitiveValue::Boolean(false),
                        JsonToken::String(s) => JsonPrimitiveValue::String(s),
                        JsonToken::Null => JsonPrimitiveValue::Null,
                        JsonToken::ArrayOpen => {
                            *self.state_stack.last_mut().unwrap() = StateStackEntry::AfterArrayOpen;
                            return Ok(Some(JsonFragment::BeginArray.with_span(span)));
                        }
                        JsonToken::ObjOpen => {
                            *self.state_stack.last_mut().unwrap() =
                                StateStackEntry::AfterObjectOpen;
                            return Ok(Some(JsonFragment::BeginObject.with_span(span)));
                        }
                        t @ JsonToken::Comma
                        | t @ JsonToken::ArrayClose
                        | t @ JsonToken::Colon
                        | t @ JsonToken::ObjClose => {
                            return Err(JsonParseError::new(
                                format!("Unexpected token {t:?}"),
                                location,
                            ));
                        }
                    };
                    self.state_stack.pop();
                    return Ok(Some(JsonFragment::PrimitiveValue(value).with_span(span)));
                }
                StateStackEntry::AfterObjectOpen => {
                    let (token, location) = self.tokenizer.next_token_and_location()?;
                    if matches!(token, JsonToken::ObjClose) {
                        self.state_stack.pop();
                        let span = LocationSpan::new(location, self.tokenizer.location());
                        return Ok(Some(JsonFragment::EndObject.with_span(span)));
                    }

                    *self.state_stack.last_mut().unwrap() =
                        StateStackEntry::BeforeObjectPropertyKeyWithToken { location, token };
                }
                StateStackEntry::BeforeObjectPropertyKeyWithToken { location, token } => {
                    let key = match token {
                        JsonToken::String(s) => s,
                        other_token => {
                            return Err(JsonParseError::new(
                                format!("Key of object must be string but found {other_token:?}"),
                                location,
                            ))
                        }
                    };

                    let (token, colon_location) = self.tokenizer.next_token_and_location()?;
                    if token != JsonToken::Colon {
                        return Err(JsonParseError::new(
                            format!(
                                "':' is expected after key of object but actually found '{token:?}'",
                            ),
                            colon_location,
                        ));
                    }

                    *self.state_stack.last_mut().unwrap() =
                        StateStackEntry::AfterObjectPropertyValue;
                    self.state_stack.push(StateStackEntry::BeforeAnyValue);
                    let span = LocationSpan::new(location, self.tokenizer.location());
                    return Ok(Some(JsonFragment::ObjectProperty(key).with_span(span)));
                }
                StateStackEntry::AfterObjectPropertyValue => {
                    let (token, location) = self.tokenizer.next_token_and_location()?;
                    match token {
                        JsonToken::Comma => {}
                        JsonToken::ObjClose => {
                            let span = LocationSpan::new(location, self.tokenizer.location());
                            self.state_stack.pop();
                            return Ok(Some(JsonFragment::EndObject.with_span(span)));
                        }
                        token => {
                            return Err(JsonParseError::new(
                                format!(
                                "',' or '}}' is expected for object but actually found '{token:?}'",
                            ),
                                location,
                            ))
                        }
                    }

                    let (token, location) = self.tokenizer.next_token_and_location()?;
                    *self.state_stack.last_mut().unwrap() =
                        StateStackEntry::BeforeObjectPropertyKeyWithToken { location, token };
                }
                StateStackEntry::AfterArrayOpen => {
                    let (token, location) = self.tokenizer.next_token_and_location()?;

                    if token == JsonToken::ArrayClose {
                        self.state_stack.pop();
                        let span = LocationSpan::new(location, self.tokenizer.location());
                        return Ok(Some(JsonFragment::EndArray.with_span(span)));
                    }

                    *self.state_stack.last_mut().unwrap() = StateStackEntry::AfterArrayItem;
                    self.state_stack
                        .push(StateStackEntry::BeforeAnyValueWithToken { token, location });
                }
                StateStackEntry::AfterArrayItem => {
                    let (token, location) = self.tokenizer.next_token_and_location()?;
                    match token {
                        JsonToken::Comma => {}
                        JsonToken::ArrayClose => {
                            self.state_stack.pop();
                            let span = LocationSpan::new(location, self.tokenizer.location());
                            return Ok(Some(JsonFragment::EndArray.with_span(span)));
                        }
                        token => {
                            return Err(JsonParseError::new(
                                format!(
                                "',' or ']' is expected for array but actually found '{token:?}'",
                            ),
                                location,
                            ))
                        }
                    }

                    *self.state_stack.last_mut().unwrap() = StateStackEntry::AfterArrayItem;
                    self.state_stack.push(StateStackEntry::BeforeAnyValue);
                }
            }
        }

        self.tokenizer.expect_eof()?;

        Ok(None)
    }
}

#[cfg(feature = "fallible-iterator")]
impl<I: Iterator<Item = u8>> fallible_iterator::FallibleIterator for JsonSession<I> {
    type Item = JsonFragmentWithSpan;
    type Error = JsonParseError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        self.next()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn get(s: &str) -> (Vec<JsonFragmentWithSpan>, Option<JsonParseError>) {
        let mut session = JsonSession::new(s.as_bytes().iter().cloned());
        let mut v = Vec::new();
        loop {
            match session.next() {
                Ok(Some(ev)) => v.push(ev),
                Ok(None) => return (v, None),
                Err(e) => return (v, Some(e)),
            }
        }
    }

    #[test]
    fn test_doc() {
        let s = r#"{"key1": 1234, "key2": [true], "key3": "value" }"#;
        let expected = &[
            JsonFragment::BeginObject,
            JsonFragment::ObjectProperty(String::from("key1")),
            JsonFragment::PrimitiveValue(JsonPrimitiveValue::Number(1234.0)),
            JsonFragment::ObjectProperty(String::from("key2")),
            JsonFragment::BeginArray,
            JsonFragment::PrimitiveValue(JsonPrimitiveValue::Boolean(true)),
            JsonFragment::EndArray,
            JsonFragment::ObjectProperty(String::from("key3")),
            JsonFragment::PrimitiveValue(JsonPrimitiveValue::String(String::from("value"))),
            JsonFragment::EndObject,
        ];
        let mut session = JsonSession::new(s.as_bytes().iter().cloned());
        for expected_fragment in expected {
            let fragment = session.next().unwrap().unwrap().fragment;
            assert_eq!(fragment, *expected_fragment);
        }
        assert!(session.next().unwrap().is_none());
    }

    #[test]
    fn test_basic() {
        let s =
            r#"{"propertyKey": 1234, "arr": [], "obj": {}, "arr2": [null, false, true, -0.54e2] }"#;
        let (v, e) = get(s);
        assert_eq!(v.len(), 17);
        assert!(e.is_none());
    }
}
