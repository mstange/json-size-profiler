use crate::JsonParseError;

use super::tokenizer::{JsonParseResult, JsonToken, JsonTokenizer, Location};

/// A fragment of JSON, along with location information.
#[derive(Debug, Clone)]
pub struct JsonFragmentWithSpan {
    pub fragment: JsonFragment,
    pub span: LocationSpan,
}

/// A fragment of JSON.
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct LocationSpan {
    pub start: Location,
    pub end: Location,
}

impl LocationSpan {
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum JsonPrimitiveValue {
    Number(f64),
    Boolean(bool),
    String(String),
    Null,
}

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
    ArrayAfterOpen,
    ArrayAfterItem,
}

impl<I: Iterator<Item = u8>> JsonSession<I> {
    pub fn new(it: I) -> Self {
        JsonSession {
            tokenizer: JsonTokenizer::new(it),
            state_stack: vec![StateStackEntry::BeforeAnyValue],
        }
    }

    pub fn next(&mut self) -> JsonParseResult<Option<JsonFragmentWithSpan>> {
        while let Some(entry) = self.state_stack.last().cloned() {
            match entry {
                StateStackEntry::BeforeAnyValue => {
                    let location = self.tokenizer.location();
                    let token = self.tokenizer.next_token()?;
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
                            *self.state_stack.last_mut().unwrap() = StateStackEntry::ArrayAfterOpen;
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
                    let location = self.tokenizer.location();
                    let token = self.tokenizer.next_token()?;
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

                    let colon_location = self.tokenizer.location();
                    let token = self.tokenizer.next_token()?;
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
                    let location = self.tokenizer.location();
                    match self.tokenizer.next_token()? {
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

                    let location = self.tokenizer.location();
                    let token = self.tokenizer.next_token()?;
                    *self.state_stack.last_mut().unwrap() =
                        StateStackEntry::BeforeObjectPropertyKeyWithToken { location, token };
                }
                StateStackEntry::ArrayAfterOpen => {
                    let location = self.tokenizer.location();
                    let token = self.tokenizer.next_token()?;

                    if token == JsonToken::ArrayClose {
                        self.state_stack.pop();
                        let span = LocationSpan::new(location, self.tokenizer.location());
                        return Ok(Some(JsonFragment::EndArray.with_span(span)));
                    }

                    *self.state_stack.last_mut().unwrap() = StateStackEntry::ArrayAfterItem;
                    self.state_stack
                        .push(StateStackEntry::BeforeAnyValueWithToken { token, location });
                }
                StateStackEntry::ArrayAfterItem => {
                    let location = self.tokenizer.location();
                    match self.tokenizer.next_token()? {
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

                    *self.state_stack.last_mut().unwrap() = StateStackEntry::ArrayAfterItem;
                    self.state_stack.push(StateStackEntry::BeforeAnyValue);
                }
            }
        }

        self.tokenizer.expect_eof()?;

        Ok(None)
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
        let mut session = JsonSession::new(s.as_bytes().iter().cloned());
            session.next().unwrap().unwrap().fragment; // JsonFragment::BeginObject
            session.next().unwrap().unwrap().fragment; // JsonFragment::ObjectProperty(String::from("key1"))
            session.next().unwrap().unwrap().fragment; // JsonFragment::PrimitiveValue(JsonPrimitiveValue::Number(1234.0));
            session.next().unwrap().unwrap().fragment; // JsonFragment::ObjectProperty(String::from("key2"))
            session.next().unwrap().unwrap().fragment; // JsonFragment::BeginArray
            session.next().unwrap().unwrap().fragment; // JsonFragment::PrimitiveValue(JsonPrimitiveValue::Bool(true));
            session.next().unwrap().unwrap().fragment; // JsonFragment::EndArray
            session.next().unwrap().unwrap().fragment; // JsonFragment::ObjectProperty(String::from("key3"))
            session.next().unwrap().unwrap().fragment; // JsonFragment::PrimitiveValue(JsonPrimitiveValue::String(String::from(value)));
            session.next().unwrap().unwrap().fragment; // JsonFragment::EndObject
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
