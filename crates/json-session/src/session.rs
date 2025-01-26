use crate::JsonParseError;

use super::tokenizer::{JsonParseResult, JsonToken, JsonTokenizer, Location};

#[derive(Debug, Clone)]
pub enum JsonSessionEvent {
    BeginObject {
        location_at_obj_start: Location,
    },
    ObjectProperty {
        property_key: String,
        location_at_prop_key_start: Location,
    },
    EndObject {
        location_after_obj_end: Location,
    },
    BeginArray {
        location_at_array_start: Location,
    },
    EndArray {
        location_after_array_end: Location,
    },
    PrimitiveValue {
        value: JsonPrimitiveValue,
        location_at_value_start: Location,
        location_after_value_end: Location,
    },
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

    pub fn next(&mut self) -> JsonParseResult<Option<JsonSessionEvent>> {
        while let Some(entry) = self.state_stack.last().cloned() {
            match entry {
                StateStackEntry::BeforeAnyValue => {
                    let location = self.tokenizer.location();
                    let token = self.tokenizer.next_token()?;
                    *self.state_stack.last_mut().unwrap() =
                        StateStackEntry::BeforeAnyValueWithToken { token, location };
                }
                StateStackEntry::BeforeAnyValueWithToken { location, token } => {
                    let value = match token {
                        JsonToken::Number(num) => JsonPrimitiveValue::Number(num),
                        JsonToken::True => JsonPrimitiveValue::Boolean(true),
                        JsonToken::False => JsonPrimitiveValue::Boolean(false),
                        JsonToken::String(s) => JsonPrimitiveValue::String(s),
                        JsonToken::Null => JsonPrimitiveValue::Null,
                        JsonToken::ArrayOpen => {
                            *self.state_stack.last_mut().unwrap() = StateStackEntry::ArrayAfterOpen;
                            return Ok(Some(JsonSessionEvent::BeginArray {
                                location_at_array_start: location,
                            }));
                        }
                        JsonToken::ObjOpen => {
                            *self.state_stack.last_mut().unwrap() =
                                StateStackEntry::AfterObjectOpen;
                            return Ok(Some(JsonSessionEvent::BeginObject {
                                location_at_obj_start: location,
                            }));
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
                    let location_after = self.tokenizer.location();
                    self.state_stack.pop();
                    return Ok(Some(JsonSessionEvent::PrimitiveValue {
                        value,
                        location_at_value_start: location,
                        location_after_value_end: location_after,
                    }));
                }
                StateStackEntry::AfterObjectOpen => {
                    let location = self.tokenizer.location();
                    let token = self.tokenizer.next_token()?;
                    if matches!(token, JsonToken::ObjClose) {
                        self.state_stack.pop();
                        let location_after_obj_end = self.tokenizer.location();
                        return Ok(Some(JsonSessionEvent::EndObject {
                            location_after_obj_end,
                        }));
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
                    return Ok(Some(JsonSessionEvent::ObjectProperty {
                        property_key: key,
                        location_at_prop_key_start: location,
                    }));
                }
                StateStackEntry::AfterObjectPropertyValue => {
                    let location = self.tokenizer.location();
                    match self.tokenizer.next_token()? {
                        JsonToken::Comma => {}
                        JsonToken::ObjClose => {
                            let location_after_obj_end = self.tokenizer.location();
                            self.state_stack.pop();
                            return Ok(Some(JsonSessionEvent::EndObject {
                                location_after_obj_end,
                            }));
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
                        let location_after_array_end = self.tokenizer.location();
                        self.state_stack.pop();
                        return Ok(Some(JsonSessionEvent::EndArray {
                            location_after_array_end,
                        }));
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
                            let location_after_array_end = self.tokenizer.location();
                            self.state_stack.pop();
                            return Ok(Some(JsonSessionEvent::EndArray {
                                location_after_array_end,
                            }));
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

    fn get(s: &str) -> (Vec<JsonSessionEvent>, Option<JsonParseError>) {
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
    fn test_basic() {
        let s =
            r#"{"propertyKey": 1234, "arr": [], "obj": {}, "arr2": [null, false, true, -0.54e2] }"#;
        let (v, e) = get(s);
        assert_eq!(v.len(), 17);
    }
}
