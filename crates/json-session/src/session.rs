use super::tokenizer::{JsonToken, JsonTokenizer, JsonParseResult};

#[derive(Debug, Clone)]
pub enum JsonSessionEvent {
    BeginObject {
        pos_at_obj_start: u64,
    },
    ObjectProperty {
        property_key: String,
        pos_at_prop_key_start: u64,
    },
    EndObject {
        pos_after_obj_end: u64,
    },
    BeginArray {
        pos_at_array_start: u64,
    },
    EndArray {
        pos_after_array_end: u64,
    },
    PrimitiveValue {
        value: JsonPrimitiveValue,
        pos_before: u64,
        pos_after: u64,
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
    AnyValue,
    AnyValueWithToken {
        token: JsonToken,
        pos_before: u64,
    },
    ObjectAfterOpen,
    ObjectPropertyKey {
        pos_before: u64,
        current_token: JsonToken,
    },
    ObjectAfterPropertyValue,
    ArrayAfterOpen,
    ArrayAfterItem,
}

impl<I: Iterator<Item = u8>> JsonSession<I> {
    pub fn new(it: I) -> Self {
        JsonSession {
            tokenizer: JsonTokenizer::new(it),
            state_stack: vec![StateStackEntry::AnyValue],
        }
    }

    pub fn next(&mut self) -> JsonParseResult<Option<JsonSessionEvent>> {
        while let Some(entry) = self.state_stack.last().cloned() {
            match entry {
                StateStackEntry::AnyValue => {
                    let pos_before = self.tokenizer.offset;
                    let token = self.tokenizer.next_token()?;
                    *self.state_stack.last_mut().unwrap() =
                        StateStackEntry::AnyValueWithToken { token, pos_before };
                }
                StateStackEntry::AnyValueWithToken { token, pos_before } => {
                    let value = match token {
                        JsonToken::Number(num) => JsonPrimitiveValue::Number(num),
                        JsonToken::True => JsonPrimitiveValue::Boolean(true),
                        JsonToken::False => JsonPrimitiveValue::Boolean(false),
                        JsonToken::String(s) => JsonPrimitiveValue::String(s),
                        JsonToken::Null => JsonPrimitiveValue::Null,
                        JsonToken::ArrayOpen => {
                            *self.state_stack.last_mut().unwrap() = StateStackEntry::ArrayAfterOpen;
                            return Ok(Some(JsonSessionEvent::BeginArray {
                                pos_at_array_start: pos_before,
                            }));
                        }
                        JsonToken::ObjOpen => {
                            *self.state_stack.last_mut().unwrap() =
                                StateStackEntry::ObjectAfterOpen;
                            return Ok(Some(JsonSessionEvent::BeginObject {
                                pos_at_obj_start: pos_before,
                            }));
                        }
                        t @ JsonToken::Comma
                        | t @ JsonToken::ArrayClose
                        | t @ JsonToken::Colon
                        | t @ JsonToken::ObjClose => {
                            return self.tokenizer.err(format!("Unexpected token {t:?}"));
                        }
                    };
                    let pos_after = self.tokenizer.offset;
                    self.state_stack.pop();
                    return Ok(Some(JsonSessionEvent::PrimitiveValue {
                        value,
                        pos_before,
                        pos_after,
                    }));
                }
                StateStackEntry::ObjectAfterOpen => {
                    let pos_before = self.tokenizer.offset;
                    let first_token = self.tokenizer.next_token()?;
                    if matches!(first_token, JsonToken::ObjClose) {
                        self.state_stack.pop();
                        let pos_after_obj_end = self.tokenizer.offset;
                        return Ok(Some(JsonSessionEvent::EndObject { pos_after_obj_end }));
                    }

                    *self.state_stack.last_mut().unwrap() = StateStackEntry::ObjectPropertyKey {
                        pos_before,
                        current_token: first_token,
                    };
                }
                StateStackEntry::ObjectPropertyKey {
                    pos_before,
                    current_token,
                } => {
                    let key = match current_token {
                        JsonToken::String(s) => s,
                        v => {
                            return self
                                .tokenizer
                                .err(format!("Key of object must be string but found {:?}", v))
                        }
                    };

                    let c = self.tokenizer.next_token()?;
                    if c != JsonToken::Colon {
                        return self.tokenizer.err(format!(
                            "':' is expected after key of object but actually found '{c:?}'",
                        ));
                    }

                    *self.state_stack.last_mut().unwrap() =
                        StateStackEntry::ObjectAfterPropertyValue;
                    self.state_stack.push(StateStackEntry::AnyValue);
                    return Ok(Some(JsonSessionEvent::ObjectProperty {
                        property_key: key,
                        pos_at_prop_key_start: pos_before,
                    }));
                }
                StateStackEntry::ObjectAfterPropertyValue => {
                    match self.tokenizer.next_token()? {
                        JsonToken::Comma => {}
                        JsonToken::ObjClose => {
                            let pos_after_obj_end = self.tokenizer.offset;
                            self.state_stack.pop();
                            return Ok(Some(JsonSessionEvent::EndObject { pos_after_obj_end }));
                        }
                        c => {
                            return self.tokenizer.err(format!(
                                "',' or '}}' is expected for object but actually found '{c:?}'",
                            ))
                        }
                    }

                    let pos_before = self.tokenizer.offset;
                    let current_token = self.tokenizer.next_token()?;
                    *self.state_stack.last_mut().unwrap() = StateStackEntry::ObjectPropertyKey {
                        pos_before,
                        current_token,
                    };
                }
                StateStackEntry::ArrayAfterOpen => {
                    let pos_before = self.tokenizer.offset;
                    let first_token = self.tokenizer.next_token()?;

                    if first_token == JsonToken::ArrayClose {
                        let pos_after_array_end = self.tokenizer.offset;
                        self.state_stack.pop();
                        return Ok(Some(JsonSessionEvent::EndArray {
                            pos_after_array_end,
                        }));
                    }

                    *self.state_stack.last_mut().unwrap() = StateStackEntry::ArrayAfterItem;
                    self.state_stack.push(StateStackEntry::AnyValueWithToken {
                        token: first_token,
                        pos_before,
                    });
                }
                StateStackEntry::ArrayAfterItem => {
                    match self.tokenizer.next_token()? {
                        JsonToken::Comma => {}
                        JsonToken::ArrayClose => {
                            let pos_after_array_end = self.tokenizer.offset;
                            self.state_stack.pop();
                            return Ok(Some(JsonSessionEvent::EndArray {
                                pos_after_array_end,
                            }));
                        }
                        c => {
                            return self.tokenizer.err(format!(
                                "',' or ']' is expected for array but actually found '{c:?}'",
                            ))
                        }
                    }

                    *self.state_stack.last_mut().unwrap() = StateStackEntry::ArrayAfterItem;
                    self.state_stack.push(StateStackEntry::AnyValue);
                }
            }
        }

        if let Some(c) = self.tokenizer.next() {
            return self
                .tokenizer
                .err(format!("Expected EOF but got character '{c:#x}'"));
        }

        Ok(None)
    }
}
