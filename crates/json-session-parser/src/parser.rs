use std::collections::HashMap;

use json_session::{JsonParseResult, JsonPrimitiveValue, JsonSession, JsonSessionEvent};

use super::json_value::JsonValue;

pub struct JsonParser<I: Iterator<Item = u8>> {
    iter: I,
}

struct JsonParserState {
    stack: Vec<JsonParserStateStackEntry>,
    final_value: Option<JsonValue>,
}

enum JsonParserStateStackEntry {
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>, Option<String>),
}

impl JsonParserState {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            final_value: None,
        }
    }

    fn put_value(&mut self, value: JsonValue) {
        match self.stack.last_mut() {
            None => self.final_value = Some(value),
            Some(JsonParserStateStackEntry::Array(v)) => v.push(value),
            Some(JsonParserStateStackEntry::Object(m, key)) => {
                m.insert(key.take().unwrap(), value);
            }
        }
    }
}

impl<I: Iterator<Item = u8>> JsonParser<I> {
    pub fn new(iter: I) -> Self {
        JsonParser { iter }
    }

    pub fn parse(self) -> JsonParseResult<JsonValue> {
        let mut session = JsonSession::new(self.iter);
        let mut state = JsonParserState::new();
        while let Some(event) = session.next()? {
            match event {
                JsonSessionEvent::BeginObject { .. } => {
                    state
                        .stack
                        .push(JsonParserStateStackEntry::Object(HashMap::new(), None));
                }
                JsonSessionEvent::ObjectProperty { property_key, .. } => {
                    match state.stack.last_mut().unwrap() {
                        JsonParserStateStackEntry::Object(_, key) => *key = Some(property_key),
                        _ => panic!(),
                    }
                }
                JsonSessionEvent::EndObject { .. } => match state.stack.pop().unwrap() {
                    JsonParserStateStackEntry::Object(m, None) => {
                        state.put_value(JsonValue::Object(m))
                    }
                    _ => panic!(),
                },
                JsonSessionEvent::BeginArray { .. } => {
                    state
                        .stack
                        .push(JsonParserStateStackEntry::Array(Vec::new()));
                }
                JsonSessionEvent::EndArray { .. } => match state.stack.pop().unwrap() {
                    JsonParserStateStackEntry::Array(v) => state.put_value(JsonValue::Array(v)),
                    _ => panic!(),
                },
                JsonSessionEvent::PrimitiveValue { value, .. } => {
                    let value = match value {
                        JsonPrimitiveValue::Number(n) => JsonValue::Number(n),
                        JsonPrimitiveValue::Boolean(b) => JsonValue::Boolean(b),
                        JsonPrimitiveValue::String(s) => JsonValue::String(s),
                        JsonPrimitiveValue::Null => JsonValue::Null,
                    };
                    state.put_value(value);
                }
            }
        }
        Ok(state.final_value.take().unwrap())
    }
}
