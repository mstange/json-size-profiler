use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

use smallvec::SmallVec;

use crate::json_value::JsonValue;

#[derive(Debug, Clone, PartialEq)]
pub enum JsonToken {
    Number(f64),
    True,
    False,
    String(String),
    Null,
    ArrayOpen,
    Comma,
    ArrayClose,
    ObjOpen,
    Colon,
    ObjClose,
}

#[derive(Debug)]
pub struct JsonParseError {
    msg: String,
    line: u64,
    col: u64,
}

impl JsonParseError {
    fn new(msg: String, line: u64, col: u64) -> JsonParseError {
        JsonParseError { msg, line, col }
    }
}

impl fmt::Display for JsonParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Parse error at line:{}, col:{}: {}",
            self.line, self.col, &self.msg,
        )
    }
}

impl std::error::Error for JsonParseError {}

pub type JsonParseResult<T> = Result<T, JsonParseError>;

// Note: char::is_ascii_whitespace is not available because some characters are not defined as
// whitespace character in JSON spec. For example, U+000C FORM FEED is whitespace in Rust but
// it isn't in JSON.
fn is_whitespace(c: u8) -> bool {
    matches!(c, 0x20 | 0xa | 0xd | 0x9)
}

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
pub struct JsonTokenizer<I: Iterator<Item = u8>> {
    bytes: Peekable<I>,
    offset: u64,
    line: u64,
    col: u64,
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

impl<I: Iterator<Item = u8>> JsonTokenizer<I> {
    pub fn new(it: I) -> Self {
        JsonTokenizer {
            bytes: it.peekable(),
            offset: 0,
            line: 1,
            col: 0,
        }
    }

    fn err<T>(&self, msg: String) -> Result<T, JsonParseError> {
        Err(JsonParseError::new(msg, self.line, self.col))
    }

    fn unexpected_eof(&self) -> Result<u8, JsonParseError> {
        Err(JsonParseError::new(
            String::from("Unexpected EOF"),
            self.line,
            self.col,
        ))
    }

    fn next_pos(&mut self, c: u8) {
        if c == b'\n' {
            self.col = 0;
            self.line += 1;
        } else {
            self.col += 1;
        }
        self.offset += 1;
    }

    fn peek(&mut self) -> Result<u8, JsonParseError> {
        while let Some(c) = self.bytes.peek().copied() {
            if !is_whitespace(c) {
                return Ok(c);
            }
            self.next_pos(c);
            self.bytes.next().unwrap();
        }
        self.unexpected_eof()
    }

    fn next(&mut self) -> Option<u8> {
        while let Some(c) = self.bytes.next() {
            self.next_pos(c);
            if !is_whitespace(c) {
                return Some(c);
            }
        }
        None
    }

    fn next_no_skip(&mut self) -> Option<u8> {
        if let Some(c) = self.bytes.next() {
            self.next_pos(c);
            return Some(c);
        }
        None
    }

    fn consume(&mut self) -> Result<u8, JsonParseError> {
        if let Some(c) = self.next() {
            Ok(c)
        } else {
            self.unexpected_eof()
        }
    }

    fn consume_no_skip(&mut self) -> Result<u8, JsonParseError> {
        if let Some(c) = self.next_no_skip() {
            Ok(c)
        } else {
            self.unexpected_eof()
        }
    }

    fn parse_string(&mut self) -> JsonParseResult<JsonToken> {
        if self.consume()? != b'"' {
            return self.err(String::from("String must starts with double quote"));
        }

        let mut s = SmallVec::<[u8; 10]>::new();
        loop {
            let c = match self.consume_no_skip()? {
                b'\\' => match self.consume_no_skip()? {
                    b'\\' => b'\\',
                    b'/' => b'/',
                    b'"' => b'"',
                    b'b' => 0x8,
                    b'f' => 0xc,
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'u' => {
                        let mut u = 0u16;
                        for _ in 0..4 {
                            let b = self.consume_no_skip()?;
                            if let Some(h) = ascii_byte_to_hex_digit(b) {
                                u = u * 0x10 + h as u16;
                            } else {
                                return self.err(format!("Unicode character must be \\uXXXX (X is hex character) format but found byte {b:#x}"));
                            }
                        }
                        let c = match u {
                            0xD800..=0xDBFF => {
                                // First surrogate

                                // Parse the second surrogate, which must be directly following.
                                if self.consume_no_skip()? != b'\\'
                                    || self.consume_no_skip()? != b'u'
                                {
                                    return self.err(format!("First UTF-16 surragate {u:#x} must be directly followed by a second \\uXXXX surrogate."));
                                }
                                let mut u2 = 0u16;
                                for _ in 0..4 {
                                    let b = self.consume_no_skip()?;
                                    if let Some(h) = ascii_byte_to_hex_digit(b) {
                                        u2 = u2 * 0x10 + h as u16;
                                    } else {
                                        return self.err(format!("Unicode character must be \\uXXXX (X is hex character) format but found byte '{b:#x}'"));
                                    }
                                }
                                if !matches!(u2, 0xDC00..=0xDFFF) {
                                    return self.err(format!("First UTF-16 surrogate {u:#x} must be directly followed by a second \\uXXXX surrogate, but found something that's not a second surrogate: {u2:#x}."));
                                }

                                // Now we have both the first and the second surrogate. Assemble them into a char, the same way that char::decode_utf16 does it.
                                let c =
                                    (((u & 0x3ff) as u32) << 10 | (u2 & 0x3ff) as u32) + 0x1_0000;
                                char::from_u32(c).unwrap()
                            }
                            0xDC00..=0xDFFF => {
                                return self
                                    .err(format!("Unpaired UTF-16 second surrogate: {u:#x}"));
                            }
                            _ => char::from_u32(u as u32).unwrap(),
                        };
                        match c.len_utf8() {
                            1 => s.push(c as u8),
                            _ => s.extend(c.encode_utf8(&mut [0; 4]).as_bytes().iter().cloned()),
                        }
                        continue;
                    }
                    c => return self.err(format!("'\\{}' is invalid escaped character", c)),
                },
                b'"' => {
                    let s = String::from_utf8(s.to_vec())
                        .or_else(|_| self.err("Invalid UTF-8 in string".into()))?;
                    return Ok(JsonToken::String(s));
                }
                // Note: c.is_control() is not available here because JSON accepts 0x7f (DEL) in
                // string literals but 0x7f is control character.
                // Rough spec of JSON says string literal cannot contain control characters. But it
                // can actually contain 0x7f.
                c if c < 0x20 => {
                    return self.err(format!("String cannot contain control character {c:#x}"));
                }
                c => c,
            };

            s.push(c);
        }
    }

    fn parse_constant(&mut self, s: &'static [u8]) -> Option<JsonParseError> {
        for c in s {
            match self.consume_no_skip() {
                Ok(x) if x != *c => {
                    return Some(JsonParseError::new(
                        format!(
                            "Unexpected character '{}' while parsing '{}'",
                            c,
                            std::str::from_utf8(s).unwrap()
                        ),
                        self.line,
                        self.col,
                    ));
                }
                Ok(_) => {}
                Err(e) => return Some(e),
            }
        }
        None
    }

    fn parse_null(&mut self) -> JsonParseResult<JsonToken> {
        match self.parse_constant(b"null") {
            Some(err) => Err(err),
            None => Ok(JsonToken::Null),
        }
    }

    fn parse_true(&mut self) -> JsonParseResult<JsonToken> {
        match self.parse_constant(b"true") {
            Some(err) => Err(err),
            None => Ok(JsonToken::True),
        }
    }

    fn parse_false(&mut self) -> JsonParseResult<JsonToken> {
        match self.parse_constant(b"false") {
            Some(err) => Err(err),
            None => Ok(JsonToken::False),
        }
    }

    fn parse_number(&mut self) -> JsonParseResult<JsonToken> {
        let neg = if self.peek()? == b'-' {
            self.consume_no_skip().unwrap();
            true
        } else {
            false
        };

        let mut s = SmallVec::<[u8; 16]>::new();
        let mut saw_dot = false;
        let mut saw_exp = false;

        while let Some(d) = self.bytes.peek() {
            match d {
                b'0'..=b'9' => s.push(*d),
                b'.' => {
                    saw_dot = true;
                    break;
                }
                b'e' | b'E' => {
                    saw_exp = true;
                    break;
                }
                _ => break,
            }
            self.consume_no_skip().unwrap();
        }

        if s.is_empty() {
            return self.err("Integer part must not be empty in number literal".to_string());
        }

        if s.starts_with(b"0") && s.len() > 1 {
            return self
                .err("Integer part of number must not start with 0 except for '0'".to_string());
        }

        if saw_dot {
            s.push(self.consume_no_skip().unwrap()); // eat '.'
            while let Some(d) = self.bytes.peek() {
                match d {
                    b'0'..=b'9' => s.push(*d),
                    b'e' | b'E' => {
                        saw_exp = true;
                        break;
                    }
                    _ => break,
                }
                self.consume_no_skip().unwrap();
            }
            if s.ends_with(b".") {
                return self.err("Fraction part of number must not be empty".to_string());
            }
        }

        if saw_exp {
            s.push(self.consume_no_skip().unwrap()); // eat 'e' or 'E'
            if let Some(b'+') | Some(b'-') = self.bytes.peek() {
                s.push(self.consume_no_skip().unwrap());
            }

            let mut saw_digit = false;
            while let Some(d) = self.bytes.peek() {
                match d {
                    b'0'..=b'9' => s.push(*d),
                    _ => break,
                }
                saw_digit = true;
                self.consume_no_skip().unwrap();
            }

            if !saw_digit {
                return self.err("Exponent part must not be empty in number literal".to_string());
            }
        }

        let s = std::str::from_utf8(&s).unwrap();
        match s.parse::<f64>() {
            Ok(n) => Ok(JsonToken::Number(if neg { -n } else { n })),
            Err(err) => self.err(format!("Invalid number literal '{}': {}", s, err)),
        }
    }

    pub fn next_token(&mut self) -> JsonParseResult<JsonToken> {
        let token = match self.peek()? {
            b'[' => JsonToken::ArrayOpen,
            b']' => JsonToken::ArrayClose,
            b'{' => JsonToken::ObjOpen,
            b'}' => JsonToken::ObjClose,
            b':' => JsonToken::Colon,
            b',' => JsonToken::Comma,
            b'0'..=b'9' | b'-' => return self.parse_number(),
            b'"' => return self.parse_string(),
            b't' => return self.parse_true(),
            b'f' => return self.parse_false(),
            b'n' => return self.parse_null(),
            c => return self.err(format!("Invalid byte: {c:#x}")),
        };
        self.consume_no_skip()?;
        Ok(token)
    }
}

impl FromStr for JsonValue {
    type Err = JsonParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        JsonParser::new(s.bytes()).parse()
    }
}

fn ascii_byte_to_hex_digit(c: u8) -> Option<u8> {
    if c.is_ascii_digit() {
        Some(c - b'0')
    } else if (b'a'..=b'f').contains(&c) {
        Some(10 + (c - b'a'))
    } else if (b'A'..=b'F').contains(&c) {
        Some(10 + (c - b'A'))
    } else {
        None
    }
}
