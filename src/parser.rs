use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

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

pub struct JsonTokenizer<I: Iterator<Item = u8>> {
    bytes: Peekable<I>,
    offset: u64,
    line: u64,
    col: u64,
}

pub struct JsonParser<I: Iterator<Item = u8>> {
    tokenizer: JsonTokenizer<I>,
}

impl<I: Iterator<Item = u8>> JsonParser<I> {
    pub fn new(it: I) -> Self {
        JsonParser {
            tokenizer: JsonTokenizer::new(it),
        }
    }

    fn parse_object(&mut self) -> JsonParseResult<JsonValue> {
        let first_token = self.tokenizer.next_token()?;
        if matches!(first_token, JsonToken::ObjClose) {
            return Ok(JsonValue::Object(HashMap::new()));
        }

        let mut m = HashMap::new();
        let mut current_token = first_token;
        loop {
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

            m.insert(key, self.parse_any()?);

            match self.tokenizer.next_token()? {
                JsonToken::Comma => {}
                JsonToken::ObjClose => return Ok(JsonValue::Object(m)),
                c => {
                    return self.tokenizer.err(format!(
                        "',' or '}}' is expected for object but actually found '{c:?}'",
                    ))
                }
            }

            current_token = self.tokenizer.next_token()?;
        }
    }

    fn parse_array(&mut self) -> JsonParseResult<JsonValue> {
        let first_token = self.tokenizer.next_token()?;

        if first_token == JsonToken::ArrayClose {
            return Ok(JsonValue::Array(vec![]));
        }

        let first_value = match first_token {
            JsonToken::Number(num) => JsonValue::Number(num),
            JsonToken::True => JsonValue::Boolean(true),
            JsonToken::False => JsonValue::Boolean(false),
            JsonToken::String(s) => JsonValue::String(s),
            JsonToken::Null => JsonValue::Null,
            JsonToken::ArrayOpen => self.parse_array()?,
            JsonToken::ObjOpen => self.parse_object()?,
            JsonToken::Comma | JsonToken::ArrayClose | JsonToken::Colon | JsonToken::ObjClose => {
                todo!()
            }
        };

        let mut v = vec![first_value];
        loop {
            match self.tokenizer.next_token()? {
                JsonToken::Comma => {}
                JsonToken::ArrayClose => return Ok(JsonValue::Array(v)),
                c => {
                    return self.tokenizer.err(format!(
                        "',' or ']' is expected for array but actually found '{c:?}'",
                    ))
                }
            }

            v.push(self.parse_any()?); // Next element
        }
    }

    fn parse_any(&mut self) -> JsonParseResult<JsonValue> {
        match self.tokenizer.next_token()? {
            JsonToken::Number(num) => Ok(JsonValue::Number(num)),
            JsonToken::True => Ok(JsonValue::Boolean(true)),
            JsonToken::False => Ok(JsonValue::Boolean(false)),
            JsonToken::String(s) => Ok(JsonValue::String(s)),
            JsonToken::Null => Ok(JsonValue::Null),
            JsonToken::ArrayOpen => self.parse_array(),
            JsonToken::ObjOpen => self.parse_object(),
            JsonToken::Comma | JsonToken::ArrayClose | JsonToken::Colon | JsonToken::ObjClose => {
                todo!()
            }
        }
    }

    pub fn parse(&mut self) -> JsonParseResult<JsonValue> {
        let v = self.parse_any()?;

        if let Some(c) = self.tokenizer.next() {
            return self
                .tokenizer
                .err(format!("Expected EOF but got character '{c:#x}'"));
        }

        Ok(v)
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

        let mut s = Vec::new();
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
                            let c = self.consume()?;
                            if let Some(h) = ascii_byte_to_hex_digit(c) {
                                u = u * 0x10 + h as u16;
                            } else {
                                return self.err(format!("Unicode character must be \\uXXXX (X is hex character) format but found character '{}'", c));
                            }
                        }
                        s.push((u & 0xf) as u8); // XXXmstange: untested
                        s.push((u >> 8) as u8);
                        // Additional \uXXXX character may follow. UTF-16 characters must be converted
                        // into UTF-8 string as sequence because surrogate pairs must be considered
                        // like "\uDBFF\uDFFF".
                        continue;
                    }
                    c => return self.err(format!("'\\{}' is invalid escaped character", c)),
                },
                b'"' => {
                    let s = String::from_utf8(s)
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

        let mut s = Vec::new();
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

        if s.starts_with(&[b'0']) && s.len() > 1 {
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
            if s.ends_with(&[b'.']) {
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
