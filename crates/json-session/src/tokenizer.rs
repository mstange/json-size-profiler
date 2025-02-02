use std::fmt;
use std::iter::Peekable;

use smallvec::SmallVec;

/// A JSON token.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
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

/// A byte offset and the corresponding line and column number.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub byte_offset: u64,
    pub line: u64,
    pub col: u64,
}

impl Location {
    fn advance_by_byte(&mut self, c: u8) {
        if c == b'\n' {
            self.col = 0;
            self.line += 1;
        } else {
            self.col += 1;
        }
        self.byte_offset += 1;
    }
}

/// The error type used in this crate. Comes with Location information.
#[derive(Debug)]
pub struct JsonParseError {
    msg: String,
    location: Location,
}

impl JsonParseError {
    /// Creates a new [`JsonParseError`].
    pub fn new(msg: String, location: Location) -> JsonParseError {
        JsonParseError { msg, location }
    }

    /// The error message.
    pub fn msg(&self) -> &str {
        &self.msg
    }

    /// The location in the source document at which the parse error was encountered.
    pub fn location(&self) -> Location {
        self.location
    }
}

impl fmt::Display for JsonParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Parse error at line:{}, col:{}: {}",
            self.location.line, self.location.col, &self.msg,
        )
    }
}

impl std::error::Error for JsonParseError {}

/// A type alias for `Result<T, JsonParseError>`.
pub type JsonParseResult<T> = Result<T, JsonParseError>;

// Note: char::is_ascii_whitespace is not available because some characters are not defined as
// whitespace character in JSON spec. For example, U+000C FORM FEED is whitespace in Rust but
// it isn't in JSON.
fn is_whitespace(c: u8) -> bool {
    matches!(c, 0x20 | 0xa | 0xd | 0x9)
}

/// A pull-based tokenizer which takes an iterator over bytes and emits [`JsonToken`]s.
pub struct JsonTokenizer<I: Iterator<Item = u8>> {
    bytes: Peekable<I>,
    location: Location,
}

impl<I: Iterator<Item = u8>> JsonTokenizer<I> {
    /// Create a new [`JsonTokenizer`]
    pub fn new(it: I) -> Self {
        JsonTokenizer {
            bytes: it.peekable(),
            location: Location::default(),
        }
    }

    /// The location of the token that will be returned by the next call to `next_token()`.
    ///
    /// Well that's not entirely true if there is whitespace before the next token. In that
    /// case, this wuld be the location of that white space.
    pub fn location(&self) -> Location {
        self.location
    }

    /// Returns an error if there is more than just white space in the remaining bytes.
    pub fn expect_eof(&mut self) -> Result<(), JsonParseError> {
        match self.peek_byte_skip_whitespace() {
            Some(b) => self.err(format!("Expected EOF but found byte {b:#x}")),
            None => Ok(()),
        }
    }

    fn err<T>(&self, msg: String) -> Result<T, JsonParseError> {
        Err(JsonParseError::new(msg, self.location))
    }

    fn eof_err(&self) -> JsonParseError {
        JsonParseError::new(String::from("Unexpected EOF"), self.location)
    }

    fn peek_byte_skip_whitespace(&mut self) -> Option<u8> {
        while let Some(c) = self.bytes.peek().copied() {
            if is_whitespace(c) {
                self.bytes.next().unwrap();
                self.location.advance_by_byte(c);
                continue;
            }
            return Some(c);
        }
        None
    }

    fn consume_byte(&mut self) -> Result<u8, JsonParseError> {
        match self.bytes.next() {
            Some(b) => {
                self.location.advance_by_byte(b);
                Ok(b)
            }
            None => Err(self.eof_err()),
        }
    }

    fn consume_string(&mut self) -> JsonParseResult<JsonToken> {
        if self.consume_byte().unwrap() != b'"' {
            panic!("This function should only be called after the caller has encountered a start quote");
        }

        let mut s = SmallVec::<[u8; 10]>::new();
        loop {
            let b = match self.consume_byte()? {
                b'\\' => match self.consume_byte()? {
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
                            let b = self.consume_byte()?;
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
                                if self.consume_byte()? != b'\\' || self.consume_byte()? != b'u' {
                                    return self.err(format!("First UTF-16 surragate {u:#x} must be directly followed by a second \\uXXXX surrogate."));
                                }
                                let mut u2 = 0u16;
                                for _ in 0..4 {
                                    let b = self.consume_byte()?;
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
                            _ => s.extend_from_slice(c.encode_utf8(&mut [0; 4]).as_bytes()),
                        }
                        continue;
                    }
                    b => return self.err(format!("{b:#x} is invalid escaped character")),
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
                b if b < 0x20 => {
                    return self.err(format!("Unexpected control character {b:#x} in string"));
                }
                b => b,
            };

            s.push(b);
        }
    }

    fn consume_constant(&mut self, s: &'static str) -> Result<(), JsonParseError> {
        for expected_byte in s.as_bytes() {
            let b = self.consume_byte()?;
            if b != *expected_byte {
                return Err(JsonParseError::new(
                    format!("Unexpected byte {b:#x} while parsing '{s}'",),
                    self.location,
                ));
            }
        }
        Ok(())
    }

    fn consume_null(&mut self) -> JsonParseResult<JsonToken> {
        self.consume_constant("null")?;
        Ok(JsonToken::Null)
    }

    fn consume_true(&mut self) -> JsonParseResult<JsonToken> {
        self.consume_constant("true")?;
        Ok(JsonToken::True)
    }

    fn consume_false(&mut self) -> JsonParseResult<JsonToken> {
        self.consume_constant("false")?;
        Ok(JsonToken::False)
    }

    fn consume_number(&mut self) -> JsonParseResult<JsonToken> {
        let neg = *self.bytes.peek().unwrap() == b'-';
        if neg {
            self.consume_byte().unwrap();
        }

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
            self.consume_byte().unwrap();
        }

        if s.is_empty() {
            return self.err("Integer part must not be empty in number literal".to_string());
        }

        if s.starts_with(b"0") && s.len() > 1 {
            return self
                .err("Integer part of number must not start with 0 except for '0'".to_string());
        }

        if saw_dot {
            s.push(self.consume_byte().unwrap()); // eat '.'
            while let Some(d) = self.bytes.peek() {
                match d {
                    b'0'..=b'9' => s.push(*d),
                    b'e' | b'E' => {
                        saw_exp = true;
                        break;
                    }
                    _ => break,
                }
                self.consume_byte().unwrap();
            }
            if s.ends_with(b".") {
                return self.err("Fraction part of number must not be empty".to_string());
            }
        }

        if saw_exp {
            s.push(self.consume_byte().unwrap()); // eat 'e' or 'E'
            if let Some(b'+') | Some(b'-') = self.bytes.peek() {
                s.push(self.consume_byte().unwrap());
            }

            let mut saw_digit = false;
            while let Some(d) = self.bytes.peek() {
                match d {
                    b'0'..=b'9' => s.push(*d),
                    _ => break,
                }
                saw_digit = true;
                self.consume_byte().unwrap();
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

    /// Parses a token and returns it, or an error.
    pub fn next_token(&mut self) -> JsonParseResult<JsonToken> {
        let b = self
            .peek_byte_skip_whitespace()
            .ok_or_else(|| self.eof_err())?;
        let token = match b {
            b'[' => JsonToken::ArrayOpen,
            b']' => JsonToken::ArrayClose,
            b'{' => JsonToken::ObjOpen,
            b'}' => JsonToken::ObjClose,
            b':' => JsonToken::Colon,
            b',' => JsonToken::Comma,
            b'0'..=b'9' | b'-' => return self.consume_number(),
            b'"' => return self.consume_string(),
            b't' => return self.consume_true(),
            b'f' => return self.consume_false(),
            b'n' => return self.consume_null(),
            c => return self.err(format!("Invalid byte: {c:#x}")),
        };
        self.consume_byte()?;
        Ok(token)
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
