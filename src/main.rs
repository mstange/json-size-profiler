use std::{fs::File, io::Read, time::SystemTime};

use fxprof_processed_profile::{
    CategoryHandle, Frame, FrameFlags, FrameInfo, Profile, ReferenceTimestamp, SamplingInterval,
};

use crate::json_value::{JsonValue, Span};

mod json_value;
mod parser;

struct IoReadIterator<R> {
    reader: R,
    buf: Vec<u8>,
    valid_slice_start: usize,
    valid_slice_end: usize,
}

impl<R> IoReadIterator<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buf: vec![0; 4096],
            valid_slice_start: 0,
            valid_slice_end: 0,
        }
    }
}

impl<R: Read> Iterator for IoReadIterator<R> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.valid_slice_start != self.valid_slice_end {
            let b = self.buf[self.valid_slice_start];
            self.valid_slice_start += 1;
            Some(b)
        } else {
            let read_len = self.reader.read(&mut self.buf).ok()?;
            if read_len != 0 {
                self.valid_slice_start = 1;
                self.valid_slice_end = read_len;
                Some(self.buf[0])
            } else {
                None
            }
        }
    }
}

fn main() {
    // let file = File::open("/Users/mstange/Downloads/big-profile.json").unwrap();
    let file =
        &br#"{"hello": 5, "what": null, "yo": [], "aha": ["yeah", 43, { "false": false } ]}"#[..];
    let bytes = IoReadIterator::new(file);
    let parser = parser::JsonParser::new(bytes);
    let root_value = parser.parse().unwrap();
    dbg!(root_value);

    let profile = Profile::new(
        "JSON",
        ReferenceTimestamp::from_system_time(SystemTime::now()),
        SamplingInterval::from_hz(1000.0),
    );
    // thread_builder.add_sample(timestamp, frames, cpu_delta);
}
