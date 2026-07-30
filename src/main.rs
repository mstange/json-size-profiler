use std::{
    fs::File,
    io::{BufWriter, Read},
    path::PathBuf,
};

use json_session::{JsonFragment, JsonSession};

mod jslb;
mod state;

use state::State;

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
    let path: PathBuf = std::env::args_os()
        .nth(1)
        .expect("Usage: cmd <FILENAME>")
        .into();
    let file = File::open(&path).unwrap();

    #[cfg(unix)]
    let size = {
        use std::os::unix::fs::MetadataExt;
        file.metadata().unwrap().size()
    };

    #[cfg(windows)]
    let size = {
        use std::os::windows::fs::MetadataExt;
        file.metadata().unwrap().file_size()
    };

    // How often should we flush aggregated samples? Depends on the size.
    // If I want to limit to 100 samples, and I have 1000 bytes to parse,
    // then, on average, I want just 1 sample per 10 bytes.
    // Let's say I've just passed byte 100, and I have 9 stacks in my
    // aggregation map. Flush them out.
    // Now I've passed byte 200, and my aggregation map contains 15 entries.
    // Flushing them out now would increase the total sample count to 9 + 15 = 24.
    // 24 is more than 20, so I don't flush and keep aggregating.
    // Now I've passed byte 300. Luckily a lot of the stuff between byte 200 and
    // byte 300 was hitting stacks that were already in the map, and the map has
    // only grown by 2 more entries. It now contains 17 entries.
    // Flushing now would increase the total sample count to 9 + 17 = 26.
    // That's below 30, so I can flush.
    // If the aggregation map grows faster than the rate I was hoping for, that's
    // too bad and I will end up emitting more than the MAX_SAMPLE_COUNT target.
    // At the end of the input I definitely need to flush.
    //
    // Ok so at what points should I check whether I can flush? Probably at every
    // update.
    const MAX_SAMPLE_COUNT: u64 = 100_000;

    let bytes_per_sample = (size / MAX_SAMPLE_COUNT).clamp(1, 1_000_000);

    // Peek at the file's first 8 bytes to detect JSLB magic without loading
    // the whole file (JSLB files can be several hundred MB).
    let mut magic = [0u8; 8];
    let file_for_magic = File::open(&path).unwrap();
    let read = (&file_for_magic)
        .take(8)
        .read(&mut magic)
        .unwrap_or_default();

    let profile = if read == 8 && jslb::is_jslb(&magic) {
        let bytes = std::fs::read(&path).unwrap();
        jslb::process(&bytes, "JSLB", bytes_per_sample)
    } else {
        let bytes = IoReadIterator::new(file);
        let mut session = JsonSession::new(bytes);
        let mut state = State::new("JSON", bytes_per_sample, "json");

        while let Some(event) = session.next().unwrap() {
            match event.fragment {
                JsonFragment::BeginObject => state.begin_object(event.span.start.byte_offset),
                JsonFragment::ObjectProperty(property_key) => {
                    state.object_property(event.span.start.byte_offset, property_key)
                }
                JsonFragment::EndObject => state.end_object(event.span.end.byte_offset),
                JsonFragment::BeginArray => state.begin_array(event.span.start.byte_offset),
                JsonFragment::EndArray => state.end_array(event.span.end.byte_offset),
                JsonFragment::PrimitiveValue(value) => state.primitive_value(
                    event.span.start.byte_offset,
                    event.span.end.byte_offset,
                    value,
                ),
            }
        }

        state.finish()
    };

    let filename = path.file_name().unwrap().to_string_lossy();
    let out_path = path.with_file_name(format!("{}-size-profile.json", filename));
    let out_file = File::create(&out_path).unwrap();
    let writer = BufWriter::new(out_file);
    serde_json::to_writer(writer, &profile).unwrap();
    eprintln!("JSON size profile saved to {out_path:?}");
    eprintln!("samply load {out_path:?}");
}
