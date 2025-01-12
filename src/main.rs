use std::{
    fs::File,
    io::{BufWriter, Read},
    os::unix::fs::MetadataExt,
    path::PathBuf,
    time::SystemTime,
};

use fxprof_processed_profile::{
    CategoryColor, CategoryHandle, CpuDelta, Frame, FrameFlags, FrameInfo, Profile,
    ReferenceTimestamp, SamplingInterval, StackHandle, StringHandle, ThreadHandle, Timestamp,
};
use indexmap::IndexMap;
use tinyjson_session::{JsonPrimitiveValue, JsonSession, JsonSessionEvent};

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
    let size = file.metadata().unwrap().size();

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

    // let file =
    //     &br#"{"hello": 5, "what": null, "yo": [], "aha": ["yeah", 43, { "false": false } ]}"#[..];
    let bytes = IoReadIterator::new(file);
    let mut session = JsonSession::new(bytes);
    let mut state = State::new("JSON", bytes_per_sample);

    while let Some(event) = session.next().unwrap() {
        match event {
            JsonSessionEvent::BeginObject { pos_at_obj_start } => {
                state.begin_object(pos_at_obj_start)
            }
            JsonSessionEvent::ObjectProperty {
                property_key,
                pos_at_prop_key_start,
            } => state.object_property(pos_at_prop_key_start, property_key),
            JsonSessionEvent::EndObject { pos_after_obj_end } => {
                state.end_object(pos_after_obj_end)
            }
            JsonSessionEvent::BeginArray { pos_at_array_start } => {
                state.begin_array(pos_at_array_start)
            }
            JsonSessionEvent::EndArray {
                pos_after_array_end,
            } => state.end_array(pos_after_array_end),
            JsonSessionEvent::PrimitiveValue {
                value,
                pos_before,
                pos_after,
            } => state.primitive_value(pos_before, pos_after, value),
        }
    }

    let profile = state.finish();

    let filename = path.file_name().unwrap().to_string_lossy();
    let out_path = path.with_file_name(format!("{}-size-profile.json", &filename));
    let out_file = File::create(&out_path).unwrap();
    let writer = BufWriter::new(out_file);
    serde_json::to_writer(writer, &profile).unwrap();
    eprintln!("JSON size profile saved to {out_path:?}");
    eprintln!("samply load {out_path:?}");

    // thread_builder.add_sample(timestamp, frames, cpu_delta);
}

struct State {
    profile: Profile,
    thread: ThreadHandle,
    stack: Vec<StackHandle>,
    stack_labels: Vec<(StringHandle, CategoryHandle)>,
    top_label: StringHandle,
    top_category: CategoryHandle,
    string_stack: Vec<String>,
    scope_stack: Vec<Scope>,
    cat_obj: CategoryHandle,
    cat_arr: CategoryHandle,
    cat_null: CategoryHandle,
    cat_bool: CategoryHandle,
    cat_number: CategoryHandle,
    cat_str: CategoryHandle,
    last_pos: u64,
    /// How many bytes, roughly, we should have consumed for each
    /// emitted sample.
    bytes_per_sample: u64,
    /// Number of samples emitted so far.
    sample_count: u64,
    /// Stores the accumulated bytes per stack.
    aggregation_map: IndexMap<StackHandle, u64>,
    aggregation_start_pos: u64,
    array_depth: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scope {
    Object,
    Array,
}

impl State {
    pub fn new(name: &str, bytes_per_sample: u64) -> Self {
        let mut profile = Profile::new(
            name,
            ReferenceTimestamp::from_system_time(SystemTime::now()),
            SamplingInterval::from_nanos(1000),
        );
        let process = profile.add_process("Bytes", 0, Timestamp::from_nanos_since_reference(0));
        let thread = profile.add_thread(process, 0, Timestamp::from_nanos_since_reference(0), true);
        let cat_obj = profile.add_category("Object", CategoryColor::Gray);
        let cat_arr = profile.add_category("Array", CategoryColor::Gray);
        let cat_null = profile.add_category("Null", CategoryColor::Yellow);
        let cat_bool = profile.add_category("Bool", CategoryColor::Blue);
        let cat_number = profile.add_category("Number", CategoryColor::LightBlue);
        let cat_str = profile.add_category("String", CategoryColor::Green);
        let top_label = profile.intern_string("json");
        let top_category = cat_obj;
        Self {
            profile,
            thread,
            stack: Vec::new(),
            stack_labels: Vec::new(),
            top_label,
            top_category,
            string_stack: vec!["json".into()],
            scope_stack: Vec::new(),
            cat_obj,
            cat_arr,
            cat_null,
            cat_bool,
            cat_number,
            cat_str,
            last_pos: 0,
            bytes_per_sample,
            sample_count: 0,
            aggregation_map: IndexMap::new(),
            aggregation_start_pos: 0,
            array_depth: 0,
        }
    }

    fn advance(&mut self, pos: u64) {
        if pos == 0 && self.last_pos == 0 {
            return;
        }
        assert!(pos > self.last_pos);
        let delta = pos - self.last_pos;
        let top_frame = self.profile.intern_frame(
            self.thread,
            FrameInfo {
                frame: Frame::Label(self.top_label),
                category_pair: self.top_category.into(),
                flags: FrameFlags::empty(),
            },
        );
        let parent_stack = self.stack.last().cloned();
        let stack_handle = self
            .profile
            .intern_stack(self.thread, parent_stack, top_frame);
        *self.aggregation_map.entry(stack_handle).or_insert(0) += delta;
        self.last_pos = pos;
        self.maybe_flush();
    }

    fn should_flush(&self) -> bool {
        let aggregated_stack_count = self.aggregation_map.len() as u64;
        if aggregated_stack_count <= 1 {
            return false;
        }
        let sample_count_if_we_were_to_flush_now = self.sample_count + aggregated_stack_count;
        let allowed_sample_count_at_current_pos = self.last_pos / self.bytes_per_sample;
        sample_count_if_we_were_to_flush_now <= allowed_sample_count_at_current_pos
    }

    fn maybe_flush(&mut self) {
        if self.should_flush() {
            self.flush();
        }
    }

    fn flush(&mut self) {
        let mut synth_last_pos = self.aggregation_start_pos;
        let mut synth_last_timestamp = Timestamp::from_nanos_since_reference(synth_last_pos * 1000);
        for (&stack_handle, &acc_delta) in &self.aggregation_map {
            let synth_pos = synth_last_pos + acc_delta;
            self.profile.add_sample(
                self.thread,
                synth_last_timestamp,
                Some(stack_handle),
                CpuDelta::ZERO,
                0,
            );
            let cpu_delta = CpuDelta::from_micros(acc_delta);
            let weight = acc_delta as i32;
            let synth_timestamp = Timestamp::from_nanos_since_reference(synth_pos * 1000);
            self.profile.add_sample(
                self.thread,
                synth_timestamp,
                Some(stack_handle),
                cpu_delta,
                weight,
            );
            synth_last_pos = synth_pos;
            synth_last_timestamp = synth_timestamp;
            self.sample_count += 1; // we count these two samples as one
        }
        assert_eq!(self.last_pos, synth_last_pos);
        self.aggregation_start_pos = self.last_pos;
        self.aggregation_map.clear();
    }

    fn push_scope(&mut self, scope: Scope) {
        self.scope_stack.push(scope);
        self.top_category = match scope {
            Scope::Object => self.cat_obj,
            Scope::Array => self.cat_arr,
        };
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
        if let Some(parent_scope) = self.scope_stack.last().cloned() {
            self.top_category = match parent_scope {
                Scope::Object => self.cat_obj,
                Scope::Array => self.cat_arr,
            };
        }
    }

    fn push_label(&mut self, label: StringHandle, category: CategoryHandle) {
        let top_frame = FrameInfo {
            frame: Frame::Label(self.top_label),
            category_pair: self.top_category.into(),
            flags: FrameFlags::empty(),
        };
        let top_frame_handle = self.profile.intern_frame(self.thread, top_frame);
        let new_stack_handle =
            self.profile
                .intern_stack(self.thread, self.stack.last().cloned(), top_frame_handle);
        self.stack.push(new_stack_handle);
        self.stack_labels.push((self.top_label, self.top_category));
        self.top_label = label;
        self.top_category = category;
    }

    fn pop_label(&mut self) {
        self.stack.pop().unwrap();
        let (label, category) = self.stack_labels.pop().unwrap();
        self.top_label = label;
        self.top_category = category;
    }

    fn maybe_end_property(&mut self) {
        if self.scope_stack.last() == Some(&Scope::Object) {
            // Pop the current property from the stack.
            self.pop_label();
            self.string_stack.pop();
        }
    }

    pub fn finish(mut self) -> Profile {
        self.flush();
        self.profile
    }

    fn begin_object(&mut self, pos_at_obj_start: u64) {
        self.advance(pos_at_obj_start);
        self.push_scope(Scope::Object);
    }

    fn object_property(&mut self, pos_at_prop_key_start: u64, property_key: String) {
        self.advance(pos_at_prop_key_start);

        let parent_string = self.string_stack.last().unwrap();
        let concat_string = format!("{parent_string}.{property_key}");
        let label = self.profile.intern_string(&concat_string);
        self.string_stack.push(concat_string);
        self.push_label(label, self.cat_obj);
    }

    fn end_object(&mut self, pos_after_obj_end: u64) {
        self.advance(pos_after_obj_end);
        self.pop_scope();
        self.maybe_end_property();
    }

    fn begin_array(&mut self, pos_at_array_start: u64) {
        self.advance(pos_at_array_start);

        self.push_scope(Scope::Array);

        const INDEXER_CHARS: &str = "ijklmnopqrstuvwxyz";
        let indexer = &INDEXER_CHARS[(self.array_depth % INDEXER_CHARS.len())..][..1];
        let parent_string = self.string_stack.last().unwrap();
        let concat_string = format!("{parent_string}[{indexer}]");
        self.string_stack.push(concat_string);
        self.array_depth += 1;
    }

    fn end_array(&mut self, pos_after_array_end: u64) {
        self.advance(pos_after_array_end);
        self.string_stack.pop();
        self.array_depth -= 1;
        self.pop_scope();
        self.maybe_end_property();
    }

    fn primitive_value(&mut self, pos_before: u64, pos_after: u64, value: JsonPrimitiveValue) {
        self.advance(pos_before);

        let category = match value {
            JsonPrimitiveValue::Number(_) => self.cat_number,
            JsonPrimitiveValue::Boolean(_) => self.cat_bool,
            JsonPrimitiveValue::String(_) => self.cat_str,
            JsonPrimitiveValue::Null => self.cat_null,
        };
        let prev_category = std::mem::replace(&mut self.top_category, category);
        self.top_category = category;
        self.advance(pos_after);
        self.top_category = prev_category;

        self.maybe_end_property();
    }
}
