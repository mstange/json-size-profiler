use std::{
    collections::HashMap,
    fs::File,
    io::{BufWriter, Read},
    time::SystemTime,
};

use fxprof_processed_profile::{
    CategoryColor, CategoryHandle, CpuDelta, Frame, FrameFlags, FrameInfo, Profile,
    ReferenceTimestamp, SamplingInterval, StackHandle, StringHandle, ThreadHandle, Timestamp,
};
use parser::{JsonPrimitiveValue, JsonSessionObserver};

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
    let filename = std::env::args_os().nth(1).expect("Usage: cmd <FILENAME>");
    let file = File::open(filename).unwrap();
    // let file =
    //     &br#"{"hello": 5, "what": null, "yo": [], "aha": ["yeah", 43, { "false": false } ]}"#[..];
    let bytes = IoReadIterator::new(file);
    let mut state = State::new("JSON", 4);
    let mut parser = parser::JsonSession::new(bytes, &mut state);
    parser.parse().unwrap();
    let profile = state.finish();
    let out_file = File::create("json-bytes-profile.json").unwrap();
    let writer = BufWriter::new(out_file);
    serde_json::to_writer(writer, &profile).unwrap();

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
    aggregate_depth: usize,
    aggregation_map: Option<(u64, HashMap<StackHandle, u64>)>,
    array_depth: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scope {
    Object,
    Array,
}

impl State {
    pub fn new(name: &str, aggregate_depth: usize) -> Self {
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
            aggregate_depth,
            aggregation_map: None,
            array_depth: 0,
        }
    }

    fn advance(&mut self, pos: u64) {
        if pos <= self.last_pos {
            return;
        }
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
        if let Some((_, map)) = &mut self.aggregation_map {
            *map.entry(stack_handle).or_insert(0) += delta;
        } else {
            let start_timestamp = Timestamp::from_nanos_since_reference(self.last_pos * 1000);
            let end_timestamp = Timestamp::from_nanos_since_reference(pos * 1000);
            self.profile.add_sample(
                self.thread,
                start_timestamp,
                Some(stack_handle),
                CpuDelta::ZERO,
                0,
            );
            let cpu_delta = CpuDelta::from_micros(delta);
            let weight = delta as i32;
            self.profile.add_sample(
                self.thread,
                end_timestamp,
                Some(stack_handle),
                cpu_delta,
                weight,
            );
        }
        self.last_pos = pos;
    }

    fn consume_aggregation(&mut self) {
        let (start_pos, map) = self.aggregation_map.take().unwrap();
        let mut synth_last_pos = start_pos;
        let mut synth_last_timestamp = Timestamp::from_nanos_since_reference(start_pos * 1000);
        for (stack_handle, acc_delta) in map {
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
        }
        assert_eq!(self.last_pos, synth_last_pos);
    }

    fn push_scope(&mut self, scope: Scope) {
        self.scope_stack.push(scope);
        if self.scope_stack.len() == self.aggregate_depth {
            self.aggregation_map = Some((self.last_pos, HashMap::new()));
        }
        self.top_category = match scope {
            Scope::Object => self.cat_obj,
            Scope::Array => self.cat_arr,
        };
    }

    fn pop_scope(&mut self) {
        if self.scope_stack.len() == self.aggregate_depth {
            self.consume_aggregation();
        }
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

    pub fn finish(self) -> Profile {
        self.profile
    }
}

impl JsonSessionObserver for State {
    fn begin_object(&mut self, pos_at_obj_start: u64) -> Result<(), String> {
        self.advance(pos_at_obj_start);
        self.push_scope(Scope::Object);
        Ok(())
    }

    fn object_property(&mut self, pos: u64, property_name: String) -> Result<(), String> {
        self.advance(pos);

        let parent_string = self.string_stack.last().unwrap();
        let concat_string = format!("{parent_string}.{property_name}");
        let label = self.profile.intern_string(&concat_string);
        self.string_stack.push(concat_string);
        self.push_label(label, self.cat_obj);

        Ok(())
    }

    fn end_object(&mut self, pos_after_obj_end: u64) -> Result<(), String> {
        self.advance(pos_after_obj_end);
        self.pop_scope();
        self.maybe_end_property();
        Ok(())
    }

    fn begin_array(&mut self, pos_at_array_start: u64) -> Result<(), String> {
        self.advance(pos_at_array_start);

        self.push_scope(Scope::Array);

        const INDEXER_CHARS: &str = "ijklmnopqrstuvwxyz";
        let indexer = &INDEXER_CHARS[(self.array_depth % INDEXER_CHARS.len())..][..1];
        let parent_string = self.string_stack.last().unwrap();
        let concat_string = format!("{parent_string}[{indexer}]");
        self.string_stack.push(concat_string);
        self.array_depth += 1;

        Ok(())
    }

    fn end_array(&mut self, pos_after_array_end: u64) -> Result<(), String> {
        self.advance(pos_after_array_end);
        self.string_stack.pop();
        self.array_depth -= 1;
        self.pop_scope();
        self.maybe_end_property();
        Ok(())
    }

    fn primitive_value(
        &mut self,
        pos_before: u64,
        pos_after: u64,
        value: JsonPrimitiveValue,
    ) -> Result<(), String> {
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

        Ok(())
    }
}
