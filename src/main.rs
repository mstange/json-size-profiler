use std::{
    collections::HashMap,
    fs::File,
    io::{BufWriter, Read},
    time::SystemTime,
};

use fxprof_processed_profile::{
    CategoryColor, CategoryHandle, CpuDelta, Frame, FrameFlags, FrameHandle, FrameInfo, Profile,
    ReferenceTimestamp, SamplingInterval, StackHandle, ThreadHandle, Timestamp,
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
    let file = File::open("/Users/mstange/Downloads/big-profile.json").unwrap();
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
    scope_stack: Vec<Scope>,
    object_category: CategoryHandle,
    f_obj: FrameHandle,
    f_arr: FrameHandle,
    f_null: FrameHandle,
    f_bool: FrameHandle,
    f_number: FrameHandle,
    f_str: FrameHandle,
    last_pos: u64,
    aggregate_depth: usize,
    aggregation_map: Option<(u64, HashMap<StackHandle, u64>)>,
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
        let object_category = profile.add_category("Object", CategoryColor::DarkGray);
        let f = FrameInfo {
            frame: Frame::Label(profile.intern_string("<object>")),
            category_pair: object_category.into(),
            flags: FrameFlags::empty(),
        };
        let f_obj = profile.intern_frame(thread, f);
        let f = FrameInfo {
            frame: Frame::Label(profile.intern_string("<array>")),
            category_pair: profile.add_category("Array", CategoryColor::Gray).into(),
            flags: FrameFlags::empty(),
        };
        let f_arr = profile.intern_frame(thread, f);
        let f = FrameInfo {
            frame: Frame::Label(profile.intern_string("<null>")),
            category_pair: profile.add_category("Null", CategoryColor::Yellow).into(),
            flags: FrameFlags::empty(),
        };
        let f_null = profile.intern_frame(thread, f);
        let f = FrameInfo {
            frame: Frame::Label(profile.intern_string("<bool>")),
            category_pair: profile.add_category("Bool", CategoryColor::Blue).into(),
            flags: FrameFlags::empty(),
        };
        let f_bool = profile.intern_frame(thread, f);
        let f = FrameInfo {
            frame: Frame::Label(profile.intern_string("<number>")),
            category_pair: profile.add_category("Number", CategoryColor::Red).into(),
            flags: FrameFlags::empty(),
        };
        let f_number = profile.intern_frame(thread, f);
        let f = FrameInfo {
            frame: Frame::Label(profile.intern_string("<string>")),
            category_pair: profile.add_category("String", CategoryColor::Green).into(),
            flags: FrameFlags::empty(),
        };
        let f_str = profile.intern_frame(thread, f);
        let root_frame = FrameInfo {
            frame: Frame::Label(profile.intern_string("json")),
            category_pair: object_category.into(),
            flags: FrameFlags::empty(),
        };
        let f_root = profile.intern_frame(thread, root_frame);
        let s_root = profile.intern_stack(thread, None, f_root);
        Self {
            profile,
            thread,
            stack: vec![s_root],
            scope_stack: Vec::new(),
            object_category,
            f_obj,
            f_arr,
            f_null,
            f_bool,
            f_number,
            f_str,
            last_pos: 0,
            aggregate_depth,
            aggregation_map: None,
        }
    }

    fn advance(&mut self, pos: u64) {
        if pos <= self.last_pos {
            return;
        }
        let delta = pos - self.last_pos;
        let stack_handle = self.stack.last().cloned();
        if let Some((_, map)) = &mut self.aggregation_map {
            *map.entry(stack_handle.unwrap()).or_insert(0) += delta;
        } else {
            let start_timestamp = Timestamp::from_nanos_since_reference(self.last_pos * 1000);
            let end_timestamp = Timestamp::from_nanos_since_reference(pos * 1000);
            self.profile.add_sample(
                self.thread,
                start_timestamp,
                stack_handle,
                CpuDelta::ZERO,
                0,
            );
            let cpu_delta = CpuDelta::from_micros(delta);
            let weight = delta as i32;
            self.profile
                .add_sample(self.thread, end_timestamp, stack_handle, cpu_delta, weight);
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

    pub fn finish(self) -> Profile {
        self.profile
    }
}

impl JsonSessionObserver for State {
    fn begin_object(&mut self, pos_at_obj_start: u64) -> Result<(), String> {
        self.advance(pos_at_obj_start);
        let new_stack_handle =
            self.profile
                .intern_stack(self.thread, self.stack.last().cloned(), self.f_obj);
        self.stack.push(new_stack_handle);
        self.scope_stack.push(Scope::Object);
        if self.scope_stack.len() == self.aggregate_depth {
            self.aggregation_map = Some((self.last_pos, HashMap::new()));
        }
        Ok(())
    }

    fn object_property(&mut self, pos: u64, property_name: String) -> Result<(), String> {
        self.advance(pos);
        let s = self.profile.intern_string(&property_name);
        let frame = self.profile.intern_frame(
            self.thread,
            FrameInfo {
                frame: Frame::Label(s),
                category_pair: self.object_category.into(),
                flags: FrameFlags::empty(),
            },
        );
        let new_stack_handle =
            self.profile
                .intern_stack(self.thread, self.stack.last().cloned(), frame);
        self.stack.push(new_stack_handle);
        Ok(())
    }

    fn end_object(&mut self, pos_after_obj_end: u64) -> Result<(), String> {
        self.advance(pos_after_obj_end);
        self.stack.pop();
        if self.scope_stack.len() == self.aggregate_depth {
            self.consume_aggregation();
        }
        self.scope_stack.pop();
        if self.scope_stack.last() == Some(&Scope::Object) {
            self.stack.pop(); // pop property
        }
        Ok(())
    }

    fn begin_array(&mut self, pos_at_array_start: u64) -> Result<(), String> {
        self.advance(pos_at_array_start);
        let new_stack_handle =
            self.profile
                .intern_stack(self.thread, self.stack.last().cloned(), self.f_arr);
        self.stack.push(new_stack_handle);
        self.scope_stack.push(Scope::Array);
        if self.scope_stack.len() == self.aggregate_depth {
            self.aggregation_map = Some((self.last_pos, HashMap::new()));
        }
        Ok(())
    }

    fn end_array(&mut self, pos_after_array_end: u64) -> Result<(), String> {
        self.advance(pos_after_array_end);
        self.stack.pop();
        if self.scope_stack.len() == self.aggregate_depth {
            self.consume_aggregation();
        }
        self.scope_stack.pop();
        if self.scope_stack.last() == Some(&Scope::Object) {
            self.stack.pop(); // pop property
        }
        Ok(())
    }

    fn primitive_value(
        &mut self,
        pos_before: u64,
        pos_after: u64,
        value: JsonPrimitiveValue,
    ) -> Result<(), String> {
        self.advance(pos_before);
        let entry = match value {
            JsonPrimitiveValue::Number(_) => self.f_number,
            JsonPrimitiveValue::Boolean(_) => self.f_bool,
            JsonPrimitiveValue::String(_) => self.f_str,
            JsonPrimitiveValue::Null => self.f_null,
        };
        let new_stack_handle =
            self.profile
                .intern_stack(self.thread, self.stack.last().cloned(), entry);
        self.stack.push(new_stack_handle);
        self.advance(pos_after);
        self.stack.pop();
        if self.scope_stack.last() == Some(&Scope::Object) {
            self.stack.pop(); // pop property
        }
        Ok(())
    }
}
