use std::{
    fs::File,
    io::{BufWriter, Read},
    path::PathBuf,
    time::SystemTime,
};

use fxprof_processed_profile::{
    CategoryColor, CategoryHandle, CpuDelta, Frame, FrameFlags, FrameInfo, Profile,
    ReferenceTimestamp, SamplingInterval, StackHandle, ThreadHandle, Timestamp,
};
use indexmap::IndexMap;
use json_session::{JsonFragment, JsonPrimitiveValue, JsonSession};
use rustc_hash::{FxBuildHasher, FxHashMap};
use string_interner::{DefaultStringInterner, DefaultSymbol};

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

    // let file =
    //     &br#"{"hello": 5, "what": null, "yo": [], "aha": ["yeah", 43, { "false": false } ]}"#[..];
    let bytes = IoReadIterator::new(file);
    let mut session = JsonSession::new(bytes);
    let mut state = State::new("JSON", bytes_per_sample);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum JsonPiece {
    Object,
    Array,
    Null,
    Bool,
    Number,
    String,
    PropertyKey,
}

impl JsonPiece {
    pub fn description(&self) -> &'static str {
        match self {
            JsonPiece::Object => "object",
            JsonPiece::Array => "array",
            JsonPiece::Null => "null",
            JsonPiece::Bool => "bool",
            JsonPiece::Number => "number",
            JsonPiece::String => "string",
            JsonPiece::PropertyKey => "property key",
        }
    }
}

fn make_json_piece_categories(profile: &mut Profile) -> FxHashMap<JsonPiece, CategoryHandle> {
    use CategoryColor::*;
    const PIECES: [(JsonPiece, &str, CategoryColor); 7] = [
        (JsonPiece::Object, "Object", Gray),
        (JsonPiece::Array, "Array", Gray),
        (JsonPiece::Null, "Null", Yellow),
        (JsonPiece::Bool, "Bool", Brown),
        (JsonPiece::Number, "Number", Green),
        (JsonPiece::String, "String", Blue),
        (JsonPiece::PropertyKey, "Property Key", LightBlue),
    ];
    let mut map = FxHashMap::default();
    for (piece, name, color) in PIECES {
        map.insert(piece, profile.add_category(name, color));
    }
    map
}

enum Scope {
    Object {
        path: DefaultSymbol,                                // "json.counters[i].samples"
        stack_handle: StackHandle,                          // json.counters[i].samples (object)
        path_for_current_prop_value: Option<DefaultSymbol>, // "json.counters[i].samples.length"
        array_depth: usize,
    },
    Array {
        stack_handle: StackHandle,           // json.counters (array)
        path_for_array_elems: DefaultSymbol, // "json.counters[i]"
        array_depth: usize,
    },
}

struct ScopeInfo {
    stack_handle: Option<StackHandle>,
    path: DefaultSymbol,
    array_depth: usize,
}

struct State {
    profile: Profile,
    thread: ThreadHandle,
    root_path: DefaultSymbol,
    scope_stack: Vec<Scope>,
    top_stack_handle: Option<StackHandle>,
    categories: FxHashMap<JsonPiece, CategoryHandle>,
    last_pos: u64,
    /// How many bytes, roughly, we should have consumed for each
    /// emitted sample.
    bytes_per_sample: u64,
    /// Number of samples emitted so far.
    sample_count: u64,
    /// Stores the accumulated bytes per stack.
    aggregation_map: IndexMap<StackHandle, u64, FxBuildHasher>,
    aggregation_start_pos: u64,
    string_interner: DefaultStringInterner,
    cached_property_paths: FxHashMap<(DefaultSymbol, DefaultSymbol), DefaultSymbol>,
    cached_indexer_paths: FxHashMap<(DefaultSymbol, usize), DefaultSymbol>,
    node_cache: FxHashMap<(Option<StackHandle>, DefaultSymbol, JsonPiece), StackHandle>,
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

        let categories = make_json_piece_categories(&mut profile);

        let mut string_interner = DefaultStringInterner::new();
        let root_path = string_interner.get_or_intern("json");

        Self {
            profile,
            thread,
            root_path,
            scope_stack: Vec::new(),
            categories,
            top_stack_handle: None,
            last_pos: 0,
            bytes_per_sample,
            sample_count: 0,
            aggregation_map: IndexMap::with_hasher(rustc_hash::FxBuildHasher),
            aggregation_start_pos: 0,
            string_interner,
            cached_property_paths: FxHashMap::default(),
            cached_indexer_paths: FxHashMap::default(),
            node_cache: FxHashMap::default(),
        }
    }

    fn advance(&mut self, pos: u64) {
        if pos == 0 && self.last_pos == 0 {
            return;
        }
        assert!(pos > self.last_pos);
        let delta = pos - self.last_pos;
        let stack_handle = self.top_stack_handle.unwrap();
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

    pub fn finish(mut self) -> Profile {
        self.flush();
        self.profile
    }

    fn get_stack(&mut self, parent_scope: &ScopeInfo, piece: JsonPiece) -> StackHandle {
        let key = (parent_scope.stack_handle, parent_scope.path, piece);
        if let Some(s) = self.node_cache.get(&key) {
            return *s;
        }

        let label = self.profile.intern_string(&format!(
            "{} ({})",
            self.string_interner.resolve(parent_scope.path).unwrap(),
            piece.description()
        ));
        let category = self.categories[&piece];
        let frame_info = FrameInfo {
            frame: Frame::Label(label),
            category_pair: category.into(),
            flags: FrameFlags::empty(),
        };
        let frame_handle = self.profile.intern_frame(self.thread, frame_info);
        let stack_handle =
            self.profile
                .intern_stack(self.thread, parent_scope.stack_handle, frame_handle);
        self.node_cache.insert(key, stack_handle);
        stack_handle
    }

    fn exit_scope(&mut self) {
        self.scope_stack.pop();

        self.top_stack_handle = match self.scope_stack.last() {
            Some(Scope::Object { stack_handle, .. }) => Some(*stack_handle),
            Some(Scope::Array { stack_handle, .. }) => Some(*stack_handle),
            None => None,
        };
    }

    fn current_scope(&self) -> ScopeInfo {
        match self.scope_stack.last() {
            Some(Scope::Object {
                stack_handle,
                path_for_current_prop_value,
                array_depth,
                ..
            }) => ScopeInfo {
                stack_handle: Some(*stack_handle),
                path: path_for_current_prop_value.unwrap(),
                array_depth: *array_depth,
            },
            Some(Scope::Array {
                stack_handle,
                path_for_array_elems,
                array_depth,
                ..
            }) => ScopeInfo {
                stack_handle: Some(*stack_handle),
                path: *path_for_array_elems,
                array_depth: *array_depth,
            },
            None => ScopeInfo {
                stack_handle: None,
                path: self.root_path,
                array_depth: 0,
            },
        }
    }

    fn begin_object(&mut self, pos_at_obj_start: u64) {
        self.advance(pos_at_obj_start);

        let parent_scope = self.current_scope();
        let stack_handle = self.get_stack(&parent_scope, JsonPiece::Object);
        self.scope_stack.push(Scope::Object {
            stack_handle,
            path: parent_scope.path,
            path_for_current_prop_value: None,
            array_depth: parent_scope.array_depth,
        });
        self.top_stack_handle = Some(stack_handle);
    }

    fn object_property(&mut self, pos_at_prop_key_start: u64, property_key: String) {
        self.advance(pos_at_prop_key_start);

        let property_key = self.string_interner.get_or_intern(&property_key);
        let (obj_scope_path, path_for_current_prop_value) =
            match self.scope_stack.last_mut().unwrap() {
                Scope::Object {
                    path,
                    path_for_current_prop_value,
                    ..
                } => (*path, path_for_current_prop_value),
                _ => panic!(),
            };

        let cache_key = (obj_scope_path, property_key);
        let property_path = if let Some(s) = self.cached_property_paths.get(&cache_key) {
            *s
        } else {
            let property_path = format!(
                "{}.{}",
                self.string_interner.resolve(obj_scope_path).unwrap(),
                self.string_interner.resolve(property_key).unwrap()
            );
            let property_path = self.string_interner.get_or_intern(&property_path);
            self.cached_property_paths.insert(cache_key, property_path);
            property_path
        };

        *path_for_current_prop_value = Some(property_path);
        let obj_scope = self.current_scope();
        let stack_handle = self.get_stack(&obj_scope, JsonPiece::PropertyKey);
        self.top_stack_handle = Some(stack_handle);
    }

    fn end_object(&mut self, pos_after_obj_end: u64) {
        self.advance(pos_after_obj_end);

        self.exit_scope();
    }

    fn begin_array(&mut self, pos_at_array_start: u64) {
        self.advance(pos_at_array_start);

        let parent_scope = self.current_scope();
        let cache_key = (parent_scope.path, parent_scope.array_depth);
        let path_for_array_elems = if let Some(s) = self.cached_indexer_paths.get(&cache_key) {
            *s
        } else {
            const INDEXER_CHARS: &str = "ijklmnopqrstuvwxyz";
            let indexer = &INDEXER_CHARS[(parent_scope.array_depth % INDEXER_CHARS.len())..][..1];
            let path_for_array_elems = format!(
                "{}[{indexer}]",
                self.string_interner.resolve(parent_scope.path).unwrap()
            );
            let path_for_array_elems = self.string_interner.get_or_intern(&path_for_array_elems);
            self.cached_indexer_paths
                .insert(cache_key, path_for_array_elems);
            path_for_array_elems
        };

        let stack_handle = self.get_stack(&parent_scope, JsonPiece::Array);
        self.top_stack_handle = Some(stack_handle);
        self.scope_stack.push(Scope::Array {
            stack_handle,
            path_for_array_elems,
            array_depth: parent_scope.array_depth + 1,
        });
    }

    fn end_array(&mut self, pos_after_array_end: u64) {
        self.advance(pos_after_array_end);

        self.exit_scope();
    }

    fn primitive_value(&mut self, pos_before: u64, pos_after: u64, value: JsonPrimitiveValue) {
        self.advance(pos_before);

        let scope = self.current_scope();

        let piece = match value {
            JsonPrimitiveValue::Number(_) => JsonPiece::Number,
            JsonPrimitiveValue::Boolean(_) => JsonPiece::Bool,
            JsonPrimitiveValue::String(_) => JsonPiece::String,
            JsonPrimitiveValue::Null => JsonPiece::Null,
        };

        let stack_handle = self.get_stack(&scope, piece);
        self.top_stack_handle = Some(stack_handle);

        self.advance(pos_after);
        self.top_stack_handle = scope.stack_handle;
    }
}
