use std::time::SystemTime;

use fxprof_processed_profile::{
    Category, CategoryColor, CategoryHandle, CpuDelta, FrameFlags, Profile, ReferenceTimestamp,
    SamplingInterval, StackHandle, ThreadHandle, TimelineUnit, Timestamp, WeightType,
};
use indexmap::IndexMap;
use json_session::JsonPrimitiveValue;
use rustc_hash::{FxBuildHasher, FxHashMap};
use string_interner::{DefaultStringInterner, DefaultSymbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JsonPiece {
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
        map.insert(piece, profile.handle_for_category(Category(name, color)));
    }
    map
}

enum Scope {
    Object {
        path: DefaultSymbol,
        stack_handle: StackHandle,
        path_for_current_prop_value: Option<DefaultSymbol>,
        array_depth: usize,
    },
    Array {
        stack_handle: StackHandle,
        path_for_array_elems: DefaultSymbol,
        array_depth: usize,
    },
}

struct ScopeInfo {
    stack_handle: Option<StackHandle>,
    path: DefaultSymbol,
    array_depth: usize,
}

/// The outer scope that surrounds any JSON walked so far.
///
/// For plain JSON files this is fixed after construction (path = "json",
/// no parent stack). For JSLB files it is swapped between slabs via
/// [`State::set_root_scope`].
#[derive(Clone, Copy)]
struct RootScope {
    path: DefaultSymbol,
    stack_handle: Option<StackHandle>,
}

pub struct State {
    pub profile: Profile,
    thread: ThreadHandle,
    root_scope: RootScope,
    scope_stack: Vec<Scope>,
    top_stack_handle: Option<StackHandle>,
    categories: FxHashMap<JsonPiece, CategoryHandle>,
    last_pos: u64,
    bytes_per_sample: u64,
    sample_count: u64,
    aggregation_map: IndexMap<StackHandle, u64, FxBuildHasher>,
    aggregation_start_pos: u64,
    string_interner: DefaultStringInterner,
    cached_property_paths: FxHashMap<(DefaultSymbol, DefaultSymbol), DefaultSymbol>,
    cached_indexer_paths: FxHashMap<(DefaultSymbol, usize), DefaultSymbol>,
    node_cache: FxHashMap<(Option<StackHandle>, DefaultSymbol, JsonPiece), StackHandle>,
}

impl State {
    pub fn new(name: &str, bytes_per_sample: u64, initial_root_path: &str) -> Self {
        let mut profile = Profile::new(
            name,
            ReferenceTimestamp::from_system_time(SystemTime::now()),
            SamplingInterval::from_nanos(1000),
        );
        let process = profile.add_process("Bytes", 0, Timestamp::from_nanos_since_reference(0));
        let thread = profile.add_thread(process, 0, Timestamp::from_nanos_since_reference(0), true);
        profile.set_thread_samples_weight_type(thread, WeightType::Bytes);
        profile.set_timeline_unit(TimelineUnit::Bytes);

        let categories = make_json_piece_categories(&mut profile);

        let mut string_interner = DefaultStringInterner::new();
        let root_path = string_interner.get_or_intern(initial_root_path);

        Self {
            profile,
            thread,
            root_scope: RootScope {
                path: root_path,
                stack_handle: None,
            },
            scope_stack: Vec::new(),
            categories,
            top_stack_handle: None,
            last_pos: 0,
            bytes_per_sample,
            sample_count: 0,
            aggregation_map: IndexMap::with_hasher(FxBuildHasher),
            aggregation_start_pos: 0,
            string_interner,
            cached_property_paths: FxHashMap::default(),
            cached_indexer_paths: FxHashMap::default(),
            node_cache: FxHashMap::default(),
        }
    }

    /// Handle for a top-level frame (used as the parent for a slab's inner
    /// stacks in JSLB mode). The frame belongs to the given color category.
    pub fn top_level_stack(&mut self, label: &str, color: CategoryColor) -> StackHandle {
        let category = self.profile.handle_for_category(Category("Slab", color));
        let label_handle = self.profile.handle_for_string(label);
        let frame_handle = self.profile.handle_for_frame_with_label(
            self.thread,
            label_handle,
            category,
            FrameFlags::empty(),
        );
        self.profile
            .handle_for_stack(self.thread, frame_handle, None)
    }

    /// Set the outer scope. May only be called when no JSON scopes are open.
    /// `stack_handle` becomes the parent of the next JSON frame emitted; `path`
    /// is the string used to build its label.
    pub fn set_root_scope(&mut self, path: &str, stack_handle: Option<StackHandle>) {
        assert!(self.scope_stack.is_empty());
        let path = self.string_interner.get_or_intern(path);
        self.root_scope = RootScope { path, stack_handle };
        self.top_stack_handle = stack_handle;
    }

    pub fn advance(&mut self, pos: u64) {
        if pos == self.last_pos {
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
        let mut synth_last_timestamp =
            Timestamp::from_millis_since_reference(synth_last_pos as f64);
        for (&stack_handle, &acc_delta) in &self.aggregation_map {
            let synth_pos = synth_last_pos + acc_delta;
            self.profile.add_sample(
                self.thread,
                synth_last_timestamp,
                Some(stack_handle),
                CpuDelta::ZERO,
                0,
            );
            let cpu_delta = CpuDelta::from_millis(acc_delta as f64);
            let weight = acc_delta as i32;
            let synth_timestamp = Timestamp::from_millis_since_reference(synth_pos as f64);
            self.profile.add_sample(
                self.thread,
                synth_timestamp,
                Some(stack_handle),
                cpu_delta,
                weight,
            );
            synth_last_pos = synth_pos;
            synth_last_timestamp = synth_timestamp;
            self.sample_count += 1;
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

        let label = self.profile.handle_for_string(&format!(
            "{} ({})",
            self.string_interner.resolve(parent_scope.path).unwrap(),
            piece.description()
        ));
        let category = self.categories[&piece];
        let frame_handle = self.profile.handle_for_frame_with_label(
            self.thread,
            label,
            category,
            FrameFlags::empty(),
        );
        let stack_handle =
            self.profile
                .handle_for_stack(self.thread, frame_handle, parent_scope.stack_handle);
        self.node_cache.insert(key, stack_handle);
        stack_handle
    }

    fn exit_scope(&mut self) {
        self.scope_stack.pop();

        self.top_stack_handle = match self.scope_stack.last() {
            Some(Scope::Object { stack_handle, .. }) => Some(*stack_handle),
            Some(Scope::Array { stack_handle, .. }) => Some(*stack_handle),
            None => self.root_scope.stack_handle,
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
                stack_handle: self.root_scope.stack_handle,
                path: self.root_scope.path,
                array_depth: 0,
            },
        }
    }

    pub fn begin_object(&mut self, pos_at_obj_start: u64) {
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

    pub fn object_property(&mut self, pos_at_prop_key_start: u64, property_key: String) {
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

    pub fn end_object(&mut self, pos_after_obj_end: u64) {
        self.advance(pos_after_obj_end);

        self.exit_scope();
    }

    pub fn begin_array(&mut self, pos_at_array_start: u64) {
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

    pub fn end_array(&mut self, pos_after_array_end: u64) {
        self.advance(pos_after_array_end);

        self.exit_scope();
    }

    pub fn primitive_value(&mut self, pos_before: u64, pos_after: u64, value: JsonPrimitiveValue) {
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
