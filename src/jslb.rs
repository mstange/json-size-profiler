use fxprof_processed_profile::{CategoryColor, Profile};
use json_session::{JsonFragment, JsonPrimitiveValue, JsonSession};
use json_slabs::{SLAB_REF_KEY, SlabDirectory, SlabType};

use crate::state::State;

/// True when `bytes` starts with the JSLB magic.
pub fn is_jslb(bytes: &[u8]) -> bool {
    bytes.starts_with(&json_slabs::MAGIC)
}

pub fn process(bytes: &[u8], name: &str, bytes_per_sample: u64) -> Profile {
    let dir = SlabDirectory::parse(bytes).expect("failed to parse JSLB header or slab table");
    dir.validate_extents(bytes.len() as u64)
        .expect("JSLB slab data overruns buffer");
    assert_eq!(
        dir.root_entry().slab_type,
        SlabType::Json,
        "JSLB root slab is not TYPE_JSON",
    );

    let paths = resolve_slab_paths(bytes, &dir);

    let mut state = State::new(name, bytes_per_sample, "");

    let overhead_stack = state.top_level_stack("Container overhead", CategoryColor::LightRed);
    state.set_root_scope("Container overhead", Some(overhead_stack));

    let mut slab_order: Vec<usize> = (0..dir.entries.len()).collect();
    slab_order.sort_by_key(|&i| dir.entries[i].start_offset);

    for slab_idx in slab_order {
        let entry = &dir.entries[slab_idx];
        let start = entry.start_offset;
        let end = start + entry.byte_length;

        state.advance(start);

        let path_display = paths[slab_idx].as_deref().unwrap_or("(unreferenced)");
        let label = format!(
            "Slab {slab_idx} ({}): {path_display}",
            entry.slab_type.name()
        );
        let color = color_for_slab_type(entry.slab_type);
        let slab_stack = state.top_level_stack(&label, color);
        // The slab's top-level frame carries the "Slab N (type): " prefix; the
        // JSON subtree inside uses the placement path alone, so its frames look
        // like a plain JSON tree rooted at that path.
        state.set_root_scope(path_display, Some(slab_stack));

        if entry.slab_type == SlabType::Json {
            let data = &bytes[start as usize..end as usize];
            let mut session = JsonSession::new(data.iter().cloned());
            while let Some(event) = session.next().unwrap() {
                let s = start + event.span.start.byte_offset;
                let e = start + event.span.end.byte_offset;
                match event.fragment {
                    JsonFragment::BeginObject => state.begin_object(s),
                    JsonFragment::ObjectProperty(k) => state.object_property(s, k),
                    JsonFragment::EndObject => state.end_object(e),
                    JsonFragment::BeginArray => state.begin_array(s),
                    JsonFragment::EndArray => state.end_array(e),
                    JsonFragment::PrimitiveValue(v) => state.primitive_value(s, e, v),
                }
            }
            state.advance(end);
        } else {
            state.advance(end);
        }

        state.set_root_scope("Container overhead", Some(overhead_stack));
    }

    state.advance(bytes.len() as u64);

    state.finish()
}

fn color_for_slab_type(t: SlabType) -> CategoryColor {
    match t {
        SlabType::Json => CategoryColor::Gray,
        SlabType::Int8
        | SlabType::Uint8
        | SlabType::Int16
        | SlabType::Uint16
        | SlabType::Int32
        | SlabType::Uint32
        | SlabType::Int64
        | SlabType::Uint64 => CategoryColor::Purple,
        SlabType::Float32 | SlabType::Float64 => CategoryColor::Orange,
    }
}

/// Walk the root JSON skeleton (and any JSON sub-slabs) to find the first
/// dot-path where each slab is referenced. Mirrors the algorithm used by
/// the `jslb ls` example: BFS from the root, recording a path on first
/// visit, and recursing into JSON sub-slabs so nested placeholders get
/// paths rooted at their parent's placeholder location.
fn resolve_slab_paths(bytes: &[u8], dir: &SlabDirectory) -> Vec<Option<String>> {
    let mut paths: Vec<Option<String>> = vec![None; dir.entries.len()];
    let root = dir.root_json_index();
    paths[root] = Some("root".to_string());

    let mut queue: Vec<(usize, String)> = vec![(root, "root".to_string())];
    while let Some((idx, base)) = queue.pop() {
        let entry = &dir.entries[idx];
        if entry.slab_type != SlabType::Json {
            continue;
        }
        let data =
            &bytes[entry.start_offset as usize..(entry.start_offset + entry.byte_length) as usize];
        walk_skeleton(data, &base, |child_idx, path| {
            if child_idx < paths.len() && paths[child_idx].is_none() {
                paths[child_idx] = Some(path.to_string());
                if dir.entries[child_idx].slab_type == SlabType::Json {
                    queue.push((child_idx, path.to_string()));
                }
            }
        });
    }
    paths
}

/// Streaming walk of a JSON skeleton. Invokes `on_placeholder(slab_idx, path)`
/// for each `{"$s": <non-negative integer>}` object.
///
/// `cur` mirrors the path to the value position we're about to read (or, mid-
/// object, to the value under the current key). Each frame on `stack` records
/// how many bytes it pushed onto `cur`, so its `saved_len` can be truncated
/// off when the next sibling arrives or the container closes.
fn walk_skeleton<F: FnMut(usize, &str)>(json_bytes: &[u8], base_path: &str, mut on_placeholder: F) {
    let mut cur = base_path.to_string();
    let mut stack: Vec<Frame> = Vec::new();

    let mut session = JsonSession::new(json_bytes.iter().cloned());
    while let Some(event) = session.next().unwrap() {
        // Before handling most events, if we're inside an array and this event
        // is the start of a new element, push its `[N]` component. Object
        // properties handle their own path component in the ObjectProperty
        // arm below.
        let starts_element = matches!(
            event.fragment,
            JsonFragment::BeginObject | JsonFragment::BeginArray | JsonFragment::PrimitiveValue(_)
        );
        if starts_element {
            if let Some(Frame::Array {
                index, saved_len, ..
            }) = stack.last_mut()
            {
                if let Some(len) = *saved_len {
                    cur.truncate(cur.len() - len);
                }
                let component = format!("[{index}]");
                let len = component.len();
                cur.push_str(&component);
                *saved_len = Some(len);
                *index += 1;
            }
        }

        match event.fragment {
            JsonFragment::BeginObject => stack.push(Frame::new_object()),
            JsonFragment::EndObject => {
                let Frame::Object {
                    num_props,
                    first_key_dollar_s,
                    first_val_int,
                    saved_len,
                } = stack.pop().expect("balanced BeginObject/EndObject")
                else {
                    unreachable!("EndObject with Array on top of stack");
                };
                if let Some(len) = saved_len {
                    cur.truncate(cur.len() - len);
                }
                if num_props == 1 && first_key_dollar_s {
                    if let Some(child_idx) = first_val_int {
                        on_placeholder(child_idx, &cur);
                    }
                }
            }
            JsonFragment::ObjectProperty(key) => {
                let Some(Frame::Object {
                    num_props,
                    first_key_dollar_s,
                    saved_len,
                    ..
                }) = stack.last_mut()
                else {
                    unreachable!("ObjectProperty outside object");
                };
                if let Some(len) = *saved_len {
                    cur.truncate(cur.len() - len);
                }
                if *num_props == 0 && key == SLAB_REF_KEY {
                    *first_key_dollar_s = true;
                }
                *num_props += 1;
                let component = format!(".{key}");
                let len = component.len();
                cur.push_str(&component);
                *saved_len = Some(len);
            }
            JsonFragment::BeginArray => stack.push(Frame::new_array()),
            JsonFragment::EndArray => {
                let Frame::Array { saved_len, .. } =
                    stack.pop().expect("balanced BeginArray/EndArray")
                else {
                    unreachable!("EndArray with Object on top of stack");
                };
                if let Some(len) = saved_len {
                    cur.truncate(cur.len() - len);
                }
            }
            JsonFragment::PrimitiveValue(v) => {
                if let Some(Frame::Object {
                    num_props,
                    first_key_dollar_s,
                    first_val_int,
                    ..
                }) = stack.last_mut()
                {
                    if *num_props == 1 && *first_key_dollar_s && first_val_int.is_none() {
                        if let JsonPrimitiveValue::Number(n) = v {
                            if n.is_finite() && n >= 0.0 && n.fract() == 0.0 {
                                *first_val_int = Some(n as usize);
                            }
                        }
                    }
                }
            }
        }
    }
}

enum Frame {
    Object {
        num_props: usize,
        first_key_dollar_s: bool,
        first_val_int: Option<usize>,
        /// Bytes appended to `cur` by the most recent object key push. Popped
        /// on the next key or on EndObject.
        saved_len: Option<usize>,
    },
    Array {
        index: usize,
        /// Bytes appended to `cur` by the current array element's `[N]`
        /// component. Popped on the next element or on EndArray.
        saved_len: Option<usize>,
    },
}

impl Frame {
    fn new_object() -> Self {
        Self::Object {
            num_props: 0,
            first_key_dollar_s: false,
            first_val_int: None,
            saved_len: None,
        }
    }
    fn new_array() -> Self {
        Self::Array {
            index: 0,
            saved_len: None,
        }
    }
}
