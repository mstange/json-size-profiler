json-session
========

Streaming parser for JSON. This crate provides an iterator which yields values of the following enum variants:
`BeginObject`, `ObjectProperty`, `EndObject`, `BeginArray`, `EndArray`, and `PrimitiveValue`.

This allows gathering statistics about the contents of large JSON documents without ever
holding the entire document in memory.

Every `JsonFragmentWithSpan` has location information attached to it, saying at which byte offset
(and at which line and column) the relevant fragment began and ended.

The code was originally based on [`tinyjson`](https://github.com/rhysd/tinyjson).

MIT licensed
